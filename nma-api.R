# =============================================================================
# Network Meta-Analysis (NMA) Core Module API — completely separate Plumber
# process from api.R (Forest/Funnel/Sensitivity) and tsa-api.R (TSA), running
# on its own port so it can never interfere with those tools. Uses `netmeta`
# (Rücker et al.), the standard validated R package for frequentist network
# meta-analysis via the graph-theoretical / weighted-least-squares approach.
# =============================================================================
library(plumber)
library(jsonlite)
library(base64enc)

STARTED_AT <- Sys.time()

# netmeta pulls in a genuinely heavy dependency chain (igraph, Matrix, MASS,
# mvtnorm, dplyr, ggplot2, colorspace, grid, magrittr, magic, plus meta ->
# metabook) - loading all of that at top-level, before the server even
# starts listening, was found in production to make Render's free-tier
# instance (512MB RAM / 0.1 CPU) hang for 15+ minutes before its port-open
# health check gave up, even though the eventual load would likely have
# succeeded given enough time. Deferring the load to the first real request
# lets the server bind its port and pass health checks in under a second
# (the same as every other service in this project), at the cost of the
# first NMA request after a cold start taking longer while these packages
# load - a one-time, acceptable trade-off, and the SAME packages either
# way, not a reduced feature set.
.heavy_packages_loaded <- FALSE
ensure_heavy_packages_loaded <- function() {
  if (.heavy_packages_loaded) return(invisible())
  suppressPackageStartupMessages({
    library(netmeta)
    library(meta)
    library(ggplot2)
    library(scales)
  })
  .heavy_packages_loaded <<- TRUE
  invisible()
}

# Production CORS: restrict to the deployed frontend origin via the
# ALLOWED_ORIGIN env var (comma-separated for multiple origins). Defaults to
# "*" so local development (no env var set) keeps working exactly as before.
ALLOWED_ORIGINS <- strsplit(Sys.getenv("ALLOWED_ORIGIN", "*"), ",")[[1]]
resolve_cors_origin <- function(req) {
  if (length(ALLOWED_ORIGINS) == 1 && ALLOWED_ORIGINS[1] == "*") return("*")
  origin <- req$HTTP_ORIGIN
  if (!is.null(origin) && origin %in% trimws(ALLOWED_ORIGINS)) return(origin)
  trimws(ALLOWED_ORIGINS[1])
}

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", resolve_cors_origin(req))
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  res$setHeader("Vary", "Origin")
  if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 200; return(list()) }
  plumber::forward()
}

#* Lightweight liveness probe for the frontend's backend-status banner and
#* for an external supervisor/health-checker to detect a hung/dead process.
#* @serializer unboxedJSON
#* @get /api/nma/health
function(res) {
  list(status = "ok", package_version = as.character(utils::packageVersion("netmeta")),
       uptime_seconds = round(as.numeric(difftime(Sys.time(), STARTED_AT, units = "secs"))))
}

#* @plumber
function(pr) {
  pr %>% plumber::pr_set_error(function(req, res, err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "unknown error")
    log_line <- paste0("[", format(Sys.time()), "] NMA UNCAUGHT ERROR: ", msg,
                        " | route: ", req$PATH_INFO,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/nma-error.log", append = TRUE), silent = TRUE)
    res$status <- 200
    list(status = "error", message = paste("Uncaught server error:", msg))
  })
}

# --- Shared helpers ----------------------------------------------------------

safe_num <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  val[is.na(val) | is.infinite(val)] <- NA
  as.list(val)
}

# Build the arm-level data.frame from the frontend's rows, then reshape to
# contrast-level TE/seTE via netmeta's own `pairwise()` helper - this is the
# package's documented, standard entry point for arm-level data, not a
# hand-rolled effect-size calculation.
build_pairwise <- function(studies, outcome_type, sm) {
  if (outcome_type == "continuous") {
    df <- data.frame(study = as.character(studies$study), treat = as.character(studies$treatment),
                      mean = as.numeric(studies$mean), sd = as.numeric(studies$sd), n = as.numeric(studies$n),
                      stringsAsFactors = FALSE)
    pairwise(treat = treat, mean = mean, sd = sd, n = n, studlab = study, data = df, sm = sm)
  } else {
    df <- data.frame(study = as.character(studies$study), treat = as.character(studies$treatment),
                      event = as.numeric(studies$event), n = as.numeric(studies$n),
                      stringsAsFactors = FALSE)
    pairwise(treat = treat, event = event, n = n, studlab = study, data = df, sm = sm)
  }
}

# Attaches optional STUDY-level variables (subgroup, year) onto the
# contrast-level pairwise() output. These are properties of the study, not
# the arm, so a multi-arm study's repeated rows in `studies` are collapsed
# to one value per study via tapply() before being matched back onto pw's
# rows by studlab - never duplicated or invented for studies missing them.
attach_study_vars <- function(pw, studies) {
  if (!is.null(studies$subgroup) && any(!is.na(studies$subgroup) & studies$subgroup != "")) {
    su <- tapply(as.character(studies$subgroup), as.character(studies$study), function(x) { x <- x[!is.na(x) & x != ""]; if (length(x) == 0) NA_character_ else x[1] })
    pw$subgroup <- unname(su[as.character(pw$studlab)])
  } else {
    pw$subgroup <- NA_character_
  }
  if (!is.null(studies$year) && any(!is.na(suppressWarnings(as.numeric(studies$year))))) {
    ye <- tapply(suppressWarnings(as.numeric(studies$year)), as.character(studies$study), function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else x[1] })
    pw$year <- unname(ye[as.character(pw$studlab)])
  } else {
    pw$year <- NA_real_
  }
  pw
}

# Summarizes a fitted netmeta object into the same shape used across the
# Core/Diagnostics endpoints (ranking + league table), so the extensions
# below don't reimplement extraction logic and stay consistent with Core.
summarize_fit <- function(m, is_common, small_values) {
  rk <- tryCatch(netrank(m, common = is_common, random = !is_common, small.values = small_values), error = function(e) NULL)
  pscore_col <- if (!is.null(rk)) (if (is_common) rk$Pscore.fixed else rk$Pscore.random) else NULL
  ranking <- NULL
  if (!is.null(pscore_col)) {
    rdf <- data.frame(treatment = names(pscore_col), pscore = as.numeric(pscore_col), stringsAsFactors = FALSE)
    rdf <- rdf[order(-rdf$pscore), ]
    ranking <- lapply(seq_len(nrow(rdf)), function(i) list(treatment = rdf$treatment[i], rank = i, pscore = round(rdf$pscore[i], 4)))
  }
  lg <- tryCatch(netleague(m, common = is_common, random = !is_common, digits = 2, bracket = "(", separator = "; "), error = function(e) NULL)
  league <- NULL
  if (!is.null(lg)) {
    lg_mat <- if (is_common) lg$common else lg$random
    league <- list(rownames = as.list(rownames(lg_mat)), matrix = lapply(seq_len(nrow(lg_mat)), function(i) as.list(as.character(lg_mat[i, ]))))
  }
  list(
    k = m$k, n_treatments = length(m$trts), trts = as.character(m$trts),
    tau2 = tryCatch(round(as.numeric(m$tau2), 4), error = function(e) NA),
    i2 = tryCatch(round(as.numeric(m$I2), 4), error = function(e) NA),
    ranking = ranking, league = league
  )
}

validate_rows <- function(studies, outcome_type) {
  errors <- character(0)
  n <- nrow(studies)
  if (n == 0) return("No rows found in the uploaded data.")
  if (any(is.na(studies$study) | studies$study == "")) errors <- c(errors, "One or more rows are missing a study ID.")
  if (any(is.na(studies$treatment) | studies$treatment == "")) errors <- c(errors, "One or more rows are missing a treatment name.")
  if (outcome_type == "continuous") {
    if (any(is.na(as.numeric(studies$mean)))) errors <- c(errors, "One or more rows have a missing/invalid mean.")
    if (any(is.na(as.numeric(studies$sd)))) errors <- c(errors, "One or more rows have a missing/invalid SD.")
    if (any(!is.na(as.numeric(studies$sd)) & as.numeric(studies$sd) <= 0)) errors <- c(errors, "SD must be greater than 0 in all rows.")
    if (any(is.na(as.numeric(studies$n)) | as.numeric(studies$n) <= 0)) errors <- c(errors, "Total (N) must be a positive number in all rows.")
  } else {
    if (any(is.na(as.numeric(studies$event)))) errors <- c(errors, "One or more rows have a missing/invalid event count.")
    if (any(is.na(as.numeric(studies$n)) | as.numeric(studies$n) <= 0)) errors <- c(errors, "Total (N) must be a positive number in all rows.")
    ev <- suppressWarnings(as.numeric(studies$event)); tot <- suppressWarnings(as.numeric(studies$n))
    if (any(!is.na(ev) & ev < 0)) errors <- c(errors, "Events cannot be negative.")
    if (any(!is.na(ev) & !is.na(tot) & ev > tot)) errors <- c(errors, "Events cannot exceed the total in a row.")
  }
  # Trimmed for the purposes of duplicate-detection only (not case-folded -
  # a case difference may be a genuinely different treatment label, but
  # leading/trailing whitespace never is) so "A" and " A " in the same
  # study are correctly caught as the same arm entered twice, rather than
  # silently being treated as two different treatments.
  dup <- duplicated(paste(trimws(as.character(studies$study)), trimws(as.character(studies$treatment))))
  if (any(dup)) errors <- c(errors, paste0("Duplicate treatment arm(s) within the same study: ", paste(unique(paste0(studies$study[dup], " / ", studies$treatment[dup])), collapse = "; "), "."))
  arms_per_study <- table(studies$study)
  single_arm <- names(arms_per_study)[arms_per_study < 2]
  if (length(single_arm) > 0) errors <- c(errors, paste0("Stud(ies) with only one treatment arm (cannot contribute a comparison): ", paste(single_arm, collapse = ", "), "."))

  # --- NMA eligibility: distinct treatment count --------------------------
  # netmeta genuinely needs >=3 distinct treatments to be a "network" at
  # all; with exactly 2, several downstream helpers this app calls
  # (rankogram()/decomp.design()/league-table construction) build a
  # zero-width matrix internally and fail with an opaque
  # "dim(X) must have a positive length" error instead of a clean message.
  # Guarding here - inside the one row-validator every model-fitting
  # endpoint already calls before touching netmeta() - means the invalid
  # network is rejected before any of that code runs, for /validate,
  # /analyze, /diagnostics, /funnel, /sensitivity, /subgroup, and
  # /metaregression alike, without re-implementing or altering how NMA
  # itself is fit. Treatment labels are trimmed defensively here (not in
  # build_pairwise, so model-fitting label matching is unchanged) so this
  # guard holds even for a direct API call that skips the frontend's own
  # trimming.
  valid_treatments <- trimws(as.character(studies$treatment))
  valid_treatments <- valid_treatments[!is.na(valid_treatments) & valid_treatments != ""]
  n_distinct_treatments <- length(unique(valid_treatments))
  if (n_distinct_treatments == 0) {
    errors <- c(errors, "No valid treatment identifiers were detected. Please check the treatment columns in your dataset.")
  } else if (n_distinct_treatments == 1) {
    errors <- c(errors, "Network meta-analysis cannot be performed because only one distinct treatment was detected.")
  } else if (n_distinct_treatments == 2) {
    errors <- c(errors, "This dataset contains only two distinct treatments. Network meta-analysis requires at least three distinct treatments. Please use the Pairwise Meta-analysis tool for a two-treatment comparison.")
  }

  errors
}

# Multi-arm accounting, purely descriptive (no statistical calculation here -
# the actual multi-arm correlation handling happens inside netmeta() itself
# via pairwise()'s contrast-level reshaping, which correctly keeps a study's
# arms correlated rather than treating them as independent 2-arm trials).
arm_stats <- function(studies) {
  arms_per_study <- table(studies$study)
  list(
    n_treatment_arms = nrow(studies),
    n_multiarm_studies = as.integer(sum(arms_per_study >= 3)),
    max_arms = as.integer(max(arms_per_study))
  )
}

# --- Network validation (fast, no model fit) ---------------------------------

#* @serializer unboxedJSON
#* @parser json
#* @post /api/nma/validate
function(req, res) {
  ensure_heavy_packages_loaded()
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    studies <- as.data.frame(body$studies)
    outcome_type <- if (!is.null(body$outcome_type)) body$outcome_type else "dichotomous"

    row_errors <- validate_rows(studies, outcome_type)
    if (length(row_errors) > 0) {
      return(list(status = "error", message = paste(row_errors, collapse = " ")))
    }

    sm <- if (outcome_type == "continuous") "MD" else "OR"
    pw <- build_pairwise(studies, outcome_type, sm)
    nc <- netconnection(treat1, treat2, studlab, data = pw)

    treatments <- sort(unique(c(as.character(pw$treat1), as.character(pw$treat2))))
    n_participants <- if (outcome_type == "continuous") sum(as.numeric(studies$n)) else sum(as.numeric(studies$n))
    connected <- nc$n.subnets == 1

    components <- NULL
    if (!connected) {
      # nc$subnet is a per-ROW (per direct comparison) vector aligned with
      # nc$treat1/nc$treat2, not a named per-treatment vector - build the
      # treatment -> component map by walking both treatment columns.
      trt_component <- setNames(rep(NA_integer_, length(treatments)), treatments)
      for (i in seq_along(nc$subnet)) {
        trt_component[as.character(nc$treat1[i])] <- nc$subnet[i]
        trt_component[as.character(nc$treat2[i])] <- nc$subnet[i]
      }
      components <- lapply(sort(unique(na.omit(trt_component))), function(g) {
        list(component = as.integer(g), treatments = as.list(names(trt_component)[!is.na(trt_component) & trt_component == g]))
      })
    }

    arms <- arm_stats(studies)
    list(
      status = "success",
      n_studies = length(unique(studies$study)),
      n_treatments = length(treatments),
      n_treatment_arms = arms$n_treatment_arms,
      n_multiarm_studies = arms$n_multiarm_studies,
      max_arms = arms$max_arms,
      n_participants = n_participants,
      n_direct_comparisons = nrow(pw),
      treatments = as.list(treatments),
      connected = connected,
      n_components = as.integer(nc$n.subnets),
      components = components,
      message = if (connected) "Data validation passed." else "Network is disconnected. NMA requires a connected treatment network - see the listed components."
    )
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    try(cat(paste0("[", format(Sys.time()), "] NMA VALIDATE ERROR: ", msg, "\n"), file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/nma-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = msg)
  })
}

# --- Full NMA run: ONE fitted model feeds every downstream output ------------

#* @serializer unboxedJSON
#* @parser json
#* @post /api/nma/analyze
function(req, res) {
  ensure_heavy_packages_loaded()
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    studies <- as.data.frame(body$studies)
    cfg <- body$config

    outcome_type <- if (!is.null(cfg$outcome_type)) cfg$outcome_type else "dichotomous"
    sm <- if (!is.null(cfg$effect_measure)) cfg$effect_measure else if (outcome_type == "continuous") "MD" else "OR"
    is_common <- !is.null(cfg$model) && cfg$model == "Common-effect"
    tau_method <- if (!is.null(cfg$tau_method) && cfg$tau_method %in% c("DL", "ML", "REML")) cfg$tau_method else "REML"
    ref_treatment <- cfg$reference_treatment
    small_values <- if (!is.null(cfg$small_values) && cfg$small_values %in% c("desirable", "undesirable")) cfg$small_values else "undesirable"

    row_errors <- validate_rows(studies, outcome_type)
    if (length(row_errors) > 0) stop(paste(row_errors, collapse = " "))
    arms <- arm_stats(studies)

    pw <- build_pairwise(studies, outcome_type, sm)
    nc <- netconnection(treat1, treat2, studlab, data = pw)
    if (nc$n.subnets != 1) stop("Network is disconnected. NMA requires a connected treatment network. Check the Network Validation step for details on which treatments are isolated.")

    treatments_all <- sort(unique(c(as.character(pw$treat1), as.character(pw$treat2))))
    if (is.null(ref_treatment) || !(ref_treatment %in% treatments_all)) ref_treatment <- treatments_all[1]

    # ONE model fit - every output below (geometry, league table, forest,
    # ranking, rankogram) is derived from this same `m`, per spec.
    m <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw, sm = sm,
                 common = is_common, random = !is_common,
                 method.tau = if (!is_common) tau_method else "DL",
                 reference.group = ref_treatment, small.values = small_values)

    pooled <- if (is_common) "common" else "random"

    # --- Network geometry --------------------------------------------------
    n_trts <- length(treatments_all)
    geom_file <- tempfile(fileext = ".png")
    geom_dim <- max(1600, 700 + n_trts * 90)
    png(geom_file, width = geom_dim, height = geom_dim, res = 200)
    par(mar = c(1, 1, 3, 1))
    # NOTE: `col.multiarm` is deliberately left unset. netgraph()'s internal
    # multi-arm coloring uses missing(col.multiarm) to decide whether to
    # touch a data.frame subset of multi-arm studies; when the network has
    # ZERO multi-arm studies that subset has 0 rows, and explicitly passing
    # col.multiarm (even to a normal color) skips the missing-argument guard
    # and crashes with "replacement has 1 row, data has 0". Leaving it unset
    # lets the package's own default path handle both cases safely.
    netgraph(m, plastic = FALSE, thickness = "number.of.studies",
              points = TRUE, cex.points = 2.2, col.points = "#3730a3", bg.points = "#818cf8",
              number.of.studies = TRUE, cex = 1.1)
    title(main = "Network Geometry (edge label = number of studies; edge thickness = number of studies)", cex.main = 0.9)
    dev.off()

    # --- NMA forest plot (vs reference treatment) ---------------------------
    forest_file <- tempfile(fileext = ".png")
    forest_h <- max(1000, 300 + n_trts * 90)
    png(forest_file, width = 2400, height = forest_h, res = 220)
    forest(m, pooled = pooled, reference.group = ref_treatment,
           smlab = paste0(sm, " vs ", ref_treatment, " (", pooled, " effects)"),
           sortvar = TE, col.square = "#3730a3", col.diamond = "#b3111a")
    dev.off()

    # --- League table --------------------------------------------------------
    lg <- netleague(m, common = is_common, random = !is_common, digits = 2, bracket = "(", separator = "; ")
    lg_mat <- if (is_common) lg$common else lg$random
    league_rows <- as.list(rownames(lg_mat))
    league_matrix <- lapply(seq_len(nrow(lg_mat)), function(i) as.list(as.character(lg_mat[i, ])))

    # --- Treatment ranking (P-scores) ----------------------------------------
    rk <- netrank(m, common = is_common, random = !is_common, small.values = small_values)
    # netmeta 3.4.0 names this field Pscore.fixed internally even though the
    # user-facing argument/model label is "common" - verified via names(rk).
    pscore_col <- if (is_common) rk$Pscore.fixed else rk$Pscore.random
    ranking_df <- data.frame(treatment = names(pscore_col), pscore = as.numeric(pscore_col), stringsAsFactors = FALSE)
    ranking_df <- ranking_df[order(-ranking_df$pscore), ]
    ranking_df$rank <- seq_len(nrow(ranking_df))

    # --- Rankogram -------------------------------------------------------------
    rank_file <- tempfile(fileext = ".png")
    rg <- rankogram(m, common = is_common, random = !is_common, small.values = small_values, nsim = 1000)
    png(rank_file, width = 2400, height = max(1200, 200 + n_trts * 130), res = 220)
    plot(rg, common = is_common, random = !is_common)
    dev.off()

    # --- Heterogeneity ----------------------------------------------------
    tau2_val <- tryCatch(as.numeric(m$tau2), error = function(e) NA)
    i2_val <- tryCatch(as.numeric(m$I2), error = function(e) NA)

    list(
      status = "success",
      summary = list(
        n_studies = length(unique(studies$study)),
        n_treatments = n_trts,
        n_treatment_arms = arms$n_treatment_arms,
        n_multiarm_studies = arms$n_multiarm_studies,
        max_arms = arms$max_arms,
        n_participants = as.numeric(sum(as.numeric(studies$n))),
        n_direct_comparisons = nrow(pw),
        outcome_type = outcome_type,
        effect_measure = sm,
        model = if (is_common) "Common-effect" else "Random-effects",
        tau_method = if (!is_common) tau_method else "not applicable (common-effect model)",
        reference_treatment = ref_treatment,
        connected = TRUE,
        connectivity_note = "A connected network is a precondition for NMA, not confirmation that transitivity/consistency assumptions hold. See the Diagnostics tab for inconsistency assessment.",
        tau2 = if (!is_common) tau2_val else NULL,
        i2 = if (!is_common) i2_val else NULL,
        analysis_status = "Completed"
      ),
      treatments = as.list(treatments_all),
      network_geometry_base64 = paste0("data:image/png;base64,", base64encode(geom_file)),
      forest_plot_base64 = paste0("data:image/png;base64,", base64encode(forest_file)),
      rankogram_base64 = paste0("data:image/png;base64,", base64encode(rank_file)),
      league_table = list(reference_note = "Row treatment vs column treatment; diagonal is blank.", rownames = league_rows, matrix = league_matrix),
      ranking = lapply(seq_len(nrow(ranking_df)), function(i) list(
        treatment = ranking_df$treatment[i], rank = ranking_df$rank[i], pscore = round(ranking_df$pscore[i], 4)
      ))
    )
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] NMA ANALYZE ERROR: ", msg,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/nma-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = msg)
  })
}

# --- Diagnostics: Global Inconsistency, Node Splitting, Direct vs Indirect,
# Contribution Matrix, Net Heat Plot ------------------------------------------
#
# This endpoint re-fits netmeta() using the EXACT SAME {studies, config} the
# Core /analyze call received - same outcome type, effect measure, model,
# tau method and reference treatment - so statistically it is the same
# model, not a different one. It is a second R-side fit (a second HTTP
# request cannot reuse the first call's in-memory model object), which is
# documented here and surfaced to the frontend via `settings_used` rather
# than silently assumed. netheat()'s own default happens to be a
# common-effect-based decomposition regardless of the primary model; where a
# function's own defaults diverge from the primary model selection, that is
# noted explicitly in the response rather than hidden.

#* @serializer unboxedJSON
#* @parser json
#* @post /api/nma/diagnostics
function(req, res) {
  ensure_heavy_packages_loaded()
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    studies <- as.data.frame(body$studies)
    cfg <- body$config

    outcome_type <- if (!is.null(cfg$outcome_type)) cfg$outcome_type else "dichotomous"
    sm <- if (!is.null(cfg$effect_measure)) cfg$effect_measure else if (outcome_type == "continuous") "MD" else "OR"
    is_common <- !is.null(cfg$model) && cfg$model == "Common-effect"
    tau_method <- if (!is.null(cfg$tau_method) && cfg$tau_method %in% c("DL", "ML", "REML")) cfg$tau_method else "REML"
    ref_treatment <- cfg$reference_treatment
    small_values <- if (!is.null(cfg$small_values) && cfg$small_values %in% c("desirable", "undesirable")) cfg$small_values else "undesirable"

    row_errors <- validate_rows(studies, outcome_type)
    if (length(row_errors) > 0) stop(paste(row_errors, collapse = " "))

    pw <- build_pairwise(studies, outcome_type, sm)
    nc <- netconnection(treat1, treat2, studlab, data = pw)
    if (nc$n.subnets != 1) stop("Network is disconnected. NMA requires a connected treatment network.")
    treatments_all <- sort(unique(c(as.character(pw$treat1), as.character(pw$treat2))))
    if (is.null(ref_treatment) || !(ref_treatment %in% treatments_all)) ref_treatment <- treatments_all[1]

    m <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw, sm = sm,
                 common = is_common, random = !is_common,
                 method.tau = if (!is_common) tau_method else "DL",
                 reference.group = ref_treatment, small.values = small_values)
    n_trts <- length(treatments_all)
    pooled <- if (is_common) "common" else "random"

    # --- Global inconsistency: design-based decomposition of Cochran's Q
    # (König, Krahn & Rücker; implemented as decomp.design()). This splits
    # the total Q into a within-designs (heterogeneity) and a
    # between-designs (inconsistency) component - it is a genuine,
    # documented method, not something invented for this app.
    global_inc <- tryCatch({
      dd <- decomp.design(m)
      qd <- dd$Q.decomp
      list(
        method = "Design-based decomposition of Cochran's Q (Krahn/König/Rücker) - splits total heterogeneity+inconsistency into within-design (heterogeneity) and between-design (inconsistency) components.",
        table = lapply(seq_len(nrow(qd)), function(i) list(
          component = rownames(qd)[i], Q = round(qd$Q[i], 4), df = as.integer(qd$df[i]),
          pval = if (is.na(qd$pval[i])) NA else round(qd$pval[i], 4)
        )),
        interpretation = {
          between_p <- qd$pval[rownames(qd) == "Between designs"]
          if (length(between_p) == 1 && !is.na(between_p)) {
            if (between_p < 0.05) "The between-designs (inconsistency) component is statistically significant (p < 0.05): there is evidence of inconsistency between direct and indirect evidence at the network level. This does not by itself identify which comparison(s) are inconsistent - see Node Splitting for comparison-level detail."
            else "The between-designs (inconsistency) component is not statistically significant (p >= 0.05). This is consistent with, but does not prove, a consistent network - a non-significant test can reflect low statistical power rather than true consistency, especially with few studies per design."
          } else "Could not be summarized (insufficient designs to decompose)."
        }
      )
    }, error = function(e) list(method = NULL, table = NULL, interpretation = NULL, unavailable_reason = paste("decomp.design() could not be computed:", conditionMessage(e))))

    # --- Node splitting / direct vs indirect evidence (SIDE method) --------
    node_split <- tryCatch({
      ns <- netsplit(m, common = is_common, random = !is_common, small.values = small_values)
      direct_df <- if (is_common) ns$direct.common else ns$direct.random
      indirect_df <- if (is_common) ns$indirect.common else ns$indirect.random
      compare_df <- if (is_common) ns$compare.common else ns$compare.random
      prop <- if (is_common) ns$prop.common else ns$prop.random
      list(
        method = "Separate Indirect from Direct Evidence (SIDE), back-calculation method (Dias et al. 2010) as implemented in netsplit().",
        table = lapply(seq_along(ns$comparison), function(i) list(
          comparison = ns$comparison[i], k_direct_studies = as.integer(ns$k[i]),
          proportion_direct = round(prop[i], 4),
          direct_estimate = round(direct_df$TE[i], 4), direct_lower = round(direct_df$lower[i], 4), direct_upper = round(direct_df$upper[i], 4),
          indirect_estimate = if (is.na(indirect_df$TE[i])) NA else round(indirect_df$TE[i], 4),
          indirect_lower = if (is.na(indirect_df$lower[i])) NA else round(indirect_df$lower[i], 4),
          indirect_upper = if (is.na(indirect_df$upper[i])) NA else round(indirect_df$upper[i], 4),
          diff_estimate = round(compare_df$TE[i], 4), diff_p = if (is.na(compare_df$p[i])) NA else round(compare_df$p[i], 4),
          evaluable = !is.na(indirect_df$TE[i])
        ))
      )
    }, error = function(e) list(method = NULL, table = NULL, unavailable_reason = paste("netsplit() could not be computed:", conditionMessage(e))))

    # --- Contribution matrix (netcontrib) - real % contribution of each
    # direct comparison to each network estimate; heatmap rendered from
    # those actual values via ggplot2 (netcontrib provides no plot itself).
    contribution <- tryCatch({
      nctb <- netcontrib(m, common = is_common, random = !is_common)
      mat <- if (is_common) nctb$common else nctb$random
      df <- as.data.frame(as.table(mat))
      names(df) <- c("network_comparison", "direct_comparison", "contribution")
      df <- df[df$contribution > 0, ]
      p <- ggplot(df, aes(x = direct_comparison, y = network_comparison, fill = contribution)) +
        geom_tile(color = "white") +
        geom_text(aes(label = scales::percent(contribution, accuracy = 1)), size = 3.4, color = "black") +
        scale_fill_gradient(low = "#eef2ff", high = "#3730a3", labels = scales::percent, name = "Contribution") +
        labs(title = "Contribution Matrix", subtitle = paste0(pooled, "-effects model"),
             x = "Direct comparison (evidence source)", y = "Network comparison (estimate)") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold"))
      plot_file <- tempfile(fileext = ".png")
      w <- max(8, 1.5 + n_trts * (n_trts - 1) / 2 * 0.5)
      ggsave(plot_file, plot = p, width = w, height = w * 0.75, dpi = 300, bg = "white", limitsize = FALSE)
      list(method = "netcontrib() - shortest-path contribution of each direct comparison to each network treatment effect (Papakonstantinou et al. 2018).",
           plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)))
    }, error = function(e) list(method = NULL, plot_base64 = NULL, unavailable_reason = paste("netcontrib() could not be computed:", conditionMessage(e))))

    # --- Net heat plot -------------------------------------------------------
    # netheat() decomposes inconsistency into a heat matrix showing how much
    # each design contributes to inconsistency in each network estimate.
    net_heat <- tryCatch({
      plot_file <- tempfile(fileext = ".png")
      dim_px <- max(1400, 300 + n_trts * 160)
      png(plot_file, width = dim_px, height = dim_px, res = 200)
      netheat(m, random = !is_common, showall = TRUE)
      dev.off()
      list(method = "netheat() (Krahn, König & Rücker) - decomposes inconsistency contributions per design/comparison as a heat matrix with dendrogram.",
           model_used = if (!is_common) "random-effects (matches primary model)" else "common-effect (matches primary model)",
           plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)))
    }, error = function(e) { try(dev.off(), silent = TRUE); list(method = NULL, plot_base64 = NULL, unavailable_reason = paste("netheat() could not be computed:", conditionMessage(e))) })

    list(
      status = "success",
      settings_used = list(outcome_type = outcome_type, effect_measure = sm, model = if (is_common) "Common-effect" else "Random-effects",
                            tau_method = if (!is_common) tau_method else "not applicable", reference_treatment = ref_treatment,
                            note = "Diagnostics were computed by re-fitting netmeta() with the identical settings used for the primary NMA (same data, outcome type, effect measure, model and tau method), so results are directly comparable."),
      global_inconsistency = global_inc,
      node_splitting = node_split,
      direct_vs_indirect = node_split, # same SIDE computation; frontend renders a focused view of the same table
      contribution_matrix = contribution,
      net_heat_plot = net_heat
    )
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] NMA DIAGNOSTICS ERROR: ", msg,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/nma-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = msg)
  })
}

# --- Comparison-adjusted funnel plot (publication bias / small-study effects)
#
# Uses netmeta's OWN funnel.netmeta() (Chaimani & Salanti 2012 methodology):
# each study's effect is centered on its own comparison's network estimate
# before plotting, which is what makes this genuinely comparison-adjusted
# rather than an ordinary pairwise funnel plot with a new title. Re-fits
# with the identical {studies, config} settings as /analyze and
# /diagnostics for the same documented reason (see /diagnostics comment
# above) - same data, same outcome type, effect measure, model, tau method
# and reference treatment, so this is the same statistical model.
#
# The small-study-effect test is NOT plain pairwise Egger: funnel.netmeta()
# only draws its p-value as text on the plot with no accessible return
# value, so the numeric result here is obtained by feeding netmeta's own
# comparison-ADJUSTED values (TE.adj/seTE, the same numbers the plot uses)
# into meta::metabias() - verified to reproduce the plot's own printed
# p-value exactly. This is explicitly labeled as an NMA comparison-adjusted
# test, distinct from an ordinary pairwise Egger test on raw study data.

#* @serializer unboxedJSON
#* @parser json
#* @post /api/nma/funnel
function(req, res) {
  ensure_heavy_packages_loaded()
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    studies <- as.data.frame(body$studies)
    cfg <- body$config

    outcome_type <- if (!is.null(cfg$outcome_type)) cfg$outcome_type else "dichotomous"
    sm <- if (!is.null(cfg$effect_measure)) cfg$effect_measure else if (outcome_type == "continuous") "MD" else "OR"
    is_common <- !is.null(cfg$model) && cfg$model == "Common-effect"
    tau_method <- if (!is.null(cfg$tau_method) && cfg$tau_method %in% c("DL", "ML", "REML")) cfg$tau_method else "REML"
    ref_treatment <- cfg$reference_treatment
    small_values <- if (!is.null(cfg$small_values) && cfg$small_values %in% c("desirable", "undesirable")) cfg$small_values else "undesirable"

    row_errors <- validate_rows(studies, outcome_type)
    if (length(row_errors) > 0) stop(paste(row_errors, collapse = " "))

    pw <- build_pairwise(studies, outcome_type, sm)
    nc <- netconnection(treat1, treat2, studlab, data = pw)
    if (nc$n.subnets != 1) stop("Network is disconnected. NMA requires a connected treatment network.")
    treatments_all <- sort(unique(c(as.character(pw$treat1), as.character(pw$treat2))))
    if (is.null(ref_treatment) || !(ref_treatment %in% treatments_all)) ref_treatment <- treatments_all[1]
    n_direct_comparisons <- nrow(unique(pw[, c("treat1", "treat2")]))
    if (n_direct_comparisons < 3) stop(paste0("At least 3 distinct direct comparisons are required for a meaningful comparison-adjusted funnel plot (currently: ", n_direct_comparisons, ")."))

    m <- netmeta(TE, seTE, treat1, treat2, studlab, data = pw, sm = sm,
                 common = is_common, random = !is_common,
                 method.tau = if (!is_common) tau_method else "DL",
                 reference.group = ref_treatment, small.values = small_values)
    pooled <- if (is_common) "common" else "random"

    n_trts <- length(treatments_all)
    plot_file <- tempfile(fileext = ".png")
    dim_px <- max(1800, 900 + n_trts * 60)
    png(plot_file, width = dim_px + 500, height = dim_px, res = 200) # +500 gives the legend room so it never overlaps the plotted points
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot_data <- funnel(m, order = treatments_all, pooled = pooled,
                         xlab = paste0(sm, " centered at comparison-specific ", pooled, " effect"),
                         legend = TRUE, pos.legend = "topright")
    dev.off()

    # Small-study-effect test on the SAME adjusted values the plot drew (see
    # comment above) - only meaningful/stable with a reasonable number of
    # comparisons, mirrored by the same k.min=3 floor used elsewhere in this
    # app rather than the metabias() package default of 10.
    egger <- tryCatch({
      mg <- metagen(TE.adj, seTE, sm = sm, data = plot_data, common = FALSE, random = FALSE, warn = FALSE)
      metabias(mg, method.bias = "linreg", k.min = 3)
    }, error = function(e) NULL)
    egger_available <- !is.null(egger) && !is.null(egger$pval)

    list(
      status = "success",
      plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)),
      settings_used = list(outcome_type = outcome_type, effect_measure = sm, model = if (is_common) "Common-effect" else "Random-effects",
                            tau_method = if (!is_common) tau_method else "not applicable", reference_treatment = ref_treatment,
                            n_comparisons_plotted = length(unique(plot_data$comparison)),
                            note = "This funnel plot re-fits netmeta() with the identical settings used for the primary NMA (same data, outcome type, effect measure, model and tau method)."),
      small_study_effect_test = list(
        available = egger_available,
        method = "NMA comparison-adjusted small-study-effect test (Egger-type linear regression applied to network-adjusted effect sizes, per Chaimani & Salanti 2012) - NOT an ordinary pairwise Egger test on raw study data.",
        statistic = if (egger_available) round(as.numeric(egger$statistic), 4) else NULL,
        pval = if (egger_available) round(as.numeric(egger$pval), 4) else NULL,
        unavailable_reason = if (!egger_available) "Could not be computed (typically too few comparisons or a design that does not support the regression)." else NULL
      ),
      interpretation = "Visual asymmetry in a comparison-adjusted funnel plot may indicate small-study effects, but asymmetry is not specific to publication bias - it can also arise from genuine clinical heterogeneity between smaller and larger studies, or from chance. This plot does not establish that publication bias is present."
    )
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] NMA FUNNEL ERROR: ", msg,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/nma-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = msg)
  })
}

log_ext_error <- function(tag, req, e) {
  msg <- as.character(e$message)
  log_line <- paste0("[", format(Sys.time()), "] ", tag, " ERROR: ", msg,
                      " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
  try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/nma-error.log", append = TRUE), silent = TRUE)
  msg
}

# Shared parsing for {studies, config} -> fitted primary netmeta object,
# identical to /analyze's own logic, used as the common starting point for
# all three analysis-extension endpoints below so they are guaranteed to
# use the SAME data pipeline and settings as the primary analysis.
parse_and_fit <- function(body) {
  studies <- as.data.frame(body$studies)
  cfg <- body$config
  outcome_type <- if (!is.null(cfg$outcome_type)) cfg$outcome_type else "dichotomous"
  sm <- if (!is.null(cfg$effect_measure)) cfg$effect_measure else if (outcome_type == "continuous") "MD" else "OR"
  is_common <- !is.null(cfg$model) && cfg$model == "Common-effect"
  tau_method <- if (!is.null(cfg$tau_method) && cfg$tau_method %in% c("DL", "ML", "REML")) cfg$tau_method else "REML"
  ref_treatment <- cfg$reference_treatment
  small_values <- if (!is.null(cfg$small_values) && cfg$small_values %in% c("desirable", "undesirable")) cfg$small_values else "undesirable"

  row_errors <- validate_rows(studies, outcome_type)
  if (length(row_errors) > 0) stop(paste(row_errors, collapse = " "))

  pw <- build_pairwise(studies, outcome_type, sm)
  pw <- attach_study_vars(pw, studies)
  nc <- netconnection(treat1, treat2, studlab, data = pw)
  if (nc$n.subnets != 1) stop("Network is disconnected. NMA requires a connected treatment network.")
  treatments_all <- sort(unique(c(as.character(pw$treat1), as.character(pw$treat2))))
  if (is.null(ref_treatment) || !(ref_treatment %in% treatments_all)) ref_treatment <- treatments_all[1]

  fit_args <- list(TE = pw$TE, seTE = pw$seTE, treat1 = pw$treat1, treat2 = pw$treat2, studlab = pw$studlab,
                    sm = sm, common = is_common, random = !is_common,
                    method.tau = if (!is_common) tau_method else "DL",
                    reference.group = ref_treatment, small.values = small_values)
  m <- do.call(netmeta, fit_args)

  list(studies = studies, pw = pw, m = m, outcome_type = outcome_type, sm = sm, is_common = is_common,
       tau_method = tau_method, ref_treatment = ref_treatment, small_values = small_values, treatments_all = treatments_all)
}

settings_block <- function(ctx, extra = list()) {
  c(list(outcome_type = ctx$outcome_type, effect_measure = ctx$sm,
         model = if (ctx$is_common) "Common-effect" else "Random-effects",
         tau_method = if (!ctx$is_common) ctx$tau_method else "not applicable",
         reference_treatment = ctx$ref_treatment), extra)
}

# --- NMA Sensitivity Analysis -------------------------------------------------
# Refits the SAME netmeta() methodology with one deliberately modified
# input (alternative model, alternative tau2 estimator, or a filtered
# dataset), then compares primary vs sensitivity estimates and rankings
# side by side. This is standard NMA sensitivity-analysis practice, not a
# different statistical method - both fits use identical `netmeta()` code.

#* @serializer unboxedJSON
#* @parser json
#* @post /api/nma/sensitivity
function(req, res) {
  ensure_heavy_packages_loaded()
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    sens <- body$sensitivity
    if (is.null(sens) || is.null(sens$method)) stop("No sensitivity method specified.")

    primary_ctx <- parse_and_fit(body)
    method <- sens$method
    excluded_studies <- character(0)

    # Build the sensitivity fit by cloning the primary context and altering
    # exactly one thing, per method - never more than what the user chose.
    sens_body <- body
    sens_cfg <- primary_ctx$m$call
    is_common2 <- primary_ctx$is_common
    tau_method2 <- primary_ctx$tau_method

    if (method == "alt_model") {
      if (is.null(sens$alt_model) || !(sens$alt_model %in% c("Common-effect", "Random-effects"))) stop("alt_model must be 'Common-effect' or 'Random-effects'.")
      sens_body$config$model <- sens$alt_model
    } else if (method == "alt_tau") {
      if (primary_ctx$is_common) stop("Alternative tau2 estimator is not applicable - the primary analysis already uses the common-effect model (no tau2 is estimated).")
      if (is.null(sens$alt_tau_method) || !(sens$alt_tau_method %in% c("DL", "ML", "REML"))) stop("alt_tau_method must be one of DL, ML, REML.")
      if (sens$alt_tau_method == primary_ctx$tau_method) stop(paste0("Alternative tau2 estimator must differ from the primary analysis (", primary_ctx$tau_method, ")."))
      sens_body$config$tau_method <- sens$alt_tau_method
    } else if (method == "exclude_studies") {
      excluded_studies <- as.character(sens$excluded_studies)
      if (length(excluded_studies) == 0) stop("No studies selected for exclusion.")
      remaining <- primary_ctx$studies[!(as.character(primary_ctx$studies$study) %in% excluded_studies), , drop = FALSE]
      if (length(unique(remaining$study)) < 2) stop("Excluding the selected studies leaves fewer than 2 studies - cannot fit a sensitivity NMA.")
      sens_body$studies <- remaining
    } else if (method == "exclude_criterion") {
      crit <- sens$criterion
      if (is.null(crit) || is.null(crit$type)) stop("No exclusion criterion specified.")
      st <- primary_ctx$studies
      if (crit$type == "year_range") {
        if (is.null(st$year)) stop("No Year column was detected in the uploaded dataset - year-based exclusion is unavailable.")
        yr <- suppressWarnings(as.numeric(st$year))
        keep <- is.na(yr) | (yr >= as.numeric(crit$min_year) & yr <= as.numeric(crit$max_year))
        excluded_studies <- unique(as.character(st$study[!keep]))
      } else if (crit$type == "subgroup_value") {
        if (is.null(st$subgroup)) stop("No Subgroup column was detected in the uploaded dataset - subgroup-based exclusion is unavailable.")
        excluded_studies <- unique(as.character(st$study[as.character(st$subgroup) == crit$exclude_value]))
      } else stop(paste0("Unknown exclusion criterion type: ", crit$type))
      if (length(excluded_studies) == 0) stop("The specified criterion excludes no studies.")
      remaining <- st[!(as.character(st$study) %in% excluded_studies), , drop = FALSE]
      if (length(unique(remaining$study)) < 2) stop("This exclusion criterion leaves fewer than 2 studies - cannot fit a sensitivity NMA.")
      sens_body$studies <- remaining
    } else {
      stop(paste0("Unsupported sensitivity method: ", method))
    }

    sensitivity_ctx <- parse_and_fit(sens_body)

    # Comparison table: every comparison present in BOTH fits (a sensitivity
    # fit with fewer studies may lose some direct/indirect comparisons
    # entirely if a treatment becomes disconnected - those are reported as
    # unavailable, never fabricated).
    lg1 <- netleague(primary_ctx$m, common = primary_ctx$is_common, random = !primary_ctx$is_common, digits = 3)
    lg2 <- netleague(sensitivity_ctx$m, common = sensitivity_ctx$is_common, random = !sensitivity_ctx$is_common, digits = 3)
    mat1 <- if (primary_ctx$is_common) lg1$common else lg1$random
    mat2 <- if (sensitivity_ctx$is_common) lg2$common else lg2$random
    common_trts <- intersect(rownames(mat1), rownames(mat2))
    comp_table <- list()
    for (i in seq_along(common_trts)) for (j in seq_along(common_trts)) {
      if (i >= j) next
      t1 <- common_trts[i]; t2 <- common_trts[j]
      comp_table[[length(comp_table) + 1]] <- list(
        comparison = paste0(t2, " vs ", t1),
        primary = as.character(mat1[t1, t2]),
        sensitivity = as.character(mat2[t1, t2])
      )
    }

    forest_file_1 <- tempfile(fileext = ".png")
    n1 <- length(primary_ctx$treatments_all)
    png(forest_file_1, width = 2200, height = max(900, 300 + n1 * 90), res = 220)
    forest(primary_ctx$m, pooled = if (primary_ctx$is_common) "common" else "random", reference.group = primary_ctx$ref_treatment, smlab = paste0(primary_ctx$sm, " - Primary NMA"), sortvar = TE)
    dev.off()
    forest_file_2 <- tempfile(fileext = ".png")
    n2 <- length(sensitivity_ctx$treatments_all)
    png(forest_file_2, width = 2200, height = max(900, 300 + n2 * 90), res = 220)
    forest(sensitivity_ctx$m, pooled = if (sensitivity_ctx$is_common) "common" else "random", reference.group = sensitivity_ctx$ref_treatment, smlab = paste0(sensitivity_ctx$sm, " - Sensitivity NMA"), sortvar = TE)
    dev.off()

    list(
      status = "success",
      settings_used = list(primary = settings_block(primary_ctx), sensitivity = settings_block(sensitivity_ctx),
                            method = method, excluded_studies = as.list(excluded_studies)),
      primary_summary = summarize_fit(primary_ctx$m, primary_ctx$is_common, primary_ctx$small_values),
      sensitivity_summary = summarize_fit(sensitivity_ctx$m, sensitivity_ctx$is_common, sensitivity_ctx$small_values),
      comparison_table = comp_table,
      primary_forest_base64 = paste0("data:image/png;base64,", base64encode(forest_file_1)),
      sensitivity_forest_base64 = paste0("data:image/png;base64,", base64encode(forest_file_2))
    )
  }, error = function(e) {
    res$status <- 200
    list(status = "error", message = log_ext_error("NMA SENSITIVITY", req, e))
  })
}

# --- NMA Subgroup Analysis ----------------------------------------------------
# Uses netmeta's own subgroup() function, which fits a SEPARATE, fully
# valid netmeta model per subgroup (same network methodology, filtered
# data) rather than ad-hoc pairwise splits. netmeta's subgroup() exposes no
# formal between-subgroup interaction test for network meta-analysis, so
# the "between-subgroup" comparison here is explicitly descriptive
# (CI overlap), never presented as a significance test - fabricating one
# would violate this app's no-fake-statistics rule.

#* @serializer unboxedJSON
#* @parser json
#* @post /api/nma/subgroup
function(req, res) {
  ensure_heavy_packages_loaded()
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    ctx <- parse_and_fit(body)
    if (is.null(ctx$studies$subgroup) || all(is.na(ctx$studies$subgroup) | ctx$studies$subgroup == "")) {
      stop("No Subgroup column was detected in the uploaded dataset. Add an optional 'Subgroup' column to your data to use this tool.")
    }
    groups_present <- sort(unique(ctx$pw$subgroup[!is.na(ctx$pw$subgroup)]))
    if (length(groups_present) < 2) {
      stop(paste0("Only ", length(groups_present), " subgroup categor", if (length(groups_present) == 1) "y" else "ies", " detected (", paste(groups_present, collapse = ", "), "). Subgroup analysis requires at least 2 categories."))
    }
    selected <- body$selected_groups
    if (!is.null(selected)) groups_present <- intersect(groups_present, as.character(selected))
    if (length(groups_present) < 2) stop("At least 2 selected subgroups are required.")

    sg <- subgroup(ctx$m, subgroup = ctx$pw$subgroup, common = ctx$is_common, random = !ctx$is_common, method.tau = if (!ctx$is_common) ctx$tau_method else "DL")

    subgroup_results <- list()
    forest_plots <- list()
    for (g in groups_present) {
      sub_m <- sg$networks[[g]]
      if (is.null(sub_m)) { subgroup_results[[g]] <- list(available = FALSE, reason = "Not enough connected data within this subgroup to fit an NMA."); next }
      summ <- summarize_fit(sub_m, ctx$is_common, ctx$small_values)
      subgroup_results[[g]] <- c(list(available = TRUE), summ)
      pf <- tempfile(fileext = ".png")
      tryCatch({
        png(pf, width = 2200, height = max(900, 300 + length(sub_m$trts) * 90), res = 220)
        forest(sub_m, pooled = if (ctx$is_common) "common" else "random", smlab = paste0(ctx$sm, " - ", g), sortvar = TE)
        dev.off()
        forest_plots[[g]] <- paste0("data:image/png;base64,", base64encode(pf))
      }, error = function(e) { try(dev.off(), silent = TRUE) })
    }

    # Descriptive between-subgroup comparison: for each treatment comparison
    # present in every subgroup's league table, list each subgroup's
    # estimate + CI so the user can visually compare overlap. No p-value,
    # no claim of statistical difference - see comment above.
    between <- list()
    trts_lists <- lapply(groups_present, function(g) if (!is.null(sg$networks[[g]])) sort(sg$networks[[g]]$trts) else character(0))
    common_trts <- Reduce(intersect, trts_lists)
    if (length(common_trts) >= 2) {
      for (i in seq_along(common_trts)) for (j in seq_along(common_trts)) {
        if (i >= j) next
        t1 <- common_trts[i]; t2 <- common_trts[j]
        row <- list(comparison = paste0(t2, " vs ", t1))
        for (g in groups_present) {
          sm_g <- sg$networks[[g]]
          if (is.null(sm_g)) next
          lg <- tryCatch(netleague(sm_g, common = ctx$is_common, random = !ctx$is_common, digits = 3), error = function(e) NULL)
          if (is.null(lg)) next
          mat <- if (ctx$is_common) lg$common else lg$random
          if (t1 %in% rownames(mat) && t2 %in% colnames(mat)) row[[g]] <- as.character(mat[t1, t2])
        }
        between[[length(between) + 1]] <- row
      }
    }

    list(
      status = "success",
      settings_used = settings_block(ctx, list(subgroup_variable = "subgroup", groups_analyzed = as.list(groups_present))),
      subgroup_results = subgroup_results,
      forest_plots = forest_plots,
      between_subgroup_comparison = list(
        note = "Descriptive comparison only (95% CI overlap) - netmeta's subgroup() function does not provide a formal statistical test for between-subgroup interaction in a network meta-analysis, so none is reported here.",
        table = between
      )
    )
  }, error = function(e) {
    res$status <- 200
    list(status = "error", message = log_ext_error("NMA SUBGROUP", req, e))
  })
}

# --- NMA Meta-Regression -------------------------------------------------------
# Uses netmeta's own netmetareg(), a network meta-regression under the
# consistency model with independent treatment-by-covariate slopes
# (Cochrane/NICE-DSU-style parameterization: one intercept d[trt] and one
# slope beta[trt:covariate] per non-reference treatment). No plot method
# exists for this object in netmeta (verified: methods(class='netmetareg')
# lists only print/summary), so no figure is fabricated for it.

#* @serializer unboxedJSON
#* @parser json
#* @post /api/nma/metaregression
function(req, res) {
  ensure_heavy_packages_loaded()
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    ctx <- parse_and_fit(body)
    if (is.null(ctx$studies$year) || all(is.na(suppressWarnings(as.numeric(ctx$studies$year))))) {
      stop("No usable numeric covariate ('Year' column) was detected in the uploaded dataset. Add an optional 'Year' column (or another numeric study-level value) to use this tool.")
    }
    covar_by_study <- tapply(suppressWarnings(as.numeric(ctx$studies$year)), as.character(ctx$studies$study), function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else x[1] })
    n_total_studies <- length(covar_by_study)
    n_missing <- sum(is.na(covar_by_study))
    n_usable <- n_total_studies - n_missing
    usable_vals <- covar_by_study[!is.na(covar_by_study)]

    if (n_usable < 3) stop(paste0("Insufficient study-level information for meta-regression: only ", n_usable, " of ", n_total_studies, " studies have a usable Year value (at least 3 are required)."))
    if (length(unique(usable_vals)) < 2) stop("The covariate has no meaningful variation across studies (all usable values are identical) - meta-regression cannot be fitted.")

    # netmetareg() does not tolerate NA covariate values internally (it
    # errors rather than dropping incomplete rows on its own) - studies
    # missing the covariate are excluded and the network is re-fit on the
    # covariate-complete subset, per the "report exclusions, never silently
    # drop" rule. n_missing above already discloses this to the caller.
    reg_ctx <- ctx
    if (n_missing > 0) {
      excluded_ids <- names(covar_by_study)[is.na(covar_by_study)]
      filtered_body <- body
      filtered_body$studies <- ctx$studies[!(as.character(ctx$studies$study) %in% excluded_ids), , drop = FALSE]
      reg_ctx <- tryCatch(parse_and_fit(filtered_body), error = function(e) stop(paste0("Removing the ", n_missing, " study(ies) without a Year value leaves a network that cannot be fit: ", conditionMessage(e))))
    }

    # A local plain-named variable (not a nested reg_ctx$pw$year expression)
    # keeps netmetareg()'s internally-deparsed coefficient labels readable.
    covariate_vec <- reg_ctx$pw$year
    mr <- tryCatch(netmetareg(reg_ctx$m, covar = covariate_vec), error = function(e) stop(paste0("Network meta-regression could not be fitted: ", conditionMessage(e))))
    smr <- summary(mr)
    coef_names <- rownames(smr$b)
    coef_table <- lapply(seq_along(coef_names), function(i) {
      nm <- coef_names[i]
      # A colon in the term name marks a treatment-by-covariate interaction
      # (slope) term; plain treatment names are the intercepts. Verified
      # empirically rather than relying on a specific "beta[...]" prefix,
      # whose exact formatting depends on how the covariate expression gets
      # deparsed internally and isn't a stable string to match on.
      list(term = gsub("covariate_vec", "covariate", nm, fixed = TRUE),
           type = if (grepl(":", nm)) "treatment-by-covariate slope" else "treatment effect (intercept)",
           estimate = round(as.numeric(smr$b[i]), 4),
           ci_lower = round(as.numeric(smr$ci.lb[i]), 4), ci_upper = round(as.numeric(smr$ci.ub[i]), 4),
           pval = round(as.numeric(smr$pval[i]), 4))
    })

    list(
      status = "success",
      settings_used = settings_block(reg_ctx, list(covariate_variable = "year", studies_excluded_for_missing_covariate = as.list(if (n_missing > 0) names(covar_by_study)[is.na(covar_by_study)] else character(0)))),
      covariate_summary = list(n_studies_total = n_total_studies, n_usable = n_usable, n_missing = n_missing,
                                min = round(min(usable_vals), 2), max = round(max(usable_vals), 2)),
      coefficient_table = coef_table,
      model_specification = "Network meta-regression under the consistency model with independent treatment-by-covariate slopes (one intercept and one slope per non-reference treatment), via netmeta::netmetareg().",
      interpretation = "Each 'treatment-by-covariate slope' coefficient (beta) represents how much that treatment's effect (relative to the reference treatment) is estimated to change per one-unit increase in the covariate, holding the network's consistency assumption. This is an observational, study-level (aggregate) association, not a causal estimate - it can be confounded by other study characteristics that vary alongside the covariate, and a significant coefficient does not establish that the covariate causes the treatment-effect difference."
    )
  }, error = function(e) {
    res$status <- 200
    list(status = "error", message = log_ext_error("NMA METAREGRESSION", req, e))
  })
}
