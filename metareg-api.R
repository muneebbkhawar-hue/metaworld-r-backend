library(plumber)
library(jsonlite)
library(metafor)
library(base64enc)

# Pairwise Meta-Regression backend. Dedicated Plumber process on its own
# port (8003), entirely separate from Forest/Funnel/Sensitivity (api.R,
# 8000), TSA (tsa-api.R, 8001), and NMA (nma-api.R, 8002) - and separate
# from the existing NMA Meta-Regression endpoint
# (nma-api.R's /api/nma/metaregression, which fits netmetareg() on a
# NETWORK and is untouched by this file). This tool fits an ordinary
# pairwise meta-regression via metafor::rma.uni(), the standard, validated
# R implementation for this - not a custom/invented statistical method.

STARTED_AT <- Sys.time()
METAFOR_VERSION <- as.character(utils::packageVersion("metafor"))

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

#* Liveness probe for the backend supervisor / frontend status banners.
#* @serializer unboxedJSON
#* @get /health
function() {
  list(status = "ok", service = "metareg-api.R (Pairwise Meta-Regression)",
       package = "metafor", package_version = METAFOR_VERSION,
       uptime_seconds = round(as.numeric(difftime(Sys.time(), STARTED_AT, units = "secs"))))
}

# Global handler: catches errors thrown OUTSIDE an endpoint's own tryCatch
# and reshapes them into a {status, message} JSON body, same convention as
# api.R/tsa-api.R/nma-api.R, so the frontend never sees a bare 500.
#* @plumber
function(pr) {
  pr %>% plumber::pr_set_error(function(req, res, err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "unknown error")
    log_line <- paste0("[", format(Sys.time()), "] UNCAUGHT ERROR: ", msg,
                        " | route: ", req$PATH_INFO,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/metareg-error.log", append = TRUE), silent = TRUE)
    res$status <- 200
    list(status = "error", message = paste("Uncaught server error:", msg))
  })
}

# This app's shared `unboxedJSON` Plumber serializer (jsonlite::toJSON
# without an explicit `null=` argument) has a real, verified quirk: a NESTED
# list element that is R's untyped `NULL` serializes as `{}` (not `null`),
# and a nested TYPED numeric NA (e.g. NA_real_, or NA extracted from a
# numeric vector) serializes as the literal string "NA" (not `null`) -
# only a bare, untyped `NA` reliably becomes JSON `null` at any nesting
# depth. `safe_num()`/`safe_str()` below are the one place this is handled
# for every optional value this endpoint returns, so a missing/inestimable
# statistic is always genuine JSON `null`, never a fabricated number, the
# string "NA", or an empty object that would silently break the frontend's
# `number | null` typing.
safe_num <- function(x, digits = NULL) {
  v <- tryCatch(suppressWarnings(as.numeric(x)), error = function(e) NA)
  if (length(v) == 0 || is.na(v)) return(NA)
  if (!is.null(digits)) v <- round(v, digits)
  v
}
safe_str <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  v <- suppressWarnings(as.character(x))
  if (length(v) == 0 || is.na(v) || v == "") return(NA)
  v
}

log_ext_error <- function(tag, req, e) {
  msg <- tryCatch(conditionMessage(e), error = function(e2) "unknown error")
  log_line <- paste0("[", format(Sys.time()), "] ", tag, " ERROR: ", msg,
                      " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
  try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/metareg-error.log", append = TRUE), silent = TRUE)
  msg
}

# --- Row-level validation (mirrors api.R's validate_rows() conventions for
# dichotomous/continuous data, extended with generic-inverse-variance and
# moderator-aware checks specific to this tool) --------------------------
validate_rows <- function(studies, outcome_type) {
  errors <- character(0)
  n <- nrow(studies)
  if (n == 0) return("No rows found in the uploaded data.")
  if (any(is.na(studies$study) | trimws(as.character(studies$study)) == "")) errors <- c(errors, "One or more rows are missing a study ID.")
  dup <- duplicated(trimws(as.character(studies$study)))
  if (any(dup)) errors <- c(errors, paste0("Duplicated study identifiers: ", paste(unique(studies$study[dup]), collapse = ", "), ". Each row must be a distinct study for pairwise meta-regression."))

  # A column being entirely ABSENT (not just containing NA/invalid values in
  # some rows) must be caught here too, before any numeric coercion - a
  # missing column otherwise sails through the is.na() checks below
  # (any(is.na(numeric(0))) is FALSE) and only fails later inside
  # data.frame() construction with a much less clear R message.
  required_cols <- switch(outcome_type,
    dichotomous = c("event_e", "n_e", "event_c", "n_c"),
    continuous = c("mean_e", "sd_e", "n_e", "mean_c", "sd_c", "n_c"),
    c("te", "se"))
  missing_cols <- required_cols[!(required_cols %in% names(studies))]
  if (length(missing_cols) > 0) return(paste0("Required column(s) missing from the uploaded data: ", paste(missing_cols, collapse = ", "), "."))

  if (outcome_type == "dichotomous") {
    ev_e <- suppressWarnings(as.numeric(studies$event_e)); n_e <- suppressWarnings(as.numeric(studies$n_e))
    ev_c <- suppressWarnings(as.numeric(studies$event_c)); n_c <- suppressWarnings(as.numeric(studies$n_c))
    if (any(is.na(ev_e)) || any(is.na(ev_c))) errors <- c(errors, "One or more rows have a missing/invalid event count.")
    if (any(is.na(n_e) | n_e <= 0) || any(is.na(n_c) | n_c <= 0)) errors <- c(errors, "Total (N) must be a positive number in all rows for both groups.")
    if (any(!is.na(ev_e) & ev_e < 0) || any(!is.na(ev_c) & ev_c < 0)) errors <- c(errors, "Events cannot be negative.")
    if (any(!is.na(ev_e) & !is.na(n_e) & ev_e > n_e) || any(!is.na(ev_c) & !is.na(n_c) & ev_c > n_c)) errors <- c(errors, "Events cannot exceed the total in a row.")
  } else if (outcome_type == "continuous") {
    m_e <- suppressWarnings(as.numeric(studies$mean_e)); sd_e <- suppressWarnings(as.numeric(studies$sd_e)); n_e <- suppressWarnings(as.numeric(studies$n_e))
    m_c <- suppressWarnings(as.numeric(studies$mean_c)); sd_c <- suppressWarnings(as.numeric(studies$sd_c)); n_c <- suppressWarnings(as.numeric(studies$n_c))
    if (any(is.na(m_e)) || any(is.na(m_c))) errors <- c(errors, "One or more rows have a missing/invalid mean.")
    if (any(is.na(sd_e) | sd_e <= 0) || any(is.na(sd_c) | sd_c <= 0)) errors <- c(errors, "SD must be greater than 0 in all rows for both groups.")
    if (any(is.na(n_e) | n_e <= 0) || any(is.na(n_c) | n_c <= 0)) errors <- c(errors, "Total (N) must be a positive number in all rows for both groups.")
  } else { # generic inverse variance
    te <- suppressWarnings(as.numeric(studies$te)); se <- suppressWarnings(as.numeric(studies$se))
    if (any(is.na(te))) errors <- c(errors, "One or more rows have a missing/invalid effect estimate.")
    if (any(is.na(se) | se <= 0)) errors <- c(errors, "SE must be a positive number in all rows.")
  }
  errors
}

# --- Effect-size construction (metafor::escalc) --------------------------
# Only measures metafor's escalc() genuinely implements are exposed; the
# frontend's dropdown is restricted to exactly this list.
build_effect_sizes <- function(studies, outcome_type, effect_measure) {
  if (outcome_type == "dichotomous") {
    df <- data.frame(
      study = as.character(studies$study),
      ai = as.numeric(studies$event_e), n1i = as.numeric(studies$n_e),
      ci = as.numeric(studies$event_c), n2i = as.numeric(studies$n_c),
      stringsAsFactors = FALSE
    )
    df$bi <- df$n1i - df$ai
    df$di <- df$n2i - df$ci
    es <- escalc(measure = effect_measure, ai = ai, bi = bi, ci = ci, di = di, data = df)
  } else if (outcome_type == "continuous") {
    df <- data.frame(
      study = as.character(studies$study),
      m1i = as.numeric(studies$mean_e), sd1i = as.numeric(studies$sd_e), n1i = as.numeric(studies$n_e),
      m2i = as.numeric(studies$mean_c), sd2i = as.numeric(studies$sd_c), n2i = as.numeric(studies$n_c),
      stringsAsFactors = FALSE
    )
    es <- escalc(measure = effect_measure, m1i = m1i, sd1i = sd1i, n1i = n1i, m2i = m2i, sd2i = sd2i, n2i = n2i, data = df)
  } else {
    df <- data.frame(study = as.character(studies$study), yi = as.numeric(studies$te), vi = as.numeric(studies$se)^2, stringsAsFactors = FALSE)
    es <- escalc(measure = "GEN", yi = yi, vi = vi, data = df)
  }
  es$study <- df$study
  es
}

# --- Moderator design construction ---------------------------------------
# Moderator columns are attached to `es` under SAFE, generically-generated
# internal names (modvar1, modvar2, ...) rather than building an R formula
# directly from researcher-supplied text, so there is no formula-injection
# risk and no dependency on user-chosen names being valid R identifiers.
# Coefficient names are mapped back to the researcher's actual moderator
# names/reference categories for display afterwards.
build_moderator_design <- function(es, studies, moderators) {
  term_names <- character(0)
  term_info <- list()
  for (i in seq_along(moderators)) {
    mod <- moderators[[i]]
    gname <- paste0("modvar", i)
    raw <- studies[[mod$name]]
    if (is.null(raw)) stop(paste0("Moderator column '", mod$name, "' was not found in the uploaded data."))
    if (mod$type == "continuous") {
      es[[gname]] <- suppressWarnings(as.numeric(raw))
      term_info[[gname]] <- list(name = mod$name, type = "continuous", reference = NULL, levels = NULL)
    } else {
      vals <- trimws(as.character(raw))
      vals[vals == ""] <- NA
      f <- factor(vals)
      ref <- mod$reference
      if (!is.null(ref) && ref %in% levels(f)) f <- stats::relevel(f, ref = ref) else ref <- levels(f)[1]
      es[[gname]] <- f
      term_info[[gname]] <- list(name = mod$name, type = "categorical", reference = ref, levels = levels(f))
    }
    term_names <- c(term_names, gname)
  }
  list(data = es, terms = term_names, info = term_info)
}

friendly_term_label <- function(coef_name, term_info) {
  if (coef_name == "intrcpt") return("Intercept")
  for (gname in names(term_info)) {
    info <- term_info[[gname]]
    if (coef_name == gname) return(info$name)
    if (info$type == "categorical" && startsWith(coef_name, gname)) {
      level <- substring(coef_name, nchar(gname) + 1)
      return(paste0(info$name, ": ", level, " (vs ", info$reference, ")"))
    }
  }
  coef_name
}

het_block <- function(m) {
  # metafor's rma.uni object has no $QEdf field (verified against the
  # installed metafor 5.0.1 via names(m)) - the QE test's degrees of
  # freedom are k - p (studies minus estimated parameters, matching the
  # "QE(df = ...)" value metafor's own print method reports).
  qe_df_val <- tryCatch(m$k - m$p, error = function(e) NA)
  list(
    tau2 = safe_num(m$tau2, 5),
    i2 = safe_num(m$I2, 2),
    h2 = safe_num(m$H2, 2),
    qe = safe_num(m$QE, 4),
    qe_df = safe_num(qe_df_val),
    qe_p = safe_num(m$QEp, 5)
  )
}

#* @serializer unboxedJSON
#* @parser json
#* @post /api/metareg/analyze
function(req, res) {
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    studies <- as.data.frame(body$studies, stringsAsFactors = FALSE)
    outcome_type <- if (!is.null(body$outcome_type)) body$outcome_type else "dichotomous"
    effect_measure <- if (!is.null(body$effect_measure)) body$effect_measure else if (outcome_type == "continuous") "MD" else if (outcome_type == "dichotomous") "OR" else "GEN"
    model <- if (!is.null(body$model)) body$model else "Random-effects"
    is_common <- model == "Common-effect"
    tau_method <- if (!is.null(body$tau_method) && body$tau_method %in% c("REML", "ML", "DL", "PM", "HS", "SJ")) body$tau_method else "REML"
    ci_level <- if (!is.null(body$ci_level)) as.numeric(body$ci_level) else 95
    knha <- isTRUE(body$knha) && !is_common
    moderators <- if (!is.null(body$moderators)) body$moderators else list()
    moderators <- if (is.data.frame(moderators)) split(moderators, seq_len(nrow(moderators))) else moderators

    if (!(outcome_type %in% c("dichotomous", "continuous", "generic"))) stop("Unsupported outcome type.")
    if (outcome_type == "dichotomous" && !(effect_measure %in% c("RR", "OR", "RD"))) stop("Unsupported effect measure for dichotomous data.")
    if (outcome_type == "continuous" && !(effect_measure %in% c("MD", "SMD"))) stop("Unsupported effect measure for continuous data.")
    if (outcome_type == "generic") effect_measure <- "GEN"

    row_errors <- validate_rows(studies, outcome_type)
    if (length(row_errors) > 0) stop(paste(row_errors, collapse = " "))

    es_all <- build_effect_sizes(studies, outcome_type, effect_measure)

    # --- Moderator validation (before any model fit) ----------------------
    if (length(moderators) == 0) stop("Select at least one moderator for meta-regression. (Use the Sensitivity Analysis or Forest Plot tool for an ordinary pooled estimate with no moderators.)")
    for (mod in moderators) {
      if (is.null(studies[[mod$name]])) stop(paste0("Moderator column '", mod$name, "' was not found in the uploaded data."))
      if (!(mod$type %in% c("continuous", "categorical"))) stop(paste0("Invalid moderator type for '", mod$name, "'."))
    }

    design <- build_moderator_design(es_all, studies, moderators)
    es <- design$data

    # --- Row exclusion: explicit, reported, never silent -------------------
    complete_yi_vi <- !is.na(es$yi) & !is.na(es$vi)
    mod_complete <- rep(TRUE, nrow(es))
    for (gname in design$terms) mod_complete <- mod_complete & !is.na(es[[gname]])
    keep <- complete_yi_vi & mod_complete
    excluded <- es$study[!keep]
    excluded_reasons <- ifelse(!complete_yi_vi[!keep], "Missing or invalid effect estimate/variance", "Missing moderator value")
    es_used <- es[keep, , drop = FALSE]

    if (nrow(es_used) < 3) stop(paste0("Insufficient studies for the requested meta-regression after excluding incomplete rows (", nrow(es_used), " usable of ", nrow(es), " total). At least 3 studies with complete data are required."))

    # --- Constant / degenerate moderator checks -----------------------------
    for (gname in design$terms) {
      info <- design$info[[gname]]
      col <- es_used[[gname]]
      if (info$type == "continuous") {
        if (length(unique(col)) < 2 || stats::sd(col, na.rm = TRUE) == 0) stop(paste0("Moderator '", info$name, "' has no variation across the included studies (constant value) - it cannot be modeled."))
      } else {
        col <- droplevels(col)
        es_used[[gname]] <- col
        if (nlevels(col) < 2) stop(paste0("Moderator '", info$name, "' has fewer than 2 categories among the included studies - it cannot be modeled."))
        small_levels <- names(table(col))[table(col) == 1]
        # informational only - a single-study category can still be fit, just
        # with a very unstable coefficient; not a reason to block the model.
      }
    }

    n_terms <- length(design$terms) # number of moderator TERMS selected (not expanded dummy columns)
    # Rough estimability guard, not an invented statistical cutoff: rma()
    # itself will refuse to fit (and we still catch that below) once
    # parameters >= observations. This just avoids an opaque model-fit
    # failure for the most obvious cases.
    if (nrow(es_used) <= n_terms + 1) stop(paste0("Insufficient studies to fit the requested meta-regression: ", nrow(es_used), " usable studies for ", n_terms, " selected moderator(s) plus an intercept."))

    mods_formula <- stats::as.formula(paste("~", paste(design$terms, collapse = " + ")))
    fit_method <- if (is_common) "FE" else tau_method
    test_type <- if (knha) "knha" else "z"
    alpha_level <- ci_level

    m <- tryCatch(
      rma(yi, vi, mods = mods_formula, data = es_used, method = fit_method, test = test_type, level = alpha_level),
      error = function(e) stop(paste0("Unable to fit the requested model with the supplied data: ", conditionMessage(e)))
    )
    # Overall (unconditional) heterogeneity - same rows, no moderators - so
    # "residual heterogeneity after moderators" (from `m`) can be reported
    # distinctly from heterogeneity BEFORE any moderator is considered.
    m0 <- tryCatch(rma(yi, vi, data = es_used, method = fit_method, test = test_type, level = alpha_level), error = function(e) NULL)

    smry <- summary(m)
    coef_names <- rownames(smry$beta)
    stat_col <- if (test_type == "knha") "tval" else "zval"
    coefficients <- lapply(seq_along(coef_names), function(i) {
      list(
        term = friendly_term_label(coef_names[i], design$info),
        estimate = safe_num(smry$beta[i, 1], 5),
        se = safe_num(smry$se[i], 5),
        ci_lower = safe_num(smry$ci.lb[i], 5),
        ci_upper = safe_num(smry$ci.ub[i], 5),
        statistic = safe_num(smry$zval[i], 4),
        stat_type = if (test_type == "knha") "t" else "z",
        df = if (test_type == "knha") safe_num(smry$dfs) else NA,
        pval = safe_num(smry$pval[i], 6)
      )
    })

    moderator_test <- list(
      label = "Omnibus test of moderators (QM)",
      qm = safe_num(m$QM, 4),
      df1 = safe_num(m$QMdf[1]),
      df2 = safe_num(m$QMdf[2]),
      pval = safe_num(m$QMp, 6),
      test_type = if (test_type == "knha") "F" else "Chi-squared"
    )

    r2 <- if (!is_common) safe_num(m$R2, 2) else NA

    # --- Figure --------------------------------------------------------------
    figure_file <- tempfile(fileext = ".png")
    figure_type <- "coefficient"
    single_mod <- if (length(design$terms) == 1) design$info[[design$terms[1]]] else NULL

    if (!is.null(single_mod) && single_mod$type == "continuous") {
      figure_type <- "bubble"
      png(figure_file, width = 2600, height = 1700, res = 220)
      par(cex.main = 0.95) # headroom for a long moderator name + model description in the title
      # regplot() is metafor's own purpose-built bubble-plot function for
      # exactly this case: bubble size = inverse-variance model weight,
      # fitted regression line, and a confidence band, all computed by
      # metafor itself - not a custom re-implementation.
      tryCatch({
        regplot(m, mod = design$terms[1], xlab = single_mod$name,
                ylab = paste0(effect_measure, " (", if (is_common) "common" else "random", " effects)"),
                main = paste0("Meta-regression: ", effect_measure, " ~ ", single_mod$name,
                               " (", if (is_common) "Common-effect" else paste0("Random-effects, ", tau_method), ")"),
                refline = if (effect_measure %in% c("RD")) 0 else if (effect_measure %in% c("RR", "OR")) 0 else 0,
                labsize = 0.9, digits = 2)
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, paste("Figure could not be generated:", conditionMessage(e)))
      })
      dev.off()
    } else if (!is.null(single_mod) && single_mod$type == "categorical") {
      figure_type <- "categorical"
      n_rows <- length(coef_names)
      labs <- vapply(coef_names, friendly_term_label, character(1), term_info = design$info)
      max_label_chars <- max(nchar(labs))
      axis_cex <- if (max_label_chars > 45) 0.6 else if (max_label_chars > 30) 0.7 else if (max_label_chars > 18) 0.8 else 0.9
      fig_width <- max(2400, 1400 + max_label_chars * 26)
      png(figure_file, width = fig_width, height = max(900, 300 + nlevels(es_used[[design$terms[1]]]) * 220), res = 220)
      tryCatch({
        # Category coefficient/CI plot: intercept (reference category) plus
        # each non-reference level's estimated offset - built from the same
        # `smry` coefficient table above (no separate computation), plotted
        # with base R so long category names never overlap (one per row).
        # The left margin is sized from the ACTUAL rendered width of the
        # longest label (strwidth() measured on this open device, in
        # inches via `mai`) rather than a guessed "lines" count - a fixed
        # guess is what previously clipped long category names (e.g.
        # "Latin America and the Caribbean (vs Southern Europe)") off the
        # left edge, or over-corrected and pushed the whole plot area (and
        # the title) off the RIGHT edge instead.
        par(cex.main = 0.95)
        label_width_in <- max(strwidth(labs, units = "inches", cex = axis_cex))
        par(mai = c(0.9, label_width_in + 0.35, 0.75, 0.35))
        est <- as.numeric(smry$beta[, 1]); lo <- as.numeric(smry$ci.lb); hi <- as.numeric(smry$ci.ub)
        plot(est, seq_len(n_rows), xlim = range(c(lo, hi, 0)) * 1.15, ylim = c(0.5, n_rows + 0.5),
             yaxt = "n", ylab = "", xlab = paste0(effect_measure, " coefficient (", ci_level, "% CI)"),
             main = paste0("Meta-regression: ", effect_measure, " ~ ", single_mod$name, " (ref: ", single_mod$reference, ")"),
             pch = 18, cex = 1.6, col = "#3730a3")
        axis(2, at = seq_len(n_rows), labels = labs, las = 1, cex.axis = axis_cex)
        segments(lo, seq_len(n_rows), hi, seq_len(n_rows), col = "#3730a3", lwd = 2)
        abline(v = 0, lty = 2, col = "#94a3b8")
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, paste("Figure could not be generated:", conditionMessage(e)))
      })
      dev.off()
    } else {
      figure_type <- "coefficient"
      n_rows <- length(coef_names)
      labs <- vapply(coef_names, friendly_term_label, character(1), term_info = design$info)
      max_label_chars <- max(nchar(labs))
      axis_cex <- if (max_label_chars > 45) 0.55 else if (max_label_chars > 30) 0.65 else if (max_label_chars > 18) 0.75 else 0.85
      fig_width <- max(2400, 1400 + max_label_chars * 26)
      png(figure_file, width = fig_width, height = max(900, 300 + length(coef_names) * 90), res = 220)
      tryCatch({
        par(cex.main = 0.95)
        label_width_in <- max(strwidth(labs, units = "inches", cex = axis_cex))
        par(mai = c(0.9, label_width_in + 0.35, 0.75, 0.35))
        est <- as.numeric(smry$beta[, 1]); lo <- as.numeric(smry$ci.lb); hi <- as.numeric(smry$ci.ub)
        plot(est, seq_len(n_rows), xlim = range(c(lo, hi, 0)) * 1.15, ylim = c(0.5, n_rows + 0.5),
             yaxt = "n", ylab = "", xlab = paste0(effect_measure, " coefficient (", ci_level, "% CI)"),
             main = paste0("Multivariable meta-regression coefficients (", if (is_common) "Common-effect" else paste0("Random-effects, ", tau_method), ")"),
             pch = 18, cex = 1.6, col = "#3730a3")
        axis(2, at = seq_len(n_rows), labels = labs, las = 1, cex.axis = axis_cex)
        segments(lo, seq_len(n_rows), hi, seq_len(n_rows), col = "#3730a3", lwd = 2)
        abline(v = 0, lty = 2, col = "#94a3b8")
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, paste("Figure could not be generated:", conditionMessage(e)))
      })
      dev.off()
    }

    # --- Influence diagnostics (metafor::influence.rma.uni) -----------------
    diagnostics <- tryCatch({
      infl <- influence(m)
      idf <- infl$inf
      list(
        available = TRUE,
        method = "metafor::influence.rma.uni() - Cook's distance, hat/leverage, DFFITS, and studentized residuals from the fitted model.",
        table = lapply(seq_along(idf$cook.d), function(i) list(
          study = as.character(es_used$study[i]),
          cooks_distance = safe_num(idf$cook.d[i], 4),
          hat = safe_num(idf$hat[i], 4),
          dffits = safe_num(idf$dffits[i], 4),
          rstudent = safe_num(idf$rstudent[i], 4),
          influential = isTRUE(idf$inf[i] == "*")
        )),
        unavailable_reason = NA
      )
    }, error = function(e) list(available = FALSE, method = NA, table = list(), unavailable_reason = paste("Influence diagnostics could not be computed:", conditionMessage(e))))

    # --- Warnings (cautious, non-fabricated) ---------------------------------
    warnings_list <- list()
    if (nrow(es_used) < (n_terms + 1) * 10 && n_terms >= 1) {
      warnings_list[[length(warnings_list) + 1]] <- "Meta-regression estimates may be unstable when many moderators are modeled relative to the number of studies."
    }
    warnings_list[[length(warnings_list) + 1]] <- "Meta-regression uses study-level characteristics. Associations observed at the study level should not automatically be interpreted as individual-level effects."
    if (length(excluded) > 0) {
      warnings_list[[length(warnings_list) + 1]] <- paste0(length(excluded), " stud(y/ies) were excluded due to missing effect data or missing moderator values - see the Data/Exclusion Summary.")
    }

    # --- Interpretation (factual, cautious, non-causal) ----------------------
    primary_term_idx <- if (length(coef_names) > 1) 2 else 1
    interp_parts <- c(
      paste0("Model: ", if (is_common) "Common-effect" else paste0("Random-effects (tau\u00b2 estimator: ", tau_method, ")"),
             " meta-regression of ", effect_measure, " on ", length(design$terms), " moderator(s), fitted via metafor::rma.uni() (metafor ", METAFOR_VERSION, ")."),
      paste0("Omnibus test of moderators: ", moderator_test$test_type, " = ", moderator_test$qm,
             if (test_type == "knha") paste0(" (df1=", moderator_test$df1, ", df2=", moderator_test$df2, ")") else paste0(" (df=", moderator_test$df1, ")"),
             ", p = ", moderator_test$pval, "."),
      "Evidence of an association was observed for a moderator only if its individual coefficient's confidence interval excludes the null value and the p-value is below the chosen threshold - this does not by itself establish that the moderator is clinically or causally important.",
      "Associations are derived from study-level (aggregate) characteristics and should not automatically be interpreted as individual-level effects."
    )
    interpretation <- paste(interp_parts, collapse = " ")

    model_formula_display <- paste0(effect_measure, " ~ ", paste(vapply(design$terms, function(g) {
      info <- design$info[[g]]
      if (info$type == "categorical") paste0(info$name, " (ref: ", info$reference, ")") else info$name
    }, character(1)), collapse = " + "))

    list(
      status = "success",
      settings_used = list(
        outcome_type = outcome_type, effect_measure = effect_measure,
        model = if (is_common) "Common-effect" else "Random-effects",
        tau_method = if (!is_common) tau_method else "not applicable (common-effect model)",
        ci_level = ci_level, knha = knha,
        moderators = lapply(design$terms, function(g) {
          info <- design$info[[g]]
          list(name = info$name, type = info$type, reference = safe_str(info$reference))
        }),
        r_package = "metafor", r_package_version = METAFOR_VERSION, r_function = "rma.uni()"
      ),
      model_formula = model_formula_display,
      data_summary = list(
        n_studies_included = nrow(es_used),
        n_studies_excluded = length(excluded),
        excluded_studies = if (length(excluded) > 0) lapply(seq_along(excluded), function(i) list(study = as.character(excluded[i]), reason = excluded_reasons[i])) else list()
      ),
      overall_heterogeneity = if (!is.null(m0)) het_block(m0) else NA,
      residual_heterogeneity = het_block(m),
      r2 = r2,
      moderator_test = moderator_test,
      coefficients = coefficients,
      figure_base64 = paste0("data:image/png;base64,", base64encode(figure_file)),
      figure_type = figure_type,
      diagnostics = diagnostics,
      interpretation = interpretation,
      warnings = warnings_list
    )
  }, error = function(e) {
    res$status <- 200
    msg <- log_ext_error("METAREG ANALYZE", req, e)
    list(status = "error", message = msg)
  })
}
