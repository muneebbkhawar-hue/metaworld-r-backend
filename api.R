library(plumber)
library(jsonlite)
library(meta)
library(base64enc)

STARTED_AT <- Sys.time()

# Production CORS: restrict to the deployed frontend origin via the
# ALLOWED_ORIGIN env var (comma-separated for multiple origins, e.g. a
# production domain + a Vercel preview domain). Defaults to "*" so local
# development (no env var set) keeps working exactly as before.
ALLOWED_ORIGINS <- strsplit(Sys.getenv("ALLOWED_ORIGIN", "*"), ",")[[1]]
resolve_cors_origin <- function(req) {
  if (length(ALLOWED_ORIGINS) == 1 && ALLOWED_ORIGINS[1] == "*") return("*")
  origin <- req$HTTP_ORIGIN
  if (!is.null(origin) && origin %in% trimws(ALLOWED_ORIGINS)) return(origin)
  trimws(ALLOWED_ORIGINS[1]) # fallback: never omit the header entirely, just don't reflect an unrecognized origin
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
  list(status = "ok", service = "api.R (Forest/Funnel/Sensitivity)",
       uptime_seconds = round(as.numeric(difftime(Sys.time(), STARTED_AT, units = "secs"))))
}

# Global handler: catches errors thrown OUTSIDE an endpoint's own tryCatch
# (e.g. malformed JSON body, framework-level failures) and reshapes them into
# a {status, message} JSON body instead of Plumber's bare 500 with no message,
# so the frontend never has to fall back to "Unknown error occurred inside R."
#* @plumber
function(pr) {
  pr %>% plumber::pr_set_error(function(req, res, err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "unknown error")
    log_line <- paste0("[", format(Sys.time()), "] UNCAUGHT ERROR: ", msg,
                        " | route: ", req$PATH_INFO,
                        " | raw body: ", tryCatch(req$postBody, error = function(e) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/error.log", append = TRUE), silent = TRUE)
    res$status <- 200
    list(status = "error", message = paste("Uncaught server error:", msg))
  })
}

# --- Leave-One-Out Sensitivity Analysis ---
get_eff <- function(m, is_random, prop) {
  if (is.null(m)) return(NA)
  if (is_random) {
    if (!is.null(m[[paste0(prop, ".random")]])) return(m[[paste0(prop, ".random")]])
  } else {
    if (!is.null(m[[paste0(prop, ".common")]])) return(m[[paste0(prop, ".common")]])
    if (!is.null(m[[paste0(prop, ".fixed")]])) return(m[[paste0(prop, ".fixed")]])
  }
  return(m[[prop]])
}

# --- Test for overall effect (RevMan-style: "Z = .. (P = ..)") -------------
# Mirrors get_eff()'s own is_random-aware field lookup above, since which
# model (random vs. common) the Z/P-value should come from depends on which
# model the user selected/is being plotted - not hardcoded to one. meta's
# field naming has changed across versions (".common" in current releases,
# ".fixed" in older ones) - checking both, exactly like get_eff() already
# does, keeps this working regardless of the installed meta version.
get_overall_stat <- function(m, is_random, prop) {
  if (is_random) return(m[[paste0(prop, ".random")]])
  val <- m[[paste0(prop, ".common")]]
  if (is.null(val)) val <- m[[paste0(prop, ".fixed")]]
  val
}
overall_effect_stats <- function(m, is_random) {
  z <- suppressWarnings(as.numeric(get_overall_stat(m, is_random, "zval")))
  p <- suppressWarnings(as.numeric(get_overall_stat(m, is_random, "pval")))
  list(
    z_overall = if (length(z) == 0 || is.na(z)) NA else round(z, 2),
    # format.pval matches RevMan's own display convention ("P = 0.53", or
    # "P < 0.00001" for a very small p-value instead of misleading rounding to 0).
    pval_overall = if (length(p) == 0 || is.na(p)) NA else format.pval(p, digits = 2, eps = 0.00001)
  )
}

safe_array <- function(x) {
  val <- suppressWarnings(as.character(x))
  val[is.na(val) | val == "NaN" | val == "Inf" | val == "-Inf"] <- "NA"
  return(val)
}

#* @parser json
#* @serializer unboxedJSON
#* @post /api/meta/sensitivity-loo
function(req, res) {
  tryCatch({
    body <- req$body
    if(is.null(body)) body <- fromJSON(req$postBody)
    studies <- as.data.frame(body$studies)
    conf <- body$config

    is_random <- conf$model == "Random-effects"
    ci_level_val <- as.numeric(conf$ci_level) / 100
    inf_val <- if(!is.null(conf$inference) && conf$inference == "Knapp-Hartung") "HK" else "classic"
    tau_val <- if(!is.null(conf$tau_estimator)) conf$tau_estimator else "REML"
    sm_val <- if(!is.null(conf$effect_measure)) conf$effect_measure else "RR"

    if ("event_e" %in% names(studies)) {
      args <- list(event.e = as.numeric(studies$event_e), n.e = as.numeric(studies$n_e), event.c = as.numeric(studies$event_c), n.c = as.numeric(studies$n_c), studlab = as.character(studies$study), sm = sm_val, method = "MH", random = is_random, level = ci_level_val)
      if(is_random) { args$method.tau <- tau_val; args$method.random.ci <- inf_val }
      m <- do.call(metabin, args)
    } else if ("mean_e" %in% names(studies)) {
      if(sm_val %in% c("RR","OR","HR")) sm_val <- "MD"
      args <- list(n.e = as.numeric(studies$n_e), mean.e = as.numeric(studies$mean_e), sd.e = as.numeric(studies$sd_e), n.c = as.numeric(studies$n_c), mean.c = as.numeric(studies$mean_c), sd.c = as.numeric(studies$sd_c), studlab = as.character(studies$study), sm = sm_val, random = is_random, level = ci_level_val)
      if(is_random) { args$method.tau <- tau_val; args$method.random.ci <- inf_val }
      m <- do.call(metacont, args)
    } else {
      args <- list(TE = as.numeric(studies$te), seTE = as.numeric(studies$se), studlab = as.character(studies$study), sm = sm_val, random = is_random, level = ci_level_val)
      if(is_random) { args$method.tau <- tau_val; args$method.random.ci <- inf_val }
      m <- do.call(metagen, args)
    }

    loo <- if (is_random) metainf(m, pooled = "random") else metainf(m, pooled = "common")
    plot_file <- tempfile(fileext = ".png")
    # +50px baseline vs. before, matching the main forest endpoint, to leave
    # room for the "Test for overall effect" line added below.
    png(plot_file, width = 2800, height = max(1200, 300 + length(m$studlab) * 55), res = 200, pointsize = 11)
    par(mar = c(5, 5, 4, 2) + 0.1)
    # Bakes "Test for overall effect: Z = .. (P = ..)" directly into the LOO
    # plot image for the pooled (all-studies) estimate, RevMan style, same
    # as the main forest plot tool. addrows.below.overall reserves a blank
    # row so the text doesn't overlap the x-axis line/reference line, which
    # forest.metainf does not otherwise leave room for.
    forest(loo, test.overall = TRUE, addrows.below.overall = 2)
    dev.off()

    is_ratio <- sm_val %in% c("RR", "OR", "HR")
    fmt <- function(x) { sapply(x, function(v) { val <- suppressWarnings(as.numeric(v)); if(is.na(val)) return(NA); round(if(is_ratio) exp(val) else val, 3) }) }

    res$status <- 200
    list(status = "success", plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)), table = list(omitted_study = safe_array(c("Full Analysis", as.character(loo$studlab))), k = safe_array(c(length(m$studlab), rep(length(m$studlab) - 1, length(loo$studlab)))), pooled_effect = safe_array(c(fmt(get_eff(m, is_random, "TE")), fmt(loo$TE))), lower_ci = safe_array(c(fmt(get_eff(m, is_random, "lower")), fmt(loo$lower))), upper_ci = safe_array(c(fmt(get_eff(m, is_random, "upper")), fmt(loo$upper))), pval = safe_array(round(c(get_eff(m, is_random, "pval"), as.numeric(loo$pval)), 4)), tau2 = safe_array(round(c(get_eff(m, is_random, "tau2"), as.numeric(loo$tau2)), 4)), i2 = safe_array(round(c(get_eff(m, is_random, "I2"), as.numeric(loo$I2)) * 100, 1))))
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] SENSITIVITY-LOO CAUGHT ERROR: ", msg,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = msg)
  })
}

# --- UNIFIED FOREST PLOT GENERATOR ---
generate_custom_forest <- function(m, plot_file, e_lab, c_lab, config) {
  # +50px baseline vs. before to leave room for the "Test for overall
  # effect" line(s) now printed below the heterogeneity stats (RevMan style).
  png(plot_file, width = 2800, height = max(1200, 300 + length(m$studlab) * 55), res = 200, pointsize = 11)
  par(mar = c(5, 5, 4, 2) + 0.1)

  # Extract options from config
  show_pi <- !is.null(config$prediction_interval) && config$prediction_interval == "ON"
  ci_lvl <- if(!is.null(config$ci_level)) as.numeric(config$ci_level)/100 else 0.95

  forest(m,
         col.diamond = "black",
         col.square = "blue",
         print.I2 = TRUE,
         print.tau2 = TRUE,
         studlab = TRUE,
         lab.e = e_lab,
         lab.c = c_lab,
         prediction = show_pi,
         level = ci_lvl,
         spacing = 1.3,
         # Bakes "Test for overall effect: Z = .. (P = ..)" directly into the
         # plot image (RevMan style), for whichever model(s) are active -
         # meta's own formatting/rounding, not a hand-rolled duplicate.
         test.overall = TRUE)
  dev.off()
}

# 1. Dichotomous Endpoint
#* @parser json
#* @serializer unboxedJSON
#* @post /api/meta/dichotomous
function(req) {
  body <- req$body
  if(is.null(body)) body <- fromJSON(req$postBody)

  studies <- as.data.frame(body$studies)
  conf <- body$config
  e_lab <- if(!is.null(body$exp_lab) && body$exp_lab != "") body$exp_lab else "Experimental"
  c_lab <- if(!is.null(body$ctrl_lab) && body$ctrl_lab != "") body$ctrl_lab else "Control"

  is_random <- conf$model == "Random-effects"
  ci_level_val <- as.numeric(conf$ci_level) / 100
  hk_val <- if(conf$inference == "Knapp-Hartung") "hk" else "classic"

  m <- metabin(event.e = as.numeric(studies$event_e), n.e = as.numeric(studies$n_e),
               event.c = as.numeric(studies$event_c), n.c = as.numeric(studies$n_c),
               studlab = as.character(studies$study),
               sm = conf$effect_measure,
               method = "MH",
               random = is_random,
               method.tau = conf$tau_estimator,
               method.random.ci = hk_val,
               level = ci_level_val,
               clevent = e_lab, clctrl = c_lab)

  plot_file <- tempfile(fileext = ".png")
  generate_custom_forest(m, plot_file, e_lab, c_lab, conf)

  ov <- overall_effect_stats(m, is_random)
  list(
    forest_plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)),
    stats = list(
      k = length(m$studlab),
      i2 = round(m$I2 * 100, 1),
      tau2 = round(m$tau2, 4),
      q = round(m$Q, 2),
      q_pval = round(m$pval.Q, 4),
      z_overall = ov$z_overall,
      pval_overall = ov$pval_overall
    )
  )
}

# 2. Continuous Endpoint
#* @parser json
#* @serializer unboxedJSON
#* @post /api/meta/continuous
function(req) {
  body <- req$body
  if(is.null(body)) body <- fromJSON(req$postBody)

  studies <- as.data.frame(body$studies)
  conf <- body$config
  e_lab <- if(!is.null(body$exp_lab) && body$exp_lab != "") body$exp_lab else "Experimental"
  c_lab <- if(!is.null(body$ctrl_lab) && body$ctrl_lab != "") body$ctrl_lab else "Control"

  is_random <- conf$model == "Random-effects"
  ci_level_val <- as.numeric(conf$ci_level) / 100
  hk_val <- if(conf$inference == "Knapp-Hartung") "hk" else "classic"

  m <- metacont(n.e = as.numeric(studies$n_e), mean.e = as.numeric(studies$mean_e), sd.e = as.numeric(studies$sd_e),
                n.c = as.numeric(studies$n_c), mean.c = as.numeric(studies$mean_c), sd.c = as.numeric(studies$sd_c),
                studlab = as.character(studies$study),
                sm = conf$effect_measure,
                random = is_random,
                method.tau = conf$tau_estimator,
                method.random.ci = hk_val,
                level = ci_level_val,
                label.e = e_lab, label.c = c_lab)

  plot_file <- tempfile(fileext = ".png")
  generate_custom_forest(m, plot_file, e_lab, c_lab, conf)

  ov <- overall_effect_stats(m, is_random)
  list(
    forest_plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)),
    stats = list(
      k = length(m$studlab),
      i2 = round(m$I2 * 100, 1),
      tau2 = round(m$tau2, 4),
      q = round(m$Q, 2),
      q_pval = round(m$pval.Q, 4),
      z_overall = ov$z_overall,
      pval_overall = ov$pval_overall
    )
  )
}

# 3. Inverse Variance Endpoint
#* @parser json
#* @serializer unboxedJSON
#* @post /api/meta/iv
function(req) {
  body <- req$body
  if(is.null(body)) body <- fromJSON(req$postBody)

  studies <- as.data.frame(body$studies)
  conf <- body$config

  is_random <- conf$model == "Random-effects"
  ci_level_val <- as.numeric(conf$ci_level) / 100
  hk_val <- if(conf$inference == "Knapp-Hartung") "hk" else "classic"
  # BUG FIX: this previously hardcoded sm = "RR" regardless of the
  # frontend's "Generic Effect Type" selection (HR/RR/OR/GEN) - every
  # generic inverse-variance forest plot was labeled "Risk Ratio" even when
  # pooling log(HR) or log(OR) data. meta::metagen()/forest() already knows
  # how to label and back-transform "HR" and "OR" correctly (verified
  # directly against the `meta` package - forest() prints "Hazard Ratio"/
  # "HR"/"lnHR" when sm="HR" is actually passed), so this only needed to
  # stop discarding the value the frontend already sent.
  sm_val <- if (!is.null(conf$effect_measure) && conf$effect_measure != "") conf$effect_measure else "RR"

  m <- metagen(TE = as.numeric(studies$te), seTE = as.numeric(studies$se),
               studlab = as.character(studies$study),
               sm = sm_val,
               random = is_random,
               method.tau = conf$tau_estimator,
               method.random.ci = hk_val,
               level = ci_level_val)

  plot_file <- tempfile(fileext = ".png")
  generate_custom_forest(m, plot_file, "Experimental", "Control", conf)

  ov <- overall_effect_stats(m, is_random)
  list(
    forest_plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)),
    stats = list(
      k = length(m$studlab),
      i2 = round(m$I2 * 100, 1),
      tau2 = round(m$tau2, 4),
      q = round(m$Q, 2),
      q_pval = round(m$pval.Q, 4),
      z_overall = ov$z_overall,
      pval_overall = ov$pval_overall
    )
  )
}

# Funnel Helper with Options
create_advanced_funnel <- function(m, studies, conf) {
  # metabias() refuses to compute below k.min=10 by default and silently
  # returns a stub object whose pval/estimate/statistic are all NULL (it does
  # not error), which the frontend's own "computed but interpret cautiously"
  # advisory notice for k<10 implies should still produce real numbers.
  # k.min=3 is the statistical floor for a 2-parameter linear regression.
  egger <- tryCatch({ metabias(m, method.bias = "linreg", k.min = 3) }, error = function(e) { NULL })
  if (!is.null(egger) && is.null(egger$pval)) egger <- NULL
  begg <- tryCatch({ metabias(m, method.bias = "rank", k.min = 3) }, error = function(e) { NULL })
  if (!is.null(begg) && is.null(begg$pval)) begg <- NULL

  n_studies <- nrow(studies)
  plot_file <- tempfile(fileext = ".png")

  png(plot_file, width = 1400, height = 900, res = 150)
  par(mar = c(5, 5, 4, 14), xpd = TRUE)
  study_colors <- rainbow(n_studies)

  show_contours <- !is.null(conf$confidence_region) && conf$confidence_region == TRUE

  if (show_contours) {
    funnel(m, studlab = FALSE, pch = 19, col = study_colors, cex = 1.4, contour = c(0.90, 0.95, 0.99), col.contour = c("gray95", "gray90", "gray85"))
  } else {
    funnel(m, studlab = FALSE, pch = 19, col = study_colors, cex = 1.4)
  }

  title(main = paste("Funnel Plot Analysis\nEgger Test p =", if(!is.null(egger)) round(egger$pval, 4) else "N/A"), cex.main = 1.1)
  legend("topright", inset = c(-0.38, 0), legend = studies$study, col = study_colors, pch = 19, pt.cex = 1.2, cex = 0.85, bty = "n", title = "Study")
  dev.off()

  list(
    eggers_p_value = if(!is.null(egger)) round(egger$pval, 4) else NULL,
    bias_intercept = if(!is.null(egger)) round(egger$estimate["bias"], 3) else NULL,
    t_val = if(!is.null(egger)) round(egger$statistic, 2) else NULL,
    begg_p_value = if(!is.null(begg)) round(begg$pval, 4) else NULL,
    n_studies = n_studies,
    funnel_plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file))
  )
}

# 4A. Bias: Dichotomous
#* @parser json
#* @serializer unboxedJSON
#* @post /api/meta/bias-dich
function(req) {
  body <- req$body
  if(is.null(body)) body <- fromJSON(req$postBody)
  studies <- as.data.frame(body$studies)
  conf <- body$config
  sm_val <- if (!is.null(conf$effect_measure)) conf$effect_measure else "RR"

  m <- metabin(as.numeric(studies$event_e), as.numeric(studies$n_e), as.numeric(studies$event_c), as.numeric(studies$n_c), studlab=as.character(studies$study), sm=sm_val)
  create_advanced_funnel(m, studies, conf)
}

# 4B. Bias: Continuous
#* @parser json
#* @serializer unboxedJSON
#* @post /api/meta/bias-cont
function(req) {
  body <- req$body
  if(is.null(body)) body <- fromJSON(req$postBody)
  studies <- as.data.frame(body$studies)
  conf <- body$config
  sm_val <- if (!is.null(conf$effect_measure)) conf$effect_measure else "MD"

  m <- metacont(as.numeric(studies$n_e), as.numeric(studies$mean_e), as.numeric(studies$sd_e), as.numeric(studies$n_c), as.numeric(studies$mean_c), as.numeric(studies$sd_c), studlab=as.character(studies$study), sm=sm_val)
  create_advanced_funnel(m, studies, conf)
}

# 4C. Bias: Inverse Variance
#* @parser json
#* @serializer unboxedJSON
#* @post /api/meta/bias-iv
function(req) {
  body <- req$body
  if(is.null(body)) body <- fromJSON(req$postBody)
  studies <- as.data.frame(body$studies)
  conf <- body$config

  # Same fix as /api/meta/iv above - was hardcoded to sm="RR" regardless of
  # what effect measure the data actually represents.
  sm_val <- if (!is.null(conf$effect_measure) && conf$effect_measure != "") conf$effect_measure else "RR"
  m <- metagen(as.numeric(studies$te), as.numeric(studies$se), studlab=as.character(studies$study), sm=sm_val)
  create_advanced_funnel(m, studies, conf)
}

# --- GRADE Evidence Profile Assessment (Brozek et al., 2021) ---
# NOTE: this logic previously existed only in the orphaned grade_api.R /
# grade_engine.R files, which were never wired into entrypoint.R's R_SCRIPT
# options nor render.yaml's service list nor sourced from here - so
# /api/grade/evaluate did not actually exist on the deployed metaworld-api
# service despite the frontend (app/tools/grade/page.tsx) calling it via
# META_API_URL (this file's own service). Moved here, onto the service the
# frontend already targets, fixing that gap. grade_engine.R's slightly more
# complete rule set (modular assess_* functions, ois_threshold=300) was kept
# over grade_api.R's near-duplicate inline version.
grade_assess_inconsistency <- function(i2) {
  rating <- if (i2 < 40) "Not serious" else if (i2 <= 75) "Serious" else "Very serious"
  steps <- if (rating == "Serious") 1 else if (rating == "Very serious") 2 else 0
  list(rating = rating, steps = steps)
}

grade_assess_indirectness <- function(override_rating = NULL) {
  if (!is.null(override_rating) && override_rating != "") {
    steps <- switch(override_rating, "Not serious" = 0, "Serious" = 1, "Very serious" = 2, 0)
    return(list(rating = override_rating, steps = steps))
  }
  list(rating = "Not serious", steps = 0)
}

grade_assess_imprecision <- function(sample_size, ois_threshold = 300) {
  if (sample_size < ois_threshold) return(list(rating = "Serious", steps = 1))
  list(rating = "Not serious", steps = 0)
}

grade_calculate_certainty <- function(study_design, rob_rating, inconsistency_steps, indirectness_steps, imprecision_steps, pub_bias_rating) {
  base_score <- switch(toupper(study_design), "RCT" = 4, "OBSERVATIONAL" = 2, "MODELING" = 3, 3)
  rob_steps <- switch(rob_rating, "Not serious" = 0, "Serious" = 1, "Very serious" = 2, 0)
  pub_steps <- switch(pub_bias_rating, "Undetected" = 0, "Suspected" = 1, "Serious" = 1, "Very serious" = 2, 0)
  total_downgrades <- rob_steps + inconsistency_steps + indirectness_steps + imprecision_steps + pub_steps
  final_score <- max(1, min(4, base_score - total_downgrades))
  label <- switch(as.character(final_score),
    "1" = "\u2295\u25EF\u25EF\u25EF Very low",
    "2" = "\u2295\u2295\u25EF\u25EF Low",
    "3" = "\u2295\u2295\u2295\u25EF Moderate",
    "4" = "\u2295\u2295\u2295\u2295 High"
  )
  list(score = final_score, label = label)
}

#* @parser json
#* @serializer unboxedJSON
#* @post /api/grade/evaluate
function(req, res) {
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)

    outcome <- body$outcome
    effect <- body$effect
    k <- as.numeric(body$k)
    n <- as.numeric(body$n)
    i2 <- as.numeric(body$i2)
    rob <- body$risk_of_bias
    pub_bias <- body$publication_bias
    study_design <- body$study_design
    indirectness_override <- if (!is.null(body$indirectness_override)) body$indirectness_override else NULL

    inc <- grade_assess_inconsistency(i2)
    ind <- grade_assess_indirectness(indirectness_override)
    imp <- grade_assess_imprecision(n, ois_threshold = 300)
    cert <- grade_calculate_certainty(study_design, rob, inc$steps, ind$steps, imp$steps, pub_bias)

    res$status <- 200
    list(
      status = "success",
      row = list(
        outcome = outcome,
        effect_ci = effect,
        k = k,
        n = n,
        risk_of_bias = rob,
        inconsistency = inc$rating,
        indirectness = ind$rating,
        imprecision = imp$rating,
        publication_bias = pub_bias,
        certainty = cert$label
      )
    )
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] GRADE-EVALUATE CAUGHT ERROR: ", msg,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = msg)
  })
}
