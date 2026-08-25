library(plumber)
library(jsonlite)
library(mada)
library(metafor)
library(base64enc)

# Diagnostic Test Accuracy Meta-Analysis backend. Dedicated Plumber process
# on its own port (8006), entirely separate from every other service in
# this repo - same one-script-per-tool convention as tsa-api.R/metareg-api.R.
#
# METHODOLOGY (documented here so the exact R functions/formulas used are
# traceable without reading vignettes elsewhere):
#
#   - Study-level sensitivity/specificity/PPV/NPV: plain arithmetic on the
#     raw TP/FP/FN/TN (never altered), with exact Clopper-Pearson binomial
#     confidence intervals via stats::binom.test() - the standard exact
#     method for a single proportion, not a mada-internal approximation.
#   - Study-level DOR and its CI: the standard log-scale formula
#     (Deeks 2001 / Glas et al. 2003): logDOR = log((TP*TN)/(FP*FN)),
#     SE = sqrt(1/TP + 1/FP + 1/FN + 1/TN).
#   - Study-level LR+ / LR- and their CIs: the standard log-scale formulas
#     (Simel et al. 1991): SE[log(LR+)] = sqrt(1/TP - 1/(TP+FN) + 1/FP -
#     1/(FP+TN)); SE[log(LR-)] = sqrt(1/FN - 1/(TP+FN) + 1/TN - 1/(FP+TN)).
#   - POOLED DOR / LR+ / LR-: univariate random-effects meta-analysis of
#     the log-scale study estimates above via metafor::rma() (REML) - the
#     same, already-proven-in-this-codebase pooling function used by
#     api.R/tsa-api.R/metareg-api.R. This is statistically valid because
#     DOR/LR+/LR- are each a SINGLE summary measure per study - unlike
#     sensitivity and specificity, which are correlated and must NOT be
#     pooled independently (see below).
#   - POOLED sensitivity / specificity: the BIVARIATE random-effects model
#     of Reitsma et al. (2005), fitted via mada::reitsma() (which wraps
#     mvmeta::mvmeta on the logit-transformed sensitivity and false
#     positive rate, jointly modeling their correlation). Pooled values and
#     Wald confidence intervals are computed from this fit's own coef()/
#     vcov() via the logit back-transform + delta method - the same
#     computation reitsma's own summary/print methods perform internally,
#     done explicitly here so the exact source of every number is visible.
#   - SROC curve: mada::SROC() / mada:::plot.reitsma(), which draws the
#     summary ROC curve implied by the FITTED BIVARIATE MODEL above (not a
#     separately-fitted curve). A summary confidence region comes from
#     mada::ROCellipse(). mada does not provide a genuine Rutter-Gatsonis
#     HSROC regression as a distinct model from the bivariate model - this
#     tool therefore labels this curve "SROC curve (from the bivariate
#     model)" and does NOT claim a separate HSROC model was fit, per the
#     documented equivalence between the two parametrizations for the
#     no-covariate case (Harbord et al. 2007, Stat Med).
#   - Continuity correction: mada's default (correction=0.5,
#     correction.control="all") adds 0.5 to EVERY study's cells whenever
#     ANY study has a zero cell. This tool instead uses
#     correction.control="single" - 0.5 is added only to the cells of the
#     study/studies that actually have a zero, leaving every other study's
#     computation untouched. This is reported explicitly in the response
#     (continuity_correction block) and the raw uploaded TP/FP/FN/TN values
#     are never altered - correction is applied only inside reitsma()'s own
#     model fit, not to the study-level table shown to the user.

STARTED_AT <- Sys.time()
MADA_VERSION <- as.character(utils::packageVersion("mada"))
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

#* Liveness probe.
#* @serializer unboxedJSON
#* @get /health
function() {
  list(status = "ok", service = "dta-api.R (Diagnostic Test Accuracy Meta-Analysis)",
       package = "mada", package_version = MADA_VERSION,
       uptime_seconds = round(as.numeric(difftime(Sys.time(), STARTED_AT, units = "secs"))))
}

#* @plumber
function(pr) {
  pr %>% plumber::pr_set_error(function(req, res, err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "unknown error")
    log_line <- paste0("[", format(Sys.time()), "] UNCAUGHT ERROR: ", msg, " | route: ", req$PATH_INFO)
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/dta-error.log", append = TRUE), silent = TRUE)
    res$status <- 200
    list(status = "error", message = paste("Uncaught server error:", msg))
  })
}

safe_num <- function(x, digits = NULL) {
  v <- tryCatch(suppressWarnings(as.numeric(x)), error = function(e) NA)
  if (length(v) == 0 || is.na(v) || is.infinite(v)) return(NA)
  if (!is.null(digits)) v <- round(v, digits)
  v
}

log_ext_error <- function(tag, e) {
  msg <- tryCatch(conditionMessage(e), error = function(e2) "unknown error")
  try(cat(paste0("[", format(Sys.time()), "] ", tag, ": ", msg, "\n"), file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/dta-error.log", append = TRUE), silent = TRUE)
  msg
}

# --- Exact Clopper-Pearson CI for a single proportion (used for study-level
# sensitivity/specificity/PPV/NPV) --------------------------------------------
prop_ci <- function(x, n, ci_level) {
  if (is.na(x) || is.na(n) || n <= 0) return(c(NA, NA, NA))
  est <- x / n
  if (x == 0 && n == 0) return(c(NA, NA, NA))
  bt <- tryCatch(stats::binom.test(x, n, conf.level = ci_level)$conf.int, error = function(e) c(NA, NA))
  c(est, safe_num(bt[1], 4), safe_num(bt[2], 4))
}

# --- Study-level DOR / LR+ / LR- with standard log-scale CIs -----------------
# (Deeks 2001 for DOR; Simel et al. 1991 for LR+/LR-). Returns NA (not zero)
# when a required cell is zero and the log-scale computation is undefined -
# these NAs are surfaced to the user, never silently substituted.
study_dor <- function(tp, fp, fn, tn, z) {
  if (any(c(tp, fp, fn, tn) == 0)) return(c(NA, NA, NA))
  logDOR <- log((tp * tn) / (fp * fn))
  se <- sqrt(1/tp + 1/fp + 1/fn + 1/tn)
  c(exp(logDOR), exp(logDOR - z * se), exp(logDOR + z * se))
}
study_lrpos <- function(tp, fp, fn, tn, z) {
  if (fp == 0 || tp == 0) return(c(NA, NA, NA))
  sens <- tp / (tp + fn); spec <- tn / (tn + fp)
  if (spec >= 1) return(c(NA, NA, NA))
  lr <- sens / (1 - spec)
  se <- sqrt(1/tp - 1/(tp + fn) + 1/fp - 1/(fp + tn))
  c(lr, exp(log(lr) - z * se), exp(log(lr) + z * se))
}
study_lrneg <- function(tp, fp, fn, tn, z) {
  if (fn == 0 || tn == 0) return(c(NA, NA, NA))
  sens <- tp / (tp + fn); spec <- tn / (tn + fp)
  if (sens >= 1) return(c(NA, NA, NA))
  lr <- (1 - sens) / spec
  se <- sqrt(1/fn - 1/(tp + fn) + 1/tn - 1/(fp + tn))
  c(lr, exp(log(lr) - z * se), exp(log(lr) + z * se))
}

# --- A small, self-drawn forest plot (same base-R technique already used by
# metareg-api.R's coefficient plots) - used for sensitivity/specificity/DOR
# forest plots so this endpoint isn't dependent on mada's own plotting
# function signatures, while still being real, high-resolution R graphics. -
build_forest_png <- function(labels, est, lo, hi, pooled, pooled_lo, pooled_hi, xlab, title, log_scale = FALSE, refline = NULL) {
  plot_file <- tempfile(fileext = ".png")
  n <- length(labels)
  has_pooled <- !is.na(pooled)
  total_rows <- n + if (has_pooled) 2 else 0 # +1 blank spacer, +1 pooled row
  max_label_chars <- max(nchar(labels), na.rm = TRUE)
  fig_height <- max(900, 260 + total_rows * 85)
  png(plot_file, width = 2400, height = fig_height, res = 220)
  tryCatch({
    par(cex.main = 0.95, mai = c(0.9, max(1.4, max_label_chars * 0.09), 0.75, 0.4))
    all_est <- c(est, if (has_pooled) pooled else NULL)
    all_lo <- c(lo, if (has_pooled) pooled_lo else NULL)
    all_hi <- c(hi, if (has_pooled) pooled_hi else NULL)
    xlim <- range(c(all_lo, all_hi, refline), na.rm = TRUE)
    xlim <- xlim + c(-1, 1) * diff(xlim) * 0.1
    y_study <- rev(seq_len(n)) + if (has_pooled) 2 else 0
    plot(NA, xlim = xlim, ylim = c(0.5, total_rows + 0.5), yaxt = "n", ylab = "",
         xlab = xlab, main = title, log = if (log_scale) "x" else "")
    axis(2, at = y_study, labels = labels, las = 1, cex.axis = 0.8)
    points(est, y_study, pch = 15, cex = 1.3, col = "#3730a3")
    segments(lo, y_study, hi, y_study, col = "#3730a3", lwd = 1.8)
    if (!is.null(refline)) abline(v = refline, lty = 2, col = "#94a3b8")
    if (has_pooled) {
      axis(2, at = 1, labels = "Pooled (random-effects)", las = 1, cex.axis = 0.8, font = 2)
      points(pooled, 1, pch = 18, cex = 2.2, col = "#c026d3")
      segments(pooled_lo, 1, pooled_hi, 1, col = "#c026d3", lwd = 2.2)
    }
  }, error = function(e) { plot.new(); text(0.5, 0.5, paste("Figure could not be generated:", conditionMessage(e))) })
  dev.off()
  paste0("data:image/png;base64,", base64encode(plot_file))
}

#* @parser json
#* @serializer unboxedJSON
#* @post /api/dta/analyze
function(req, res) {
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)
    studies_in <- body$studies
    conf <- if (!is.null(body$config)) body$config else list()
    ci_level <- if (!is.null(conf$ci_level)) as.numeric(conf$ci_level) / 100 else 0.95
    if (is.na(ci_level) || ci_level <= 0 || ci_level >= 1) ci_level <- 0.95
    z <- qnorm(1 - (1 - ci_level) / 2)

    if (is.null(studies_in) || length(studies_in) == 0) stop("No studies were provided.")
    df_raw <- as.data.frame(studies_in, stringsAsFactors = FALSE)
    required <- c("study", "tp", "fp", "fn", "tn")
    missing_cols <- required[!(required %in% names(df_raw))]
    if (length(missing_cols) > 0) stop(paste0("Required field(s) missing from the uploaded data: ", paste(missing_cols, collapse = ", "), "."))

    n_rows <- nrow(df_raw)
    tp <- suppressWarnings(as.numeric(df_raw$tp)); fp <- suppressWarnings(as.numeric(df_raw$fp))
    fn <- suppressWarnings(as.numeric(df_raw$fn)); tn <- suppressWarnings(as.numeric(df_raw$tn))
    study_ids <- as.character(df_raw$study)

    valid <- !is.na(tp) & !is.na(fp) & !is.na(fn) & !is.na(tn) &
      tp >= 0 & fp >= 0 & fn >= 0 & tn >= 0 &
      tp == floor(tp) & fp == floor(fp) & fn == floor(fn) & tn == floor(tn) &
      (tp + fp + fn + tn) > 0
    # This endpoint re-validates independently of the frontend (the frontend
    # already excludes malformed rows before sending, but this backend must
    # stay safe even if called directly) - never trusts the client blindly.
    included_idx <- which(valid)
    excluded_idx <- which(!valid)

    if (length(included_idx) < 2) stop(paste0("Diagnostic meta-analysis could not be performed because only ", length(included_idx), " study(ies) have complete, valid TP/FP/FN/TN data. At least 2 are required."))

    tp_i <- tp[included_idx]; fp_i <- fp[included_idx]; fn_i <- fn[included_idx]; tn_i <- tn[included_idx]
    ids_i <- study_ids[included_idx]
    total_i <- tp_i + fp_i + fn_i + tn_i

    # --- Study-level results (transparent, direct arithmetic - never via a
    # package's opaque internals) --------------------------------------------
    study_results <- lapply(seq_along(ids_i), function(i) {
      sens <- prop_ci(tp_i[i], tp_i[i] + fn_i[i], ci_level)
      spec <- prop_ci(tn_i[i], tn_i[i] + fp_i[i], ci_level)
      ppv <- prop_ci(tp_i[i], tp_i[i] + fp_i[i], ci_level)
      npv <- prop_ci(tn_i[i], tn_i[i] + fn_i[i], ci_level)
      dor <- study_dor(tp_i[i], fp_i[i], fn_i[i], tn_i[i], z)
      lrp <- study_lrpos(tp_i[i], fp_i[i], fn_i[i], tn_i[i], z)
      lrn <- study_lrneg(tp_i[i], fp_i[i], fn_i[i], tn_i[i], z)
      list(
        study = ids_i[i], tp = tp_i[i], fp = fp_i[i], fn = fn_i[i], tn = tn_i[i], total = total_i[i],
        sensitivity = safe_num(sens[1], 4), sensitivity_ci_lower = safe_num(sens[2]), sensitivity_ci_upper = safe_num(sens[3]),
        specificity = safe_num(spec[1], 4), specificity_ci_lower = safe_num(spec[2]), specificity_ci_upper = safe_num(spec[3]),
        ppv = safe_num(ppv[1], 4), npv = safe_num(npv[1], 4),
        dor = safe_num(dor[1], 3), dor_ci_lower = safe_num(dor[2], 3), dor_ci_upper = safe_num(dor[3], 3),
        lr_pos = safe_num(lrp[1], 3), lr_pos_ci_lower = safe_num(lrp[2], 3), lr_pos_ci_upper = safe_num(lrp[3], 3),
        lr_neg = safe_num(lrn[1], 3), lr_neg_ci_lower = safe_num(lrn[2], 3), lr_neg_ci_upper = safe_num(lrn[3], 3),
        has_zero_cell = any(c(tp_i[i], fp_i[i], fn_i[i], tn_i[i]) == 0)
      )
    })
    zero_cell_studies <- ids_i[tp_i == 0 | fp_i == 0 | fn_i == 0 | tn_i == 0]

    excluded_studies <- if (length(excluded_idx) > 0) lapply(excluded_idx, function(i) {
      reasons <- c()
      if (is.na(tp[i])) reasons <- c(reasons, "TP missing/invalid")
      if (is.na(fp[i])) reasons <- c(reasons, "FP missing/invalid")
      if (is.na(fn[i])) reasons <- c(reasons, "FN missing/invalid")
      if (is.na(tn[i])) reasons <- c(reasons, "TN missing/invalid")
      if (!is.na(tp[i]) && (tp[i] < 0 || tp[i] != floor(tp[i]))) reasons <- c(reasons, "TP is not a non-negative integer")
      if (!is.na(fp[i]) && (fp[i] < 0 || fp[i] != floor(fp[i]))) reasons <- c(reasons, "FP is not a non-negative integer")
      if (!is.na(fn[i]) && (fn[i] < 0 || fn[i] != floor(fn[i]))) reasons <- c(reasons, "FN is not a non-negative integer")
      if (!is.na(tn[i]) && (tn[i] < 0 || tn[i] != floor(tn[i]))) reasons <- c(reasons, "TN is not a non-negative integer")
      if (length(reasons) == 0) reasons <- c("Total sample size is 0")
      list(study = study_ids[i], reason = paste(reasons, collapse = "; "))
    }) else list()

    # --- Univariate pooling of DOR / LR+ / LR- (metafor::rma, REML) --------
    pool_log_measure <- function(est, lo, hi) {
      ok <- !is.na(est) & !is.na(lo) & !is.na(hi) & est > 0
      if (sum(ok) < 2) return(list(available = FALSE, note = "Fewer than 2 studies have a computable value (a zero cell makes the log-scale estimate undefined for the rest) - pooled estimate not available."))
      logy <- log(est[ok]); se <- (log(hi[ok]) - log(lo[ok])) / (2 * z)
      m <- tryCatch(metafor::rma(yi = logy, sei = se, method = "REML", level = ci_level * 100), error = function(e) NULL)
      if (is.null(m)) return(list(available = FALSE, note = "Random-effects model failed to converge for this measure."))
      list(available = TRUE, estimate = safe_num(exp(m$b[1]), 3), ci_lower = safe_num(exp(m$ci.lb), 3), ci_upper = safe_num(exp(m$ci.ub), 3),
           tau2 = safe_num(m$tau2, 4), i2 = safe_num(m$I2, 1), k = m$k, note = NA)
    }
    dor_v <- vapply(study_results, function(s) s$dor, numeric(1)); dor_lo <- vapply(study_results, function(s) s$dor_ci_lower, numeric(1)); dor_hi <- vapply(study_results, function(s) s$dor_ci_upper, numeric(1))
    lrp_v <- vapply(study_results, function(s) s$lr_pos, numeric(1)); lrp_lo <- vapply(study_results, function(s) s$lr_pos_ci_lower, numeric(1)); lrp_hi <- vapply(study_results, function(s) s$lr_pos_ci_upper, numeric(1))
    lrn_v <- vapply(study_results, function(s) s$lr_neg, numeric(1)); lrn_lo <- vapply(study_results, function(s) s$lr_neg_ci_lower, numeric(1)); lrn_hi <- vapply(study_results, function(s) s$lr_neg_ci_upper, numeric(1))
    pooled_dor <- pool_log_measure(dor_v, dor_lo, dor_hi)
    pooled_lrpos <- pool_log_measure(lrp_v, lrp_lo, lrp_hi)
    pooled_lrneg <- pool_log_measure(lrn_v, lrn_lo, lrn_hi)

    # --- Bivariate model (mada::reitsma) - the ONLY valid source of pooled
    # sensitivity/specificity, since they are jointly modeled here, not
    # pooled independently. ----------------------------------------------
    mada_df <- data.frame(TP = tp_i, FN = fn_i, FP = fp_i, TN = tn_i)
    bivariate <- list(available = FALSE, note = NA)
    fit <- NULL
    if (length(ids_i) >= 4) {
      fit <- tryCatch(mada::reitsma(mada_df, correction = 0.5, correction.control = "single"), error = function(e) { bivariate$note <<- log_ext_error("REITSMA", e); NULL })
    } else {
      bivariate$note <- paste0("The bivariate model requires at least 4 studies to reliably estimate the between-study correlation of sensitivity and specificity; only ", length(ids_i), " eligible study(ies) were provided.")
    }
    sroc <- list(available = FALSE, note = bivariate$note)
    if (!is.null(fit)) {
      b <- tryCatch(coef(fit), error = function(e) NULL)
      V <- tryCatch(vcov(fit), error = function(e) NULL)
      if (!is.null(b) && !is.null(V) && length(b) >= 2) {
        logit_inv <- function(x) 1 / (1 + exp(-x))
        sens_b <- b[1]; fpr_b <- b[2]
        sens_se <- sqrt(V[1, 1]); fpr_se <- sqrt(V[2, 2])
        corr <- if (V[1, 1] > 0 && V[2, 2] > 0) V[1, 2] / sqrt(V[1, 1] * V[2, 2]) else NA
        sens_est <- logit_inv(sens_b); sens_lo <- logit_inv(sens_b - z * sens_se); sens_hi <- logit_inv(sens_b + z * sens_se)
        fpr_est <- logit_inv(fpr_b); fpr_lo <- logit_inv(fpr_b - z * fpr_se); fpr_hi <- logit_inv(fpr_b + z * fpr_se)
        # between-study SD on the logit scale, when the fit exposes it (mvmeta random-effects Psi)
        tau_sens <- tryCatch(safe_num(sqrt(fit$Psi[1, 1]), 4), error = function(e) NA)
        tau_fpr <- tryCatch(safe_num(sqrt(fit$Psi[2, 2]), 4), error = function(e) NA)
        bivariate <- list(
          available = TRUE, n_studies = length(ids_i), n_participants = sum(total_i),
          pooled_sensitivity = safe_num(sens_est, 4), pooled_sensitivity_ci_lower = safe_num(sens_lo, 4), pooled_sensitivity_ci_upper = safe_num(sens_hi, 4),
          pooled_specificity = safe_num(1 - fpr_est, 4), pooled_specificity_ci_lower = safe_num(1 - fpr_hi, 4), pooled_specificity_ci_upper = safe_num(1 - fpr_lo, 4),
          correlation = safe_num(corr, 3), tau_sens_logit = tau_sens, tau_fpr_logit = tau_fpr, note = NA
        )
        # --- SROC plot: real mada plotting functions, from the SAME fit. ---
        plot_file <- tempfile(fileext = ".png")
        conf_region_ok <- FALSE
        pred_region_ok <- FALSE
        png(plot_file, width = 2200, height = 2200, res = 220)
        tryCatch({
          plot(fit, sroclwd = 2, main = "SROC curve (bivariate model)", xlim = c(0, 1), ylim = c(0, 1))
          points(fpr(mada_df), sens(mada_df), pch = 1, col = "#3730a3", cex = 1.1)
          points(fpr_est, sens_est, pch = 18, cex = 2, col = "#c026d3")
          tryCatch({ mada::ROCellipse(fit, add = TRUE, col = "#c026d3"); conf_region_ok <<- TRUE }, error = function(e) NULL)
          tryCatch({ mada::ROCellipse(fit, add = TRUE, predict = TRUE, lty = 2, col = "#94a3b8"); pred_region_ok <<- TRUE }, error = function(e) NULL)
          legend("bottomright", legend = c("Study", "Summary point", if (conf_region_ok) "95% confidence region" else NULL, if (pred_region_ok) "95% prediction region" else NULL),
                 pch = c(1, 18, if (conf_region_ok) NA else NULL, if (pred_region_ok) NA else NULL),
                 lty = c(NA, NA, if (conf_region_ok) 1 else NULL, if (pred_region_ok) 2 else NULL),
                 col = c("#3730a3", "#c026d3", if (conf_region_ok) "#c026d3" else NULL, if (pred_region_ok) "#94a3b8" else NULL), bty = "n", cex = 0.85)
        }, error = function(e) { plot.new(); text(0.5, 0.5, paste("SROC figure could not be generated:", conditionMessage(e))) })
        dev.off()
        sroc <- list(available = TRUE, plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)),
                      confidence_region_available = conf_region_ok, prediction_region_available = pred_region_ok,
                      note = "SROC curve derived from the fitted bivariate model (mada::reitsma, Reitsma et al. 2005). This is presented under the established equivalence with the HSROC parametrization for the no-covariate case (Harbord et al. 2007) - the R package used does not implement a separately-fitted Rutter-Gatsonis HSROC regression, so this tool does not claim one.")
      } else {
        bivariate$note <- "The bivariate model fit but its coefficients/covariance matrix could not be extracted."
      }
    }

    # --- Forest plots (self-drawn, real R graphics - see build_forest_png) --
    labels <- vapply(study_results, function(s) s$study, character(1))
    sens_v <- vapply(study_results, function(s) s$sensitivity, numeric(1)); sens_lo_v <- vapply(study_results, function(s) s$sensitivity_ci_lower, numeric(1)); sens_hi_v <- vapply(study_results, function(s) s$sensitivity_ci_upper, numeric(1))
    spec_v <- vapply(study_results, function(s) s$specificity, numeric(1)); spec_lo_v <- vapply(study_results, function(s) s$specificity_ci_lower, numeric(1)); spec_hi_v <- vapply(study_results, function(s) s$specificity_ci_upper, numeric(1))
    forest_sens <- build_forest_png(labels, sens_v, sens_lo_v, sens_hi_v,
      if (bivariate$available) bivariate$pooled_sensitivity else NA, if (bivariate$available) bivariate$pooled_sensitivity_ci_lower else NA, if (bivariate$available) bivariate$pooled_sensitivity_ci_upper else NA,
      "Sensitivity", "Sensitivity Forest Plot")
    forest_spec <- build_forest_png(labels, spec_v, spec_lo_v, spec_hi_v,
      if (bivariate$available) bivariate$pooled_specificity else NA, if (bivariate$available) bivariate$pooled_specificity_ci_lower else NA, if (bivariate$available) bivariate$pooled_specificity_ci_upper else NA,
      "Specificity", "Specificity Forest Plot")
    forest_dor <- build_forest_png(labels, dor_v, dor_lo, dor_hi,
      if (isTRUE(pooled_dor$available)) pooled_dor$estimate else NA, if (isTRUE(pooled_dor$available)) pooled_dor$ci_lower else NA, if (isTRUE(pooled_dor$available)) pooled_dor$ci_upper else NA,
      "Diagnostic Odds Ratio (log scale)", "Diagnostic Odds Ratio Forest Plot", log_scale = TRUE, refline = 1)

    warnings_list <- list()
    if (length(zero_cell_studies) > 0) warnings_list[[length(warnings_list) + 1]] <- paste0(length(zero_cell_studies), " study(ies) contain at least one zero cell (", paste(zero_cell_studies, collapse = ", "), "); log-scale study-level DOR/LR are not computable for those studies and are reported as unavailable, and a continuity correction was applied only to those studies' cells within the bivariate model fit.")
    if (length(ids_i) < 4) warnings_list[[length(warnings_list) + 1]] <- "Fewer than 4 studies were included - between-study correlation and heterogeneity estimates are unstable with this few studies; interpret pooled results cautiously."
    warnings_list[[length(warnings_list) + 1]] <- "Diagnostic meta-analysis results should be interpreted in the context of study design, reference standards, thresholds, spectrum of disease, and risk of bias."

    interpretation <- paste0(
      "Bivariate random-effects model (Reitsma et al. 2005) fitted via mada::reitsma() (mada ", MADA_VERSION, "), jointly modeling the logit-transformed sensitivity and false positive rate across ", length(ids_i), " studies (", sum(total_i), " participants total). ",
      if (bivariate$available) paste0("Pooled sensitivity ", round(bivariate$pooled_sensitivity * 100, 1), "% (", round(bivariate$pooled_sensitivity_ci_lower * 100, 1), "-", round(bivariate$pooled_sensitivity_ci_upper * 100, 1), "%), pooled specificity ", round(bivariate$pooled_specificity * 100, 1), "% (", round(bivariate$pooled_specificity_ci_lower * 100, 1), "-", round(bivariate$pooled_specificity_ci_upper * 100, 1), "%). ") else paste0(bivariate$note, " "),
      "Diagnostic odds ratio and likelihood ratios were pooled separately via univariate random-effects meta-analysis (metafor::rma, REML) of their log-scale study estimates, since - unlike sensitivity and specificity - each is a single per-study summary measure and is not subject to the same correlation concern."
    )

    res$status <- 200
    list(
      status = "success",
      settings_used = list(
        bivariate_model = "Bivariate random-effects model (Reitsma et al. 2005)", bivariate_package = "mada", bivariate_package_version = MADA_VERSION, bivariate_r_function = "reitsma()",
        univariate_pooling_package = "metafor", univariate_pooling_package_version = METAFOR_VERSION, univariate_pooling_r_function = "rma() [REML]",
        ci_level = round(ci_level * 100, 1),
        continuity_correction = list(value = 0.5, scope = "single (applied only to the cells of studies with a zero cell, inside the bivariate model fit only - not the mada default of 'all')", studies_affected = as.list(zero_cell_studies))
      ),
      data_summary = list(n_studies_included = length(ids_i), n_studies_excluded = length(excluded_idx), total_participants = sum(total_i), excluded_studies = excluded_studies),
      study_results = study_results,
      univariate_pooled = list(dor = pooled_dor, lr_pos = pooled_lrpos, lr_neg = pooled_lrneg),
      bivariate = bivariate,
      sroc = sroc,
      forest_plots = list(sensitivity_base64 = forest_sens, specificity_base64 = forest_spec, dor_base64 = forest_dor),
      warnings = warnings_list,
      interpretation = interpretation
    )
  }, error = function(e) {
    res$status <- 200
    msg <- log_ext_error("DTA ANALYZE", e)
    list(status = "error", message = msg)
  })
}
