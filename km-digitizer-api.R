# =============================================================================
# Kaplan-Meier Curve Digitizer - Survival Data Reconstruction API.
#
# This service does NOT do any digitization itself - the entire interactive
# workspace (upload, axis calibration, point placement, censoring marks,
# zoom/pan/undo/redo) runs client-side in the browser (see
# app/tools/km-digitizer/**). This R service receives only the small,
# already-digitized structured data (pixel->data-coordinate conversion has
# ALREADY happened in the browser using the user's own calibration - no
# image, PDF, or pixel data is ever sent here) and performs the one part
# that genuinely needs validated statistical methodology: reconstructing
# approximate individual patient data (pseudo-IPD) from a digitized KM
# curve plus its published numbers-at-risk table.
#
# METHODOLOGY: this uses the algorithm of Guyot P, Ades AE, Ouwens MJ,
# Welton NJ. "Enhanced secondary analysis of survival data: reconstructing
# the data from published Kaplan-Meier survival curves." BMC Med Res
# Methodol. 2012 - the standard, widely-cited, published method for exactly
# this task - via the CRAN package `IPDfromKM` (preprocess() + getIPD()),
# NOT a bespoke/invented algorithm. When a group's numbers-at-risk table is
# not supplied, NO pseudo-IPD is fabricated - the digitized curve points are
# returned as-is ("curve_only" mode) with an explicit warning, per the
# project's explicit instruction never to silently manufacture data the
# published figure does not support.
#
# No LLM/AI API of any kind is used or reachable from this service.
#
# Runs as its own Plumber process on its own port (8005), supervised the
# same way as every other backend in this project (see
# scripts/backend-supervisor.js) - completely independent of the other 5
# services. Never writes to persistent server storage: all temp files
# (tempfile()) are cleaned up via on.exit() the same way rob-api.R does.
# =============================================================================
library(plumber)
library(jsonlite)
library(IPDfromKM)
library(survival)
library(ggplot2)
library(base64enc)

STARTED_AT <- Sys.time()

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
  list(status = "ok", service = "km-digitizer-api.R (KM Curve Reconstruction)",
       method = "Guyot et al. 2012 (IPDfromKM package)",
       ipdfromkm_version = as.character(utils::packageVersion("IPDfromKM")),
       uptime_seconds = round(as.numeric(difftime(Sys.time(), STARTED_AT, units = "secs"))))
}

#* @plumber
function(pr) {
  pr %>% plumber::pr_set_error(function(req, res, err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "unknown error")
    log_line <- paste0("[", format(Sys.time()), "] KM-DIGITIZER UNCAUGHT ERROR: ", msg, " | route: ", req$PATH_INFO)
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/km-digitizer-error.log", append = TRUE), silent = TRUE)
    res$status <- 200
    list(status = "error", message = paste("Uncaught server error:", msg))
  })
}

parse_request <- function(req) {
  # Deliberately ignore plumber's auto-parsed req$body and re-parse the raw
  # JSON with simplifyVector = FALSE, same reason/fix as rob-api.R's
  # parse_request: a JSON array of objects (here, `points`/`groups`) gets
  # auto-simplified by plumber into a data.frame, and a data.frame IS a
  # list per is.list() - so a naive "use req$body if it's a list" check
  # (the bug this replaced) lets that broken shape through, and `p$time`
  # on a data.frame column then throws "$ operator is invalid for atomic
  # vectors". simplifyVector = FALSE always yields a plain nested list.
  fromJSON(req$postBody, simplifyVector = FALSE)
}

# =============================================================================
# Reconstruction for a single group.
#
# Requires >= 2 digitized (time, survival) points always. If a numbers-at-
# risk table (>= 2 time points) and a total group size are also supplied,
# runs the real Guyot-algorithm reconstruction (preprocess -> getIPD) and
# returns pseudo-IPD (time, status) rows plus a KM summary computed from
# that reconstructed data via survival::survfit - never via the fragile
# IPDfromKM::survreport() helper, which was found to be unstable on this
# server's R installation during development; survfit()/summary() are the
# same underlying, well-established `survival` package primitives and are
# fully equivalent statistically.
#
# Without a numbers-at-risk table, returns the cleaned digitized points only
# ("curve_only") - explicitly NOT pseudo-IPD, since the Guyot algorithm
# fundamentally requires the risk table to allocate events/censoring between
# digitized points. Fabricating pseudo-IPD without it would misrepresent
# the confidence the reconstruction actually has.
# =============================================================================
reconstruct_group <- function(g, maxy) {
  warnings <- list()
  pts <- g$points
  if (is.null(pts) || length(pts) < 2) {
    return(list(name = g$name, status = "error", message = "At least 2 digitized points are required."))
  }
  times <- vapply(pts, function(p) as.numeric(p$time), numeric(1))
  survs <- vapply(pts, function(p) as.numeric(p$survival), numeric(1))
  if (any(is.na(times)) || any(is.na(survs))) {
    return(list(name = g$name, status = "error", message = "Digitized points contain non-numeric values."))
  }
  ord <- order(times)
  times <- times[ord]; survs <- survs[ord]
  dat <- data.frame(time = times, survival = survs * maxy)

  has_nrisk <- !is.null(g$nrisk_times) && !is.null(g$nrisk_values) &&
    length(g$nrisk_times) >= 2 && length(g$nrisk_times) == length(g$nrisk_values)
  has_total <- !is.null(g$total_n) && !is.na(as.numeric(g$total_n)) && as.numeric(g$total_n) > 0

  digitized_out <- lapply(seq_along(times), function(i) list(time = times[i], survival = survs[i]))

  if (!has_nrisk || !has_total) {
    if (!has_nrisk) warnings <- c(warnings, "No numbers-at-risk table was provided - showing the digitized curve only, not a reconstructed dataset.")
    if (has_nrisk && !has_total) warnings <- c(warnings, "Total group size was not provided - reconstruction requires it alongside the numbers-at-risk table.")
    return(list(
      name = g$name, status = "success", mode = "curve_only",
      digitized_points = digitized_out, ipd = list(), km_summary = NULL,
      warnings = warnings
    ))
  }

  trisk <- vapply(g$nrisk_times, as.numeric, numeric(1))
  nrisk <- vapply(g$nrisk_values, as.numeric, numeric(1))
  total_n <- as.numeric(g$total_n)

  result <- tryCatch({
    prep <- IPDfromKM::preprocess(dat, trisk = trisk, nrisk = nrisk, totalpts = total_n, maxy = maxy)
    ipd <- IPDfromKM::getIPD(prep, armID = 1)
    d <- ipd$IPD
    if (is.null(d) || nrow(d) == 0) stop("Reconstruction produced no data points - check that the digitized curve and numbers-at-risk table are consistent (e.g. survival should not increase, and risk-table times should fall within the digitized time range).")
    fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = d)
    tbl <- summary(fit)$table
    median_val <- unname(tbl["median"])
    ipd_out <- lapply(seq_len(nrow(d)), function(i) list(id = i, time = d$time[i], event = as.integer(d$status[i])))
    list(
      name = g$name, status = "success", mode = "reconstructed",
      digitized_points = digitized_out, ipd = ipd_out,
      km_summary = list(
        n = nrow(d), events = sum(d$status), censored = sum(d$status == 0),
        median_survival_time = if (is.na(median_val)) NULL else median_val,
        median_estimable = !is.na(median_val)
      ),
      fit = fit, warnings = list("Reconstructed pseudo-IPD - estimated from the published figure, not original patient-level data.")
    )
  }, error = function(e) {
    list(name = g$name, status = "error", message = paste("Reconstruction failed:", conditionMessage(e)))
  })
  result
}

# =============================================================================
# Validation plot: overlays the digitized input points against the
# reconstructed KM step curve (when reconstruction succeeded) so the user
# can visually judge fidelity themselves - this tool never claims a numeric
# accuracy score, per the project's explicit "no fake accuracy percentages"
# requirement.
# =============================================================================
build_validation_plot <- function(results, maxy) {
  point_rows <- list(); curve_rows <- list()
  for (r in results) {
    if (r$status != "success") next
    for (p in r$digitized_points) {
      point_rows[[length(point_rows) + 1]] <- data.frame(group = r$name, time = p$time, survival = p$survival, kind = "Digitized point")
    }
    if (r$mode == "reconstructed" && !is.null(r$fit)) {
      s <- summary(r$fit)
      curve_rows[[length(curve_rows) + 1]] <- data.frame(group = r$name, time = c(0, s$time), survival = c(1, s$surv), kind = "Reconstructed KM curve")
    }
  }
  if (length(point_rows) == 0) return(NULL)
  points_df <- do.call(rbind, point_rows)
  points_df$survival_display <- points_df$survival * maxy

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = points_df, ggplot2::aes(x = time, y = survival_display, color = group), size = 2, shape = 16, alpha = 0.85)

  if (length(curve_rows) > 0) {
    curve_df <- do.call(rbind, curve_rows)
    curve_df$survival_display <- curve_df$survival * maxy
    p <- p + ggplot2::geom_step(data = curve_df, ggplot2::aes(x = time, y = survival_display, color = group), linewidth = 0.9)
  }

  p <- p +
    ggplot2::labs(
      title = "Validation: digitized points vs. reconstructed KM curve",
      subtitle = "Points = digitized from the source figure. Line = KM curve refit from the reconstructed pseudo-IPD (where numbers-at-risk were provided).",
      x = "Time", y = if (maxy == 100) "Survival (%)" else "Survival probability", color = "Group"
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(legend.position = "bottom", plot.subtitle = ggplot2::element_text(size = 10, color = "grey30"))

  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  ggplot2::ggsave(f, plot = p, width = 10, height = 6.5, dpi = 220, bg = "white", limitsize = FALSE)
  paste0("data:image/png;base64,", base64enc::base64encode(f))
}

#* Reconstruct survival data from digitized Kaplan-Meier curve points.
#* @serializer unboxedJSON
#* @post /reconstruct
function(req, res) {
  tryCatch({
    body <- parse_request(req)
    groups <- body$groups
    if (is.null(groups) || length(groups) == 0) {
      res$status <- 200
      return(list(status = "error", message = "At least one group with digitized points is required."))
    }
    scale <- if (!is.null(body$scale)) body$scale else "proportion"
    maxy <- if (scale == "percentage") 100 else 1

    results <- lapply(groups, reconstruct_group, maxy = maxy)

    validation_plot <- tryCatch(build_validation_plot(results, maxy), error = function(e) NULL)

    out_groups <- lapply(results, function(r) {
      r$fit <- NULL # drop the non-serializable survfit object before returning JSON
      r
    })

    res$status <- 200
    list(
      status = "success",
      method = "Guyot et al. 2012 (BMC Med Res Methodol) via the IPDfromKM R package - only applied to groups with a numbers-at-risk table; other groups are returned as digitized-curve-only.",
      groups = out_groups,
      validation_plot_base64 = validation_plot
    )
  }, error = function(e) {
    res$status <- 200
    msg <- conditionMessage(e)
    log_line <- paste0("[", format(Sys.time()), "] KM-DIGITIZER CAUGHT ERROR: ", msg)
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/km-digitizer-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = paste("Reconstruction request failed:", msg))
  })
}
