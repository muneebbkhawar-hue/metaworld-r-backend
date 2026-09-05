# =============================================================================
# Trial Sequential Analysis (TSA) API — completely separate Plumber process
# from api.R (Forest / Funnel / Sensitivity), running on its own port so it
# can never interfere with or be affected by those tools. Uses the RTSA
# package (https://cran.r-project.org/package=RTSA), a validated R
# implementation of Trial Sequential Analysis, rather than reimplementing the
# sequential-boundary mathematics by hand.
# =============================================================================
library(plumber)
library(jsonlite)
library(RTSA)
library(ggplot2)
library(ggrepel)
library(scales)
library(base64enc)

STARTED_AT <- Sys.time()

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

#* Liveness probe for the backend supervisor / frontend status banners.
#* @serializer unboxedJSON
#* @get /health
function() {
  list(status = "ok", service = "tsa-api.R (Trial Sequential Analysis)",
       uptime_seconds = round(as.numeric(difftime(Sys.time(), STARTED_AT, units = "secs"))))
}

#* @plumber
function(pr) {
  pr %>% plumber::pr_set_error(function(req, res, err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "unknown error")
    log_line <- paste0("[", format(Sys.time()), "] TSA UNCAUGHT ERROR: ", msg,
                        " | route: ", req$PATH_INFO,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/tsa-error.log", append = TRUE), silent = TRUE)
    res$status <- 200
    list(status = "error", message = paste("Uncaught server error:", msg))
  })
}

# es_alpha/es_beta spending function names actually supported and exposed
map_boundary <- function(x) {
  if (identical(x, "Pocock")) return("esPoc")
  return("esOF") # default / "OBrien-Fleming"
}

# Studies arrive from the frontend already in final analysis order (either
# from an explicit Analysis Order column or chronological Year order,
# resolved client-side per the tool's ordering rule) - this endpoint MUST
# NOT reorder them again. Row order in = row order used for the cumulative
# sequential analysis.
build_rtsa_data <- function(studies, outcome_type) {
  if (outcome_type == "continuous") {
    data.frame(
      study = as.character(studies$study),
      mI = as.numeric(studies$mean_e), sdI = as.numeric(studies$sd_e), nI = as.numeric(studies$n_e),
      mC = as.numeric(studies$mean_c), sdC = as.numeric(studies$sd_c), nC = as.numeric(studies$n_c),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      study = as.character(studies$study),
      eI = as.numeric(studies$event_e), nI = as.numeric(studies$n_e),
      eC = as.numeric(studies$event_c), nC = as.numeric(studies$n_c),
      stringsAsFactors = FALSE
    )
  }
}

safe_num <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  if (length(val) == 0 || is.null(val)) return(NULL)
  val[is.na(val) | is.infinite(val)] <- NA
  as.list(val)
}

#* @serializer unboxedJSON
#* @parser json
#* @post /api/tsa/analyze
function(req, res) {
  tryCatch({
    body <- req$body
    if (is.null(body)) body <- fromJSON(req$postBody)

    studies <- as.data.frame(body$studies)
    cfg <- body$config
    if (is.null(studies) || nrow(studies) < 2) stop("At least 2 studies are required for Trial Sequential Analysis.")

    outcome_type <- if (!is.null(cfg$outcome_type)) cfg$outcome_type else "dichotomous"
    sm <- if (!is.null(cfg$effect_measure)) cfg$effect_measure else if (outcome_type == "continuous") "MD" else "RR"
    fixed <- !is.null(cfg$model) && cfg$model == "Fixed-effect"
    side <- if (!is.null(cfg$side) && cfg$side == "One-sided") 1 else 2
    alpha_val <- if (!is.null(cfg$alpha)) as.numeric(cfg$alpha) else 0.05
    power_val <- if (!is.null(cfg$power)) as.numeric(cfg$power) else 90
    beta_val <- 1 - (power_val / 100)
    es_alpha <- map_boundary(cfg$boundary_type)
    futility <- if (!is.null(cfg$futility) && cfg$futility %in% c("non-binding", "binding")) cfg$futility else "none"
    es_beta <- if (futility != "none") es_alpha else NULL

    rtsa_data <- build_rtsa_data(studies, outcome_type)

    # --- Expected effect (mc) ---------------------------------------------
    expected_effect_mode <- if (!is.null(cfg$expected_effect_mode)) cfg$expected_effect_mode else "observed"
    used_observed_effect <- expected_effect_mode == "observed"
    mc_val <- NULL
    if (!used_observed_effect && !is.null(cfg$expected_effect_value)) {
      mc_val <- as.numeric(cfg$expected_effect_value)
    } else {
      # Derive the observed pooled effect on the NATURAL scale using the
      # same meta-analysis machinery (package `meta`) the other tools use,
      # so the frontend never has to send/compute a log-scale effect.
      suppressWarnings(suppressMessages(library(meta)))
      if (outcome_type == "continuous") {
        m0 <- meta::metacont(n.e = rtsa_data$nI, mean.e = rtsa_data$mI, sd.e = rtsa_data$sdI,
                              n.c = rtsa_data$nC, mean.c = rtsa_data$mC, sd.c = rtsa_data$sdC,
                              studlab = rtsa_data$study, sm = "MD", common = fixed, random = !fixed)
        te <- if (fixed) m0$TE.common else m0$TE.random
        mc_val <- as.numeric(te)
      } else {
        m0 <- meta::metabin(event.e = rtsa_data$eI, n.e = rtsa_data$nI, event.c = rtsa_data$eC, n.c = rtsa_data$nC,
                             studlab = rtsa_data$study, sm = sm, method = "MH", common = fixed, random = !fixed)
        te <- if (fixed) m0$TE.common else m0$TE.random
        mc_val <- if (sm == "RD") as.numeric(te) else exp(as.numeric(te))
      }
    }

    # sd_mc (continuous only): anticipated SD for the sample size
    # calculation. Derived from the pooled control-group SD across studies
    # (a standard, transparent convention) rather than fabricated.
    sd_mc_val <- if (outcome_type == "continuous") {
      as.numeric(sqrt(sum((rtsa_data$sdC^2) * (rtsa_data$nC - 1)) / sum(rtsa_data$nC - 1)))
    } else NULL

    # --- Control event risk (dichotomous only) -----------------------------
    pC_val <- NULL
    control_risk_mode <- if (!is.null(cfg$control_risk_mode)) cfg$control_risk_mode else "auto"
    if (outcome_type != "continuous") {
      pC_val <- if (control_risk_mode == "manual" && !is.null(cfg$control_risk_value)) {
        as.numeric(cfg$control_risk_value)
      } else {
        sum(rtsa_data$eC) / sum(rtsa_data$nC)
      }
    }

    # --- Heterogeneity adjustment for RIS -----------------------------------
    # RTSA's random_adj has no "none" option; a fixed-effect model has no
    # heterogeneity-adjustment concept at all, so this is only meaningful
    # (and only sent) for a random-effects model.
    het_adj <- if (!is.null(cfg$heterogeneity_adjustment)) cfg$heterogeneity_adjustment else "D2"
    if (!fixed && het_adj == "none") {
      stop("Heterogeneity adjustment cannot be 'none' for a random-effects TSA design - RTSA requires D2, I2, or tau2 to inflate the required information size. Choose one, or switch to the fixed-effect model.")
    }
    random_adj_val <- if (!fixed) het_adj else "tau2" # unused when fixed=TRUE, harmless default

    args <- list(
      type = "analysis", data = rtsa_data, outcome = sm, side = side,
      alpha = alpha_val, beta = beta_val, futility = futility,
      es_alpha = es_alpha, es_beta = es_beta, fixed = fixed,
      mc = mc_val, weights = "MH", random_adj = random_adj_val,
      study = rtsa_data$study
    )
    if (!is.null(sd_mc_val)) args$sd_mc <- sd_mc_val
    if (!is.null(pC_val)) args$pC <- pC_val

    result <- do.call(RTSA::RTSA, args)

    # --- Numerical results (extracted first so the custom plot below can
    # reuse them directly instead of re-deriving anything) ------------------
    rdf <- result$results$results_df
    k <- nrow(rtsa_data)
    # BUG FIX: results_df is NOT guaranteed to have exactly k rows in 1:1
    # order with the k studies - RTSA inserts an additional synthetic
    # interpolation row (with no real cumulative Z-value) at the point the
    # monitoring boundary/RIS is crossed, positioned by information
    # fraction, not appended at the end. The previous code always read rows
    # 1:k positionally, which silently substituted that synthetic row for
    # the LAST real study whenever RTSA inserted one before the final study
    # - e.g. with 3 real studies, this reads rows 1,2,3 while the true 3rd
    # study's cumulative Z actually landed in row 4 (row 3 being the
    # synthetic one, with z_fixed/z_random = NA). That NA silently dropped
    # the last study from the plotted Z-curve and study labels while AIS/
    # cum_n (computed independently from the raw input data, not from
    # results_df) still correctly included it - exactly the "only shows N-1
    # trials" symptom this was written to fix. The synthetic row is
    # reliably identifiable because it alone has no observed Z-value: a
    # real cumulative-analysis row always has one. Filter to rows with a
    # non-NA Z instead of trusting row position/count.
    z_field <- if (fixed) rdf$z_fixed else rdf$z_random
    study_rows <- which(!is.na(z_field))
    if (length(study_rows) != k) {
      stop(paste0(
        "Internal error reconciling RTSA's results with the ", k, " input studies: found ",
        length(study_rows), " result row(s) with an observed cumulative Z-value instead of ", k,
        ". This can happen with unusual boundary-crossing patterns - please report this dataset so it can be investigated rather than silently showing possibly-misaligned results."
      ))
    }
    z_col <- z_field[study_rows]
    upper_col <- rdf$upper[study_rows]
    lower_col <- rdf$lower[study_rows]
    fut_upper_col <- rdf$fut_upper[study_rows]
    fut_lower_col <- rdf$fut_lower[study_rows]
    cum_n <- cumsum(rtsa_data$nI + rtsa_data$nC)
    ais <- result$results$AIS
    # RTSA's own RIS field selection (bug fix): for a random-effects design,
    # the heterogeneity-adjusted field is SMA_HARIS - but RTSA itself
    # deliberately sets SMA_HARIS to NULL whenever the data show no material
    # heterogeneity for the requested adjustment (e.g. D2/I2 ~ 0%), because
    # there is then nothing to heterogeneity-adjust; it silently falls back
    # to its own plain (still sequentially-adjusted) SMA_RIS internally in
    # that case. The previous version of this endpoint always read
    # SMA_HARIS for random-effects models and reported "RIS unavailable"
    # whenever it came back NULL - which happens for EVERY random-effects
    # run on near-zero-heterogeneity data (the common case for a
    # well-conducted meta-analysis), even though a perfectly valid RIS
    # (SMA_RIS) was available the whole time. Mirror RTSA's own fallback
    # instead of reporting unavailable.
    ris_raw <- if (!fixed) result$results$SMA_HARIS else result$results$SMA_RIS
    ris_is_diversity_adjusted <- !fixed && !is.null(ris_raw) && !(length(ris_raw) == 1 && is.na(ris_raw))
    if (is.null(ris_raw) || (length(ris_raw) == 1 && is.na(ris_raw))) {
      ris_raw <- result$results$SMA_RIS
    }
    ris <- if (length(ris_raw) == 1 && !is.na(ris_raw)) as.numeric(ris_raw) else NA
    ris_unavailable <- is.na(ris)
    ris_label <- if (ris_is_diversity_adjusted) {
      "SMA-adjusted Diversity/Heterogeneity Adjusted RIS (HARIS)"
    } else if (!fixed) {
      "Required Information Size (RIS) - not diversity-adjusted, because this data showed no material heterogeneity (D²/I² near 0%) for the adjustment to apply"
    } else {
      "Sequentially-adjusted Required Information Size (RIS)"
    }
    info_fraction <- if (!ris_unavailable && ris > 0) ais / ris else NA
    d2_val <- tryCatch(as.numeric(result$results$metaanalysis$hete_results$hete_est$D.2), error = function(e) NA)

    crossed_benefit <- mapply(function(z, u) !is.na(z) && !is.na(u) && z >= u, z_col, upper_col)
    crossed_harm <- mapply(function(z, l) !is.na(z) && !is.na(l) && z <= l, z_col, lower_col)
    crossed_futility <- if (futility != "none") {
      mapply(function(z, fu, fl) !is.na(z) && ((!is.na(fu) && z <= fu) || (!is.na(fl) && z >= fl)), z_col, fut_upper_col, fut_lower_col)
    } else rep(FALSE, k)

    # --- Figure --------------------------------------------------------------
    # RTSA's own plot(x, model=, type=, theme=) exposes no control over axis
    # metric, per-study labels, boundary color, legend text, or an RIS
    # annotation, all of which were explicitly requested, so this builds a
    # custom ggplot2 figure directly from RTSA's own computed numbers
    # (z_col / upper_col / lower_col / cum_n / ais / ris above) rather than
    # approximating anything - every plotted value still comes straight out
    # of the RTSA model fit.
    boundary_name <- if (es_alpha == "esPoc") "Lan-DeMets Pocock" else "O'Brien-Fleming"
    model_label <- if (fixed) "fixed effect" else "random effects"
    z_series <- paste0("Cumulative Z-curve (", model_label, ")")
    bound_series <- paste0(boundary_name, " monitoring boundary")
    fut_series <- "Futility boundary"

    curve_df <- data.frame(x = cum_n, y = z_col, label = rtsa_data$study, series = z_series, grp = "z")

    # The monitoring boundary is drawn from RTSA's own smooth design-stage
    # grid (bounds$inf_frac / alpha_ubound / alpha_lbound), NOT from the
    # jagged per-study rdf$upper/lower - those are only evaluated at each
    # study's own (heterogeneity-adjusted) information fraction, which can
    # swing non-monotonically as tau2/D2 are re-estimated study-to-study and
    # produces a sawtooth artifact rather than the smooth analytic curve.
    # Information fraction -> participants uses the same RIS denominator
    # RTSA itself used to build this design.
    bnd <- result$bounds
    if (!ris_unavailable) {
      bx <- bnd$inf_frac * ris
      ok <- !is.na(bx) & !is.na(bnd$alpha_ubound) & bnd$alpha_ubound < 90
      bound_df <- rbind(
        data.frame(x = bx[ok], y = pmin(bnd$alpha_ubound[ok], 30), series = bound_series, grp = "bound_upper"),
        data.frame(x = bx[ok], y = pmax(bnd$alpha_lbound[ok], -30), series = bound_series, grp = "bound_lower")
      )
      bound_df <- bound_df[order(bound_df$grp, bound_df$x), ]
      fut_df <- if (futility != "none" && !all(is.na(bnd$beta_ubound))) {
        fok <- !is.na(bx) & !is.na(bnd$beta_ubound)
        f <- rbind(
          data.frame(x = bx[fok], y = bnd$beta_ubound[fok], series = fut_series, grp = "fut_upper"),
          data.frame(x = bx[fok], y = bnd$beta_lbound[fok], series = fut_series, grp = "fut_lower")
        )
        f[order(f$grp, f$x), ]
      } else NULL
    } else {
      # No RIS => no participant-scale x-position for the boundary grid;
      # skip the boundary curves rather than draw them at a fabricated scale.
      bound_df <- data.frame(x = numeric(0), y = numeric(0), series = character(0), grp = character(0))
      fut_df <- NULL
    }

    y_cap <- max(8, ceiling(max(abs(z_col), na.rm = TRUE)) + 1)
    x_max <- max(c(cum_n, if (!ris_unavailable) ris else 0), na.rm = TRUE) * 1.03

    series_colors <- c("black", "#b3111a", "#e08214")
    names(series_colors) <- c(z_series, bound_series, fut_series)
    series_linetypes <- c("solid", "solid", "22")
    names(series_linetypes) <- c(z_series, bound_series, fut_series)

    p <- ggplot() +
      geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
      { if (!is.null(fut_df)) geom_line(data = fut_df, aes(x = x, y = y, color = series, linetype = series, group = grp), linewidth = 0.9) } +
      geom_line(data = bound_df, aes(x = x, y = y, color = series, linetype = series, group = grp), linewidth = 1.1) +
      geom_line(data = curve_df, aes(x = x, y = y, color = series, linetype = series, group = grp), linewidth = 1.0) +
      geom_point(data = curve_df, aes(x = x, y = y), color = "black", size = 3.2) +
      ggrepel::geom_text_repel(data = curve_df, aes(x = x, y = y, label = label),
                                size = 4.3, color = "black", segment.color = "grey50",
                                # PERFORMANCE FIX: max.overlaps = Inf makes ggrepel exhaustively
                                # search for a non-overlapping position for every label, which is
                                # measurably slower than a generous finite cap (profiled: ~35-40%
                                # slower even for just 10 labels) for no visible benefit - a cap
                                # well above the study count (k) never actually drops a label in
                                # practice, so this changes nothing about how the plot looks.
                                # On Render's shared free-tier CPU this directly reduces the risk
                                # of a request running long enough to hit a network idle timeout
                                # (the likely cause of intermittent "page couldn't load" crashes
                                # reported on larger TSA datasets).
                                max.overlaps = max(30, k), box.padding = 0.5, min.segment.length = 0, seed = 1) +
      { if (!ris_unavailable) geom_vline(xintercept = ris, color = "#1a5fb3", linetype = "dotted", linewidth = 1) } +
      { if (!ris_unavailable) annotate("label", x = x_max * 0.02, y = -y_cap * 0.72, hjust = 0, vjust = 1,
          label = paste0(
            if (!fixed) "Diversity-adjusted RIS = " else "Required Information Size = ", format(round(ris), big.mark = ","),
            "\nδ (expected effect) = ", signif(mc_val, 3),
            "\nPower = ", power_val, "%   α = ", alpha_val,
            if (!fixed && !is.na(d2_val)) paste0("\nD² = ", round(d2_val * 100, 1), "%") else ""
          ), size = 4, fill = "white", label.size = 0.3, lineheight = 1.15) } +
      scale_color_manual(values = series_colors, breaks = c(z_series, bound_series, if (!is.null(fut_df)) fut_series)) +
      scale_linetype_manual(values = series_linetypes, breaks = c(z_series, bound_series, if (!is.null(fut_df)) fut_series)) +
      scale_x_continuous(name = "Cumulative number of randomised participants", labels = scales::comma, breaks = scales::pretty_breaks(n = 8), expand = expansion(mult = c(0.02, 0.05))) +
      coord_cartesian(ylim = c(-y_cap, y_cap), xlim = c(0, x_max)) +
      labs(y = "Cumulative Z-score", color = NULL, linetype = NULL,
           title = paste0("Trial Sequential Analysis — ", sm, " (", model_label, ")"),
           subtitle = paste0(boundary_name, " boundary · α = ", alpha_val, " (", side, "-sided) · power = ", power_val, "%")) +
      theme_classic(base_size = 13) +
      theme(
        axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.6),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12.5, color = "grey30"),
        legend.position = c(0.985, 0.985), legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.85), color = "grey70"),
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_line(color = "grey92")
      )

    n_studies <- nrow(rtsa_data)
    plot_width <- max(13, 9 + n_studies * 0.22)
    plot_height <- max(9, 7 + n_studies * 0.05)
    plot_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(plot_file, plot = p, width = plot_width, height = plot_height, dpi = 300, bg = "white", limitsize = FALSE)

    interpretation <- if (any(crossed_benefit, na.rm = TRUE)) {
      "The cumulative Z-curve crossed the trial sequential monitoring boundary for benefit before the required information size was reached."
    } else if (any(crossed_harm, na.rm = TRUE)) {
      "The cumulative Z-curve crossed the trial sequential monitoring boundary for harm before the required information size was reached."
    } else if (futility != "none" && any(crossed_futility, na.rm = TRUE)) {
      "The cumulative Z-curve crossed the futility boundary."
    } else if (!is.na(info_fraction) && info_fraction >= 1) {
      "The required information size has been reached and the cumulative Z-curve did not cross a monitoring boundary for benefit or harm."
    } else {
      "The cumulative Z-curve did not cross the monitoring boundaries and the required information size has not yet been reached."
    }

    list(
      status = "success",
      plot_base64 = paste0("data:image/png;base64,", base64encode(plot_file)),
      settings = list(
        outcome_type = outcome_type, effect_measure = sm,
        model = if (fixed) "Fixed-effect" else "Random-effects",
        weighting = "MH", alpha = alpha_val, beta = beta_val, power = power_val, side = side,
        expected_effect_mode = expected_effect_mode, expected_effect_used = mc_val,
        expected_effect_is_posthoc_assumption = used_observed_effect,
        control_event_risk_mode = control_risk_mode, control_event_risk_used = pC_val,
        heterogeneity_adjustment = if (fixed) "not applicable (fixed-effect model)" else het_adj,
        re_method = "DL_HKSJ (Hartung-Knapp-Sidik-Jonkman adjusted DerSimonian-Laird)",
        boundary_type = if (es_alpha == "esPoc") "Lan-DeMets Pocock" else "Lan-DeMets O'Brien-Fleming",
        futility = futility, n_studies = k
      ),
      information = list(
        accrued_information_size = as.numeric(ais),
        required_information_size = if (ris_unavailable) NA else as.numeric(ris),
        required_information_size_label = ris_label,
        required_information_size_unavailable = ris_unavailable,
        required_information_size_note = if (ris_unavailable) "RTSA could not compute a required information size for this combination of settings (this can happen with very few studies combined with a futility boundary and a diversity/heterogeneity adjustment). Accrued information size and the Z-curve are still valid; try disabling futility or using more studies to obtain a required information size." else NULL,
        information_fraction = as.numeric(info_fraction),
        diversity_d2 = d2_val,
        i2 = tryCatch(as.numeric(result$results$metaanalysis$hete_results$hete_est$I.2), error = function(e) NA),
        tau2 = tryCatch(as.numeric(result$results$metaanalysis$hete_results$hete_est$tau2), error = function(e) NA)
      ),
      interpretation = interpretation,
      table = list(
        analysis_order = as.list(seq_len(k)),
        study = as.list(rtsa_data$study),
        cumulative_participants = safe_num(cum_n),
        cumulative_information_fraction = safe_num(if (!ris_unavailable && ris > 0) cum_n / ris else rep(NA, k)),
        cumulative_z = safe_num(z_col),
        monitoring_boundary_upper = safe_num(upper_col),
        monitoring_boundary_lower = safe_num(lower_col),
        futility_boundary_upper = safe_num(fut_upper_col),
        futility_boundary_lower = safe_num(fut_lower_col),
        crossed_benefit = as.list(crossed_benefit),
        crossed_harm = as.list(crossed_harm),
        crossed_futility = as.list(crossed_futility)
      )
    )
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] TSA CAUGHT ERROR: ", msg,
                        " | raw body: ", tryCatch(req$postBody, error = function(e2) "<unavailable>"))
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/tsa-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = msg)
  })
}
