# =============================================================================
# AI-Assisted Risk of Bias Assessment API - PLOTTING ONLY.
#
# This service does NOT call any AI model and does NOT read PDFs, and does
# NOT change any classification/compatibility/decision logic - all of that
# (Gemini evidence extraction, deterministic RoB2/ROBINS-I/QUADAS-2 domain
# and overall judgment computation) happens server-side in the Next.js app
# (app/api/rob/assess/route.ts and app/lib/rob/*.ts) and is UNCHANGED by
# this file. This R service's only job is turning the FINAL, already-
# computed study-level domain judgments (either the AI-proposed ones, or
# the human-reviewed final ones - the frontend chooses which to send, but
# never invents or reinterprets a judgment) into publication-style
# traffic-light and summary plots, using the `robvis` package - the same
# widely-used tool published alongside the RoB 2 / ROBINS-I / QUADAS-2
# guidance for exactly this purpose, rather than reinventing bespoke
# plotting logic in React/HTML/CSS/canvas.
#
# robvis 0.3.1 (verified: packageVersion("robvis") == "0.3.1"; rob_tools()
# confirms tool = "ROB2" | "ROBINS-I" | "QUADAS-2" are the exact supported
# strings for this installed version).
#
# Runs as its own Plumber process on its own port, supervised the same way
# as every other backend in this project (see scripts/backend-supervisor.js)
# - completely independent of api.R/tsa-api.R/nma-api.R/metareg-api.R.
# =============================================================================
library(plumber)
library(jsonlite)
library(robvis)
library(ggplot2)
library(base64enc)
library(svglite)

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
  list(status = "ok", service = "rob-api.R (AI-Assisted Risk of Bias - plots)",
       robvis_version = as.character(utils::packageVersion("robvis")),
       uptime_seconds = round(as.numeric(difftime(Sys.time(), STARTED_AT, units = "secs"))))
}

#* @plumber
function(pr) {
  pr %>% plumber::pr_set_error(function(req, res, err) {
    msg <- tryCatch(conditionMessage(err), error = function(e) "unknown error")
    log_line <- paste0("[", format(Sys.time()), "] ROB UNCAUGHT ERROR: ", msg, " | route: ", req$PATH_INFO)
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/rob-error.log", append = TRUE), silent = TRUE)
    res$status <- 200
    list(status = "error", message = paste("Uncaught server error:", msg))
  })
}

# =============================================================================
# 1. TOOL / DOMAIN / JUDGEMENT REGISTRY
#
# The single source of truth for what a valid request looks like for each
# framework. Judgement vocabularies are the EXACT strings this project's own
# deterministic decision engine (app/lib/rob/{rob2,robinsI,quadas2}.ts)
# produces - see those files. Nothing here invents a category or accepts a
# value those files don't produce.
# =============================================================================
map_tool <- function(framework) {
  if (framework == "RoB2") return("ROB2")
  if (framework == "ROBINS-I") return("ROBINS-I")
  if (framework == "QUADAS-2") return("QUADAS-2")
  stop(paste("Unknown framework:", framework))
}

domain_keys_for <- function(framework) {
  if (framework == "RoB2") return(paste0("D", 1:5))
  if (framework == "ROBINS-I") return(paste0("D", 1:7))
  if (framework == "QUADAS-2") return(paste0("D", 1:4))
  stop(paste("Unknown framework:", framework))
}

# Applicability domains only apply to QUADAS-2's first 3 domains (Flow and
# timing / D4 has no applicability concept) - see app/lib/rob/quadas2.ts.
#
# IMPORTANT LIMITATION (verified against the installed robvis 0.3.1 source,
# not assumed): both rob_traffic_light() and rob_summary() hard-require a
# QUADAS-2 input with exactly Study + D1 + D2 + D3 + D4 + Overall (6
# columns) - there is no separate applicability-only structure in this
# package version. Applicability has only 3 domains and no official
# "overall applicability" judgement, so satisfying robvis's column
# requirement here would mean fabricating a D4/Overall value that was never
# assessed - forbidden by this project's statistical-integrity rule (never
# alter/invent a judgement to make it "fit" the plotting library). Rather
# than fabricate, applicability plotting is deliberately NOT attempted;
# applicability concerns remain fully visible in the assessment table
# (ResultsTable.tsx's second table) instead. See DOCS.md for the same note.
applicability_domain_keys_for <- function(framework) {
  if (framework == "QUADAS-2") return(paste0("D", 1:3))
  return(character(0))
}

allowed_judgements_for <- function(framework, field) {
  if (framework == "RoB2") return(c("Low risk of bias", "Some concerns", "High risk of bias"))
  if (framework == "ROBINS-I") return(c("Low", "Moderate", "Serious", "Critical", "No information"))
  if (framework == "QUADAS-2") return(c("Low", "High", "Unclear")) # same vocabulary for both RoB and applicability
  stop(paste("Unknown framework:", framework))
}

# robvis 0.3.1 buckets each judgment string by its lowercased FIRST LETTER
# only (see rob_traffic_light source: substr(tolower(x), 0, 1)), against a
# fixed per-tool set of codes. This project stores the correct OFFICIAL
# judgment wording throughout (app/lib/rob/types.ts) - this function
# translates ONLY the copy of the data sent to robvis for plotting, never
# the data returned to or stored by the frontend, and never changes which
# CATEGORY a judgement belongs to (only which token robvis needs to see to
# recognize that same category):
#   - RoB2:      h/s/l -> High/"Some concerns"/Low            (matches our wording as-is)
#   - ROBINS-I:  c/s/m/l -> Critical/Serious/Moderate/Low.
#     "No information" starts with 'n', which robvis 0.3.1 does not
#     recognize for this tool - a known limitation of the installed package
#     version (documented in MethodologyPanel and in DOCS.md). Those points
#     render as unmapped/grey (NA) rather than a fabricated category -
#     never silently recoloured as a real judgement.
#   - QUADAS-2:  l/s/h -> Low/Unclear/High. robvis's own QUADAS-2 code path
#     internally displays code "s" as the label "Unclear", so our official
#     "Unclear" wording (which starts with 'u') must be translated to a
#     string starting with 's' for robvis to bucket it correctly - "Some
#     concerns" is used here ONLY as that translation token; it is never
#     shown to the user, who always sees "Unclear" in the app's own
#     table/UI/downloads.
translate_for_robvis <- function(values, framework) {
  if (framework == "QUADAS-2") {
    values[values == "Unclear"] <- "Some concerns"
  }
  values
}

# =============================================================================
# 2. VALIDATION LAYER
#
# AI assessment output (already deterministically judged upstream) is NEVER
# sent straight into robvis. Every study is checked here first; on any
# failure this returns a clear, specific validation error and NO plot is
# generated - never a silent category conversion, never a best-effort guess.
# =============================================================================
validate_studies <- function(studies, framework, field) {
  if (length(studies) == 0) return("No studies were provided.")

  study_ids <- vapply(studies, function(s) {
    if (is.null(s$study_id) || !nzchar(trimws(as.character(s$study_id)))) return(NA_character_)
    as.character(s$study_id)
  }, character(1))
  if (any(is.na(study_ids))) return("One or more studies is missing a study ID.")
  if (any(duplicated(study_ids))) return(paste0("Duplicate study ID(s) found: ", paste(unique(study_ids[duplicated(study_ids)]), collapse = ", "), ". Each study must have a unique ID."))

  keys <- if (field == "applicability") applicability_domain_keys_for(framework) else domain_keys_for(framework)
  if (length(keys) == 0) return(paste0("Framework '", framework, "' has no '", field, "' domains to plot."))
  allowed <- allowed_judgements_for(framework, field)

  for (s in studies) {
    sid <- as.character(s$study_id)
    domains <- s[[field]]
    if (is.null(domains)) return(paste0("Study '", sid, "' is missing its '", field, "' domain judgements."))
    for (k in keys) {
      v <- domains[[k]]
      if (is.null(v) || !nzchar(as.character(v))) {
        return(paste0("Study '", sid, "' is missing a required domain judgement for ", k, "."))
      }
      if (!(as.character(v) %in% allowed)) {
        return(paste0(
          "Study '", sid, "' has an invalid judgement '", as.character(v), "' for domain ", k,
          ". Assessment requires review because the judgement could not be mapped unambiguously to the ", framework, " tool. Expected one of: ", paste(allowed, collapse = ", "), "."
        ))
      }
    }
    if (field != "applicability") {
      overall <- s[[paste0(field, "_overall")]]
      if (is.null(overall) || !nzchar(as.character(overall))) return(paste0("Study '", sid, "' is missing its overall judgement."))
      if (!(as.character(overall) %in% allowed)) {
        return(paste0("Study '", sid, "' has an invalid overall judgement '", as.character(overall), "'. Expected one of: ", paste(allowed, collapse = ", "), "."))
      }
    }
  }
  NULL # NULL = valid
}

# Builds the exact data.frame shape robvis requires: Study, D1..Dn, Overall,
# Weight. Study order is preserved exactly as received (the caller's own
# upload/assessment order - see app/tools/risk-of-bias/page.tsx) - never
# reordered alphabetically or otherwise. WEIGHTING: this tool has no
# meta-analytic precision/weight variable for any study (it is a risk-of-
# bias assessment, not an effect-size synthesis), so every study is given
# an explicit equal weight and rob_summary() is always called with
# weighted = FALSE below - the Weight column exists only because robvis's
# input contract expects the column to be present, never because a real
# weight was computed or implied.
# Shortens a study name for the PLOT LABEL ONLY when it exceeds `max`
# characters, appending an ellipsis - the full, untruncated name remains
# the study's actual identity everywhere else (assessment table, evidence
# table, exports, human-override UI - see ResultsTable.tsx/StudyDetail.tsx,
# neither of which goes through this file at all). This is communicated to
# the frontend via the `truncated` flag in the response so it can show a
# one-line note under the image, satisfying "do NOT simply truncate without
# telling the user."
#
# WHY TRUNCATE RATHER THAN WRAP ONTO MULTIPLE LINES: wrapping was tried
# first (embedding "\n" into the Study factor level, which robvis's
# facet_grid-based strip renders) and was verified, by actually rendering
# and inspecting the image, to clip unpredictably - a rendering
# interaction between this ggplot2 version's strip-text layout and
# robvis 0.3.1's fixed angle=180 strip theme that persisted across
# multiple independent fixes (larger width, larger height, extra margin,
# `strip.clip = "off"`, smaller font). Single-line truncation at a safe
# character budget was the one approach verified, by direct visual
# inspection, to render every study label completely unclipped.
truncate_study_name <- function(name, max_chars = 20) {
  if (nchar(name) > max_chars) paste0(substr(name, 1, max_chars - 1), "…") else name
}

build_df <- function(studies, framework, field) {
  keys <- if (field == "applicability") applicability_domain_keys_for(framework) else domain_keys_for(framework)
  study_names <- vapply(studies, function(s) truncate_study_name(as.character(s$study_id)), character(1))
  # Truncation could coincidentally make two DIFFERENT full study IDs
  # collapse to the same short label (validate_studies() already rejected
  # duplicate FULL study IDs, so this can only happen post-truncation) -
  # disambiguate with a numeric suffix rather than silently merging two
  # different studies' rows into one facet.
  dupe_counts <- table(study_names)
  seen <- character(0)
  for (i in seq_along(study_names)) {
    if (dupe_counts[[study_names[i]]] > 1) {
      seen <- c(seen, study_names[i])
      study_names[i] <- paste0(study_names[i], " (", sum(seen == study_names[i]), ")")
    }
  }
  cols <- list(Study = study_names)
  for (k in keys) {
    vals <- vapply(studies, function(s) as.character(s[[field]][[k]]), character(1))
    cols[[k]] <- translate_for_robvis(vals, framework)
  }
  if (field != "applicability") {
    overall_vals <- vapply(studies, function(s) as.character(s[[paste0(field, "_overall")]]), character(1))
    cols[["Overall"]] <- translate_for_robvis(overall_vals, framework)
  }
  cols[["Weight"]] <- rep(1, length(studies)) # equal weight - see WEIGHTING note above
  as.data.frame(cols, stringsAsFactors = FALSE)
}

# =============================================================================
# 3. FIGURE SIZING
#
# Dimensions scale with the number of studies and the number of domains, so
# a 5-study figure isn't full of blank space and a 50-study figure doesn't
# crush study labels into illegibility.
#
# The constants below were determined EMPIRICALLY by actually rendering and
# visually inspecting test figures at several heights and study-name
# lengths (see DOCS.md and this session's iteration history), not derived
# analytically - robvis's fixed-angle strip-label theme clips study names
# unless given noticeably more vertical room per study than a naive
# per-row estimate would suggest, AND that required room increases further
# with label length even though the labels never wrap to a second line.
# 0.8in/study is the minimum that rendered SHORT labels (e.g. "Alpha")
# unclipped; longer labels need more - name_factor below adds headroom
# scaled to the longest (already truncated - see truncate_study_name())
# label actually being plotted. This is intentionally generous rather than
# tightly optimized, because an earlier, tighter estimate silently produced
# clipped labels that only visual (not HTTP-response) inspection caught.
traffic_light_dims <- function(n_studies, n_domains, max_name_chars) {
  name_factor <- max(0, max_name_chars - 7) * 0.07
  # A floor of 3in: verified by direct visual inspection that a single
  # short-named study still clips against the fixed header/legend overhead
  # without it (the header/legend/margin allowance is a near-fixed cost
  # that a 1-2 row figure doesn't have enough total height to absorb
  # otherwise).
  height <- max(3.0, 1.5 + n_studies * (0.8 + name_factor))
  width <- max(8.5, 5.6 + n_domains * 0.85)
  list(width = width, height = height)
}

summary_dims <- function(n_domains) {
  list(width = max(7, 4 + n_domains * 0.85), height = 5.5)
}

# =============================================================================
# 4. FILE OUTPUT
#
# Generates every requested format for one plot and returns whichever
# formats actually succeeded - a format that fails for some reason (e.g. an
# unusual device backend issue) is simply omitted from the response rather
# than failing the whole request, and the frontend only shows download
# buttons for formats present in the response (never a fake/dead button).
# =============================================================================
FORMAT_SPECS <- list(
  png = list(device = "png", mime = "image/png", ext = ".png"),
  jpg = list(device = "jpeg", mime = "image/jpeg", ext = ".jpg"),
  pdf = list(device = "pdf", mime = "application/pdf", ext = ".pdf"),
  # ggsave's `device` argument wants an actual device FUNCTION for svglite,
  # not the string "svglite" (that string is only meaningful as a file
  # extension lookup, which fails since svglite isn't ggsave's built-in
  # dispatch table) - passing svglite::svglite directly is the documented
  # way to use it as a ggsave device.
  svg = list(device = svglite::svglite, mime = "image/svg+xml", ext = ".svg")
)

render_all_formats <- function(p, width, height) {
  out <- list()
  for (fmt in names(FORMAT_SPECS)) {
    spec <- FORMAT_SPECS[[fmt]]
    result <- tryCatch({
      f <- tempfile(fileext = spec$ext)
      on.exit(unlink(f), add = TRUE)
      if (fmt %in% c("png", "jpg")) {
        ggplot2::ggsave(f, plot = p, device = spec$device, width = width, height = height, dpi = 300, bg = "white", limitsize = FALSE)
      } else {
        # Vector formats: dpi is not meaningful, but width/height (inches)
        # still control the canvas so text isn't proportionally shrunk.
        ggplot2::ggsave(f, plot = p, device = spec$device, width = width, height = height, bg = "white", limitsize = FALSE)
      }
      paste0("data:", spec$mime, ";base64,", base64encode(f))
    }, error = function(e) {
      log_line <- paste0("[", format(Sys.time()), "] ROB format '", fmt, "' failed: ", conditionMessage(e))
      try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/rob-error.log", append = TRUE), silent = TRUE)
      NULL
    })
    if (!is.null(result)) out[[fmt]] <- result
  }
  out
}

# =============================================================================
# ROBVIS 0.3.1 BUG WORKAROUND (found via direct source inspection AND
# confirmed by visually inspecting a generated image, not assumed):
#
#   robvis:::rob_traffic_light() and robvis:::rob_summary() end with
#   `if (quiet != TRUE) { return(trafficlightplot) }` and NOTHING else - no
#   print(), no other return statement. When called with quiet = TRUE (the
#   value this file was originally using to keep the Plumber process from
#   printing to a display it doesn't have), the function computes the plot
#   correctly but then returns NULL, discarding it. ggsave() with a NULL
#   plot does not error - grid.draw(NULL) silently draws nothing - so this
#   previously produced a "successful" response containing a blank white
#   image of the right dimensions. Caught by actually opening the generated
#   PNG and looking at it (per this task's explicit visual-regression
#   requirement), not by the HTTP response, which reported status success.
#
#   Fix: call with quiet = FALSE, which correctly returns the plot object -
#   but quiet = FALSE also calls print() on it as a side effect, and R's
#   default graphics device in this headless Rscript process would create
#   a stray "Rplots.pdf" file in the working directory for that print. This
#   wraps the call inside a null pdf device (pdf(file = NULL), a real,
#   documented grDevices device that discards everything drawn to it) so
#   the incidental print has somewhere harmless to go, then closes that
#   device immediately - the RETURNED ggplot object itself is unaffected
#   either way and is what render_all_formats() actually saves below.
# =============================================================================
# =============================================================================
# QUADAS-2 LEGEND-LABEL FIX (found via direct source inspection AND
# confirmed by visually inspecting a generated image):
#
#   The installed robvis 0.3.1's QUADAS-2 code path in BOTH
#   rob_traffic_light() and rob_summary() hardcodes the legend text "Some
#   concerns" for the middle judgement category - literally reusing RoB 2's
#   wording - instead of QUADAS-2's own official "Unclear" terminology.
#   (rob_summary() additionally hardcodes RoB-2-style padded text like
#   "High risk of bias" for the other two categories rather than QUADAS-2's
#   plain "High"/"Low".) This is a genuine limitation in the installed
#   package version, not a choice made by this project, and left uncorrected
#   it would violate the explicit requirement never to substitute QUADAS-2's
#   "Unclear" for RoB 2's "Some concerns" wording.
#
#   Fix: ggplot2 allows a scale to be replaced on an already-built plot by
#   adding a new scale of the same aesthetic (colour/shape/fill) - this
#   overrides ONLY the legend text and is applied AFTER robvis has already
#   computed every point's position, colour bucket, and facet layout, so it
#   changes nothing about how the underlying judgement data was read,
#   bucketed, or plotted - only the words a human reads in the legend.
# =============================================================================
QUADAS2_COLOURS <- c(l = "#02C100", s = "#E2DF07", h = "#BF0000")
QUADAS2_LABELS <- c(l = "Low", s = "Unclear", h = "High")

fix_quadas2_labels <- function(p, plot_kind) {
  if (plot_kind == "traffic_light") {
    p + ggplot2::scale_colour_manual(values = QUADAS2_COLOURS, labels = QUADAS2_LABELS) +
      ggplot2::scale_shape_manual(values = c(l = 43, s = 45, h = 120), labels = QUADAS2_LABELS)
  } else {
    p + ggplot2::scale_fill_manual("Risk of Bias", values = QUADAS2_COLOURS, labels = QUADAS2_LABELS)
  }
}

generate_robvis_plot <- function(build_fn) {
  grDevices::pdf(file = NULL)
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
  p <- build_fn(FALSE)
  if (is.null(p)) stop("robvis returned no plot object for this data.")
  p
}

parse_request <- function(req) {
  # Deliberately ignore plumber's auto-parsed req$body and re-parse the raw
  # JSON with simplifyVector = FALSE: `studies` is a JSON array of objects
  # with a nested `domains` object, and jsonlite's default simplification
  # collapses that into an inconsistent data.frame/vector shape depending on
  # payload contents. Parsing with simplifyVector = FALSE always yields a
  # plain nested list, which every function above expects.
  fromJSON(req$postBody, simplifyVector = FALSE)
}

#* @serializer unboxedJSON
#* @parser json
#* @post /api/rob/traffic-light-plot
function(req, res) {
  tryCatch({
    body <- parse_request(req)
    framework <- body$framework
    studies <- body$studies
    # QUADAS-2 requires the risk-of-bias and applicability plots to be
    # generated separately (never combined) - `mode` selects which field
    # set the caller wants (ignored for RoB2/ROBINS-I, which have only one).
    mode <- if (!is.null(body$mode)) body$mode else "domains"
    if (is.null(framework) || is.null(studies) || length(studies) == 0) stop("`framework` and a non-empty `studies` array are required.")

    if (framework == "QUADAS-2" && mode == "applicability") {
      res$status <- 200
      return(list(status = "error", message = "QUADAS-2 applicability concerns cannot be rendered as a traffic-light plot: the installed robvis package (0.3.1) only supports its standard 4-domain risk-of-bias structure for QUADAS-2, with no separate applicability template, and fabricating the missing columns to force compatibility is not permitted. Applicability concerns are shown in the assessment table instead."))
    }
    field <- "domains"
    validation_error <- validate_studies(studies, framework, field)
    if (!is.null(validation_error)) {
      res$status <- 200
      return(list(status = "error", message = validation_error))
    }

    df <- build_df(studies, framework, field)
    n <- nrow(df)
    n_domains <- length(domain_keys_for(framework))
    max_name_chars <- max(nchar(df$Study))
    dims <- traffic_light_dims(n, n_domains, max_name_chars)
    original_ids <- vapply(studies, function(s) as.character(s$study_id), character(1))
    truncated <- any(nchar(original_ids) > 20)

    p <- generate_robvis_plot(function(q) robvis::rob_traffic_light(df, tool = map_tool(framework), psize = 18, quiet = q))
    if (framework == "QUADAS-2") p <- fix_quadas2_labels(p, "traffic_light")
    files <- render_all_formats(p, dims$width, dims$height)
    if (length(files) == 0) stop("Traffic-light plot generation failed for every requested format.")
    list(status = "success", files = files, mode = field, n_studies = n, truncated_labels = truncated)
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] ROB CAUGHT ERROR (traffic-light): ", msg)
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/rob-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = paste("Risk-of-bias traffic-light plot generation failed:", msg))
  })
}

#* @serializer unboxedJSON
#* @parser json
#* @post /api/rob/summary-plot
function(req, res) {
  tryCatch({
    body <- parse_request(req) # see comment in the traffic-light-plot handler above
    framework <- body$framework
    studies <- body$studies
    mode <- if (!is.null(body$mode)) body$mode else "domains"
    if (is.null(framework) || is.null(studies) || length(studies) == 0) stop("`framework` and a non-empty `studies` array are required.")

    if (framework == "QUADAS-2" && mode == "applicability") {
      res$status <- 200
      return(list(status = "error", message = "QUADAS-2 applicability concerns cannot be rendered as a summary plot: the installed robvis package (0.3.1) only supports its standard 4-domain risk-of-bias structure for QUADAS-2, with no separate applicability template, and fabricating the missing columns to force compatibility is not permitted. Applicability concerns are shown in the assessment table instead."))
    }
    field <- "domains"
    validation_error <- validate_studies(studies, framework, field)
    if (!is.null(validation_error)) {
      res$status <- 200
      return(list(status = "error", message = validation_error))
    }

    df <- build_df(studies, framework, field)
    n_domains <- length(domain_keys_for(framework))
    dims <- summary_dims(n_domains)

    # weighted = FALSE: see the WEIGHTING note in build_df() above - this
    # tool never fabricates a meta-analytic weight.
    p <- generate_robvis_plot(function(q) robvis::rob_summary(df, tool = map_tool(framework), overall = TRUE, weighted = FALSE, quiet = q))
    if (framework == "QUADAS-2") p <- fix_quadas2_labels(p, "summary")
    files <- render_all_formats(p, dims$width, dims$height)
    if (length(files) == 0) stop("Summary plot generation failed for every requested format.")
    list(status = "success", files = files, mode = field, n_studies = nrow(df))
  }, error = function(e) {
    res$status <- 200
    msg <- as.character(e$message)
    log_line <- paste0("[", format(Sys.time()), "] ROB CAUGHT ERROR (summary): ", msg)
    try(cat(log_line, "\n", file = "C:/Users/munee/OneDrive/Desktop/metaworld-r-backend/rob-error.log", append = TRUE), silent = TRUE)
    list(status = "error", message = paste("Risk-of-bias summary plot generation failed:", msg))
  })
}
