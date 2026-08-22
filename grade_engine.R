library(plumber)
library(jsonlite)

# --- 1. INCONSISTENCY ASSESSMENT ---
assess_inconsistency <- function(i2, pred_interval_crosses_null = FALSE, substantial_dir_variation = FALSE) {
  # Based on Cochrane / GRADE thresholds (Brozek et al., 2021)
  rating <- if (i2 < 40) {
    "Not serious"
  } else if (i2 >= 40 && i2 <= 75) {
    "Serious"
  } else {
    "Very serious"
  }
  
  downgrade_steps <- 0
  if (rating == "Serious") downgrade_steps <- 1
  if (rating == "Very serious" || pred_interval_crosses_null || substantial_dir_variation) downgrade_steps <- 2
  
  return(list(rating = rating, steps = downgrade_steps))
}

# --- 2. INDIRECTNESS ASSESSMENT ---
assess_indirectness <- function(override_rating = NULL, pop_diff = FALSE, intervention_diff = FALSE, surrogate_outcome = FALSE) {
  if (!is.null(override_rating)) {
    steps <- switch(override_rating, "Not serious" = 0, "Serious" = 1, "Very serious" = 2, 0)
    return(list(rating = override_rating, steps = steps))
  }
  
  if (pop_diff || intervention_diff || surrogate_outcome) {
    return(list(rating = "Serious", steps = 1))
  }
  return(list(rating = "Not serious", steps = 0))
}

# --- 3. IMPRECISION ASSESSMENT ---
assess_imprecision <- function(sample_size, ois_threshold = 400, ci_crosses_null = FALSE, very_wide_ci = FALSE) {
  steps <- 0
  rating <- "Not serious"
  
  if (ci_crosses_null || sample_size < ois_threshold) {
    rating <- "Serious"
    steps <- 1
  }
  if (very_wide_ci && sample_size < (ois_threshold / 2)) {
    rating <- "Very serious"
    steps <- 2
  }
  
  return(list(rating = rating, steps = steps))
}

# --- 4. CERTAINTY CALCULATION ---
calculate_certainty <- function(study_design, rob_rating, inconsistency_steps, indirectness_steps, imprecision_steps, pub_bias_rating, upgrade_boost = 0) {
  # Base certainty level: RCT = 4 (High), Observational = 2 (Low), Modeling = 3 (Moderate/Variable)
  base_score <- switch(toupper(study_design), "RCT" = 4, "OBSERVATIONAL" = 2, "MODELING" = 3, 3)
  
  # Risk of bias steps
  rob_steps <- switch(rob_rating, "Not serious" = 0, "Serious" = 1, "Very serious" = 2, 0)
  
  # Publication bias steps
  pub_steps <- switch(pub_bias_rating, "Undetected" = 0, "Suspected" = 1, "Undetected" = 0, 0)
  
  total_downgrades <- rob_steps + inconsistency_steps + indirectness_steps + imprecision_steps + pub_steps
  final_score <- max(1, min(4, base_score - total_downgrades + upgrade_boost))
  
  certainty_label <- switch(final_score,
    "1" = "⊕◯◯◯ Very low",
    "2" = "⊕⊕◯◯ Low",
    "3" = "⊕⊕⊕◯ Moderate",
    "4" = "⊕⊕⊕⊕ High"
  )
  
  return(list(score = final_score, label = certainty_label))
}

# --- 5. PLUMBER ENDPOINT FOR GRADE ASSESSMENT ---
#* @post /api/grade/evaluate
function(req) {
  body <- fromJSON(req$postBody)
  
  outcome <- body$outcome
  effect <- body$effect
  k <- as.numeric(body$k)
  n <- as.numeric(body$n)
  i2 <- as.numeric(body$i2)
  rob <- body$risk_of_bias
  pub_bias <- body$publication_bias
  study_design <- body$study_design
  
  # Run modular rules
  inc <- assess_inconsistency(i2)
  ind <- assess_indirectness(if(!is.null(body$indirectness_override)) body$indirectness_override else NULL)
  imp <- assess_imprecision(n, ois_threshold = 300)
  cert <- calculate_certainty(study_design, rob, inc$steps, ind$steps, imp$steps, pub_bias)
  
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
}