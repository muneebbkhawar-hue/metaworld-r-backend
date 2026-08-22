library(plumber)
library(jsonlite)

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 200; return(list()) }
  plumber::forward()
}

# GRADE Assessment Rules (Brozek et al., 2021)
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
  
  # 1. Inconsistency
  inc_rating <- if (i2 < 40) "Not serious" else if (i2 <= 75) "Serious" else "Very serious"
  inc_steps <- if (inc_rating == "Serious") 1 else if (inc_rating == "Very serious") 2 else 0
  
  # 2. Indirectness
  ind_rating <- "Not serious"
  ind_steps <- 0
  
  # 3. Imprecision
  imp_rating <- if (n >= 400) "Not serious" else "Serious"
  imp_steps <- if (imp_rating == "Serious") 1 else 0
  
  # 4. Certainty calculation
  base_score <- if (toupper(study_design) == "RCT") 4 else 2
  rob_steps <- if (rob == "Serious") 1 else if (rob == "Very serious") 2 else 0
  pub_steps <- if (pub_bias == "Suspected") 1 else 0
  
  total_downs <- rob_steps + inc_steps + ind_steps + imp_steps + pub_steps
  final_score <- max(1, min(4, base_score - total_downs))
  
  cert_label <- switch(as.character(final_score),
    "1" = "⊕◯◯◯ Very low",
    "2" = "⊕⊕◯◯ Low",
    "3" = "⊕⊕⊕◯ Moderate",
    "4" = "⊕⊕⊕⊕ High"
  )
  
  list(
    status = "success",
    row = list(
      outcome = outcome,
      effect_ci = effect,
      k = k,
      n = n,
      risk_of_bias = rob,
      inconsistency = inc_rating,
      indirectness = ind_rating,
      imprecision = imp_rating,
      publication_bias = pub_bias,
      certainty = cert_label
    )
  )
}