# Generic production entrypoint for ANY of the 5 R/Plumber services in this
# repo - which script to serve is chosen at runtime via the R_SCRIPT env var
# (set per-service in render.yaml), so one Docker image serves all 5
# services instead of duplicating near-identical Dockerfiles. Listens on
# the PORT env var Render (or any standard PaaS) injects at runtime, on
# 0.0.0.0 so it's reachable from outside the container - this is the one
# behavioral difference from local development, where
# scripts/backend-supervisor.js instead runs each script with a fixed local
# port on 127.0.0.1 only.
library(plumber)

script <- Sys.getenv("R_SCRIPT", "api.R")
port <- as.integer(Sys.getenv("PORT", "8000"))

if (!file.exists(script)) {
  stop(sprintf("R_SCRIPT is set to '%s', but that file does not exist in /app.", script))
}

pr <- plumber::pr(script)
pr$run(host = "0.0.0.0", port = port)
