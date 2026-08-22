# Production image for the MetaWorld Research Academy R/Plumber statistical
# backends. ONE image serves all 5 services - which one a given container
# runs is chosen at runtime via the R_SCRIPT env var (see entrypoint.R),
# set per-service in render.yaml. This avoids five near-duplicate
# Dockerfiles while keeping every service's actual R code and statistical
# logic completely unchanged from local development.
FROM rocker/r-ver:4.4.1

# System libraries required to COMPILE the R packages this project uses
# (curl/ssl/xml for httr/jsonlite-adjacent deps, png/jpeg/freetype/tiff for
# ggplot2's graphics device stack, fontconfig/harfbuzz/fribidi for the
# textshaping/ragg chain modern ggplot2 pulls in, pandoc for robvis report
# generation). Not guessed - this is the standard system dependency set for
# this exact package combination (meta, metafor, netmeta, RTSA, robvis,
# ggplot2, svglite) on Debian-based images.
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    pandoc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install R packages first (own layer) so code-only changes below don't
# force a full package reinstall on every rebuild.
COPY install_packages.R /app/
RUN Rscript install_packages.R

# Copy every service's source + the shared generic entrypoint. All 5
# scripts are copied into every image (they're small, plain-text R files)
# so the SAME built image can serve any of the 5 services - only R_SCRIPT
# differs between deployments, never the image itself.
COPY api.R tsa-api.R nma-api.R metareg-api.R rob-api.R entrypoint.R /app/

# Render (and most standard PaaS platforms) inject PORT at runtime and
# expect the process to bind to it - entrypoint.R reads it via
# Sys.getenv("PORT"), defaulting to 8000 for local `docker run` testing
# without an explicit -e PORT=....
EXPOSE 8000

# Basic container-level health check hitting whichever service R_SCRIPT
# points at - every one of the 5 scripts exposes a GET /health (or, for
# nma-api.R specifically, GET /api/nma/health - see HEALTHCHECK note in
# render.yaml, which overrides this per-service where the path differs).
HEALTHCHECK --interval=30s --timeout=5s --start-period=40s --retries=3 \
  CMD curl -f "http://127.0.0.1:${PORT:-8000}/health" || exit 1

CMD ["Rscript", "entrypoint.R"]
