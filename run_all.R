# Refresh all data: Rscript run_all.R (from the project root)
c("R/czso.R", "R/eurostat.R", "R/world.R") |>
  purrr::walk(source)
