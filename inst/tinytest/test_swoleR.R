unnest_exercises <- function(raw) {
  reps <- raw[c(FALSE, TRUE)]
  weights <- raw[c(TRUE, FALSE)]
  to_long(format(Sys.time(), "%Y"), seq_along(reps), reps, weights)
}

import_tabular <- function(path) {
  readLines(path) |>
    strsplit(";") |>
    lapply(strsplit, ",") |>
    rapply(as.numeric, how = "replace") |>
    unnest_exercises()
}

to_long <- function(date, exercise, reps, weight) {
  n_reps <- lengths(unlist(reps, recursive = FALSE))
  data.frame(
    dates = rep(date, lengths(reps)) |>
      rep(n_reps),
    exercises = rep(exercise, lengths(reps)) |>
      rep(n_reps),
    weights = unlist(weights) |>
      rep(n_reps),
    reps = unlist(reps)
  )
}

path <- "inst/tinytest/test.yml"
# old_csv_data <- to_long("squats.csv") |> suppressWarnings()
