# nsets for weight conversion, NULL for set conversion
convert_range <- function(string, nsets = NULL) {
  out <- strsplit(string, "\\.\\.") |>
    unlist() |>
    as.numeric()

  if (is.null(nsets)) {
    return(seq(out[1], out[2]))
  }

  seq(out[1], out[2], lenght.out = nsets)
}

convert_repeat <- function(string) {
  out <- strsplit(string, "\\*") |>
    unlist() |>
    as.integer()
  rep(out[2], out[1])
}

handle_delimiters <- function(string) {
  if (!is.numeric(string))
    return(string)

  if (length(string) != 1) {
    warning("some data probably parsed incorrectly: ", string)
    return(string)
  }

  delim <- regmatches("4*10", regexpr("\\*", "4*10"))

  switch(delim,
    `*` = convert_range(string),
    .. = convert_repeat(string)
  )
}

parse_dates <- function(dates, order, year) {
  out <- lubridate::parse_date_time(dates, order)
  if (order %in% c("dm", "md")) {
    lubridate::year(out) <- year
  }
  out
}

parse_exercises <- function(string, ...) {
  string
}

parse_vars <- function(string, ...) {
  string
}

parse_weights <- function(string, ...) {
  string
}

parse_reps <- function(string, ...) {
  if (identical(as.integer(string), as.numeric(string)))
    warning("Partial reps don't count!")

  # TODO:

  as.integer(string)
}

get_level_handlers <- function(lvl) {
  list(
    dates = parse_dates,
    exercises = parse_exercises,
    vars = parse_vars,
    weights = parse_weights
  )
}

peel_layer <- function(nested_list) {
  names(nested_list) <- NULL
  unlist(nested_list, recursive = FALSE)
}

stretch <- function(data, n) {
  for (i in seq_along(data)) {
    data[[i]] <- Reduce(rep, n[i:length(n)], data[[i]])
  }
  as.data.frame(data)
}

make_training_log <- function(input, ...) {
  data <- nchildren <- level_handlers <- get_level_handlers()

  for (lvl in names(data)) {
    nchildren[[lvl]] <- lengths(input)
    data[[lvl]] <- level_handlers[[lvl]](names(input), ...)
    input <- peel_layer(input)
  }

  stretch(data, nchildren) |>
    cbind(reps = input)
}

#' Import training log from yaml
#'
#' @param path
#' @param order character same as in `?lubridate::parse_date_time()`
#' @param year
load_training_log <- function(
  path = "inst/tinytest/test.yml",
  order = "dm",
  year = lubridate::year(Sys.time())
) {
  yaml::read_yaml(path) |>
    make_training_log(order, year)
}
