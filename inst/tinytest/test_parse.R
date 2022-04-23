year <- 2022

parse_dates("18.04.", "dm", year) |>
  tryCatch(warning = \(w) w) |>
  is("warning") |>
  expect_false()

parse_dates("18.04.2022", "dmy", year) |>
  tryCatch(warning = \(w) w) |>
  is("warning") |>
  expect_false()

parse_dates("18..04.", "dmy", year) |>
  tryCatch(warning = \(w) w) |>
  is("warning") |>
  expect_true()

parse_dates("18.04.", "dm", year) == (
  parse_dates("18.04.2022", "dmy", year) ==
  parse_dates("18..04.", "dm", year)
) |>
  all() |>
  expect_true()

convert_repeat("4*10") |>
  expect_identical(rep(10L, 4))

convert_range("12..5") |>
  expect_identical(seq(12, 5))

convert_range("12,,5") |>
  expect_warning()

# TODO: worth allowing?
convert_range("12..5, 4") |>
  expect_warning()
