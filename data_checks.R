
# checks
checks <- function(data) {
  # cases
  last_row = nrow(data)
  prior_row = nrow(data) - 1

  # cols with no nulls
  any_nuls <- any(is.na(data[last_row, c(3:11, 13)]))
  cols_check <-
    c(3:11, 13)[!is.na(data[last_row, c(3:11, 13)]) &
                  !is.na(data[prior_row, c(3:11, 13)])]

  # different
  stopifnot(any(data[last_row, cols_check] > data[prior_row, cols_check]))

  # increasing
  stopifnot(data[last_row, cols_check] >= data[prior_row, cols_check])

  # increasing < 10%
  stopifnot(data[last_row, cols_check] / data[prior_row, cols_check] < 1.1)

  # totals match
  if (any_nuls) {
    stopifnot(is.na(data[last_row, 13]))
  } else {
    stopifnot(sum(data[last_row, c(3:12)]) == data[last_row, 13])
  }

  # not too much unknown
  stopifnot(sum(data[last_row, "UNKNOWN"]) < 100)

  # return true
  return(TRUE)
}

# checks_testing
checks_testing <- function(data) {
  # cases
  last_row = nrow(data)
  prior_row = nrow(data) - 1

  # check if recoveries are blank
  cols_check <-
    c(3:6, 10)[!is.na(data[last_row, c(3:6, 10)]) &
                 !is.na(data[prior_row, c(3:6, 10)])]

  # different
  stopifnot(any(data[last_row, cols_check] > data[prior_row, cols_check]))

  # increasing
  stopifnot(data[last_row, cols_check] >= data[prior_row, cols_check])

  # increasing < 10%
  stopifnot(data[last_row, cols_check] / data[prior_row, cols_check] < 1.1)

  # totals match
  stopifnot(sum(data[last_row, c(4:5)]) == data[last_row, 3])

  # return true
  return(TRUE)
}

