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
  if (all(data[last_row, cols_check] == data[prior_row, cols_check])) {
    return(FALSE)
  }

  # increasing
  if (any(data[last_row, cols_check] < data[prior_row, cols_check])) {
    return(FALSE)
  }

  # increasing < 10%
  if (any(data[last_row, cols_check] / data[prior_row, cols_check] > 1.1)) {
    return(FALSE)
  }

  # totals match
  if (any_nuls) {
    if (!is.na(data[last_row, 13])) {
      return(FALSE)
    }
  } else {
    if (!sum(data[last_row, c(3:12)]) == data[last_row, 13]) {
      return(FALSE)
    }
  }

  # not too much unknown
  if (sum(data[last_row, "UNKNOWN"]) > 100) {
    return(FALSE)
  }

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
  if (all(data[last_row, cols_check] == data[prior_row, cols_check])) {
    return(FALSE)
  }

  # increasing
  if (any(data[last_row, cols_check] < data[prior_row, cols_check])) {
    return(FALSE)
  }

  # increasing < 10%
  if (any(data[last_row, cols_check] / data[prior_row, cols_check] > 1.1)) {
    return(FALSE)
  }

  # totals match
  if (!sum(data[last_row, c(4:5)]) == data[last_row, 3]) {
    return(FALSE)
  }

  # return true
  return(TRUE)
}
