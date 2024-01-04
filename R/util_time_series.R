cut_time <- function(x, date_start = NULL, date_end = NULL) {
  if (is.null(date_start) & is.null(date_end)) return(x)
  if (is.null(date_start)) return(x[paste0(date_start, '/')])
  if (is.null(date_end)) return(x[paste0('/', date_end)])
  return(x[paste0(date_start, '/', date_end)])
}

clean_ret <- function(x, eps = 0.05) {
  bad_col <- colSums(is.na(x)) / nrow(x) > eps
  miss_dat <- x[, bad_col]
  dat <- x[, !bad_col]
  dat[is.na(dat)] <- 0
  res <- list()
  res$ret <- dat
  res$miss_ret <- miss_dat
  return(res)
} 

is_null_dim <- function(x) {
  if (is.null(dim(x))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

fill_na_price <- function(x) {
  x_fill <- apply(x, 2, na_price)
  xts(x_fill, zoo::index(x))
}

na_price <- function(x) {
  ind <- which(!is.na(x))
  if (is.na(x[1])) {
    ind <- c(1, ind)
  }
  rep(x[ind], times = diff(c(ind, length(x) + 1)))
}
