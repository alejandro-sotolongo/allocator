#' @export
check_reb_freq <- function(x) {
  x <- x[1]
  if (is.null(x)) {
    return(x)
  }
  if (x %in% c('BH', 'D', 'M', 'Q', 'A')) {
    return(x)
  }
  stop(paste0(x, ' not in BH, D, M, Q, A'))
}
