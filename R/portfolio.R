Portfolio <- R6::R6Class(
  'Portfolio',
  public = list(
    name = 'port',
    asset_ret = xts(),
    asset_ret_clean = xts(),
    asset_wgt = xts(),
    perf_stats = NULL,
    frontier = NULL,
    risk_obj = c('vol', 'var', 'cvar', 'te', 'te.var', 'te.cvar'),
    exp_mu = NULL,
    exp_cov = NULL,
    cap_cons = NULL,
    risk_cons = NULL,
    hist_mu = NULL,
    hist_cov = NULL,
    ret_freq = NULL,
    reb_freq = NULL,
    reb_wgt = NULL,
    rf = xts(),
    benchmark = NULL,

    initialize = function(
      name = 'port',
      asset_ret = NULL,
      asset_wgt = NULL,
      risk_obj = c('vol', 'var', 'cvar', 'te', 'te.var', 'te.cvar'),
      exp_mu = NULL,
      exp_cov = NULL,
      cap_cons = NULL,
      risk_cons = NULL,
      hist_mu = NULL,
      hist_cov = NULL,
      ret_freq = c('D', 'M', 'Q', 'A'),
      reb_freq = c(NULL, 'D', 'M', 'Q', 'A'),
      reb_wgt = NULL,
      rf = NULL,
      benchmark = NULL)
    {
      if (is.null(asset_ret)) asset_ret <- xts()
      if (is.null(asset_wgt)) asset_wgt <- xts()
      if (is.null(rf)) rf <- xts()
      self$name <- name
      self$asset_ret <- asset_ret
      self$asset_wgt <- asset_wgt
      self$risk_obj <- risk_obj
      self$exp_mu <- exp_mu
      self$exp_cov <- exp_cov
      self$risk_cons <- risk_cons
      self$hist_mu <- hist_mu
      self$hist_cov <- hist_cov
      self$ret_freq <- ret_freq[1]
      self$reb_freq <- reb_freq[1]
      self$reb_wgt <- reb_wgt
      self$rf <- rf
      self$benchmark <- benchmark
    },

    clean_asset_ret = function(asset_ret = NULL, eps = 0.05) {
      if (is.null(asset_ret)) {
        asset_ret <- self$asset_ret
      }
      if (is.null(asset_ret) | nrow(asset_ret) == 0) {
        stop('no asset returns to clean')
      }
      
    }
    
    lazy_reb_wgt = function(reb_wgt = NULL, reb_freq = NULL, date_start = NULL,
                            date_end = NULL) {

      if (is.null(date_start)) {
        date_start <- zoo::index(self$asset_ret)
        if (is.na(date_start)) {
          stop('need either a date_start or asset_ret')
        }
      }
      if (is.null(date_end)) {
        date_end <- zoo::index(self$asset_ret)[nrow(self$asset_ret)]
        if (is.na(date_end)) {
          stop('need either a date_end or asset_ret')
        }
      }
      date_start <- as.Date(date_start)
      date_end <- as.Date(date_end)
      if (reb_freq == 'D') {
        dt_vec <- zoo::index(self$asset_ret)
        if (length(dt_vec) == 0) {
          stop('D reb_freq specified and missing asset_ret')
        }
      }
      if (reb_freq == 'M') {
        dt_vec <- seq(date_start, date_end, 'months')
        dt_vec <- lubridate::floor_date(dt_vec + 10, 'months') - 1
      }
      if (reb_freq == 'Q') {
        dt_vec <- seq(date_start, date_end, 'quarters')
        dt_vec <- lubridate::floor_date(dt_vec + 10, 'quarters') - 1
      }
      if (reb_freq == 'A') {
        dt_vec <- seq(date_start, date_end, 'years')
      }
    }
  )
)

cut_time <- function(x, date_start = NULL, date_end = NULL) {
  if (is.null(date_start) & is.null(date_end)) return(x)
  if (is.null(date_start)) return(x[paste0(date_start, '/')])
  if (is.null(date_end)) return(x[paste0('/', date_end)])
  return(x[paste0(date_start, '/', date_end)])
}

download_tiingo <- function(ticker, t_api) {
  t_url <- paste0('https://api.tiingo.com/tiingo/daily/',
                  ticker,
                  '/prices?startDate=1970-01-01',
                  '&endDate=2023-09-26',
                  '&token=', t_api)
  dat <- jsonlite::read_json(t_url)
  date_raw <- sapply(dat, '[[', 'date')
  date_vec <- as.Date(date_raw)
  price_vec <- sapply(dat, '[[', 'adjClose')
  price <- xts(price_vec, date_vec)
  colnames(price) <- ticker
  ret <- price / lag.xts(price, 1) - 1
  return(ret[-1, ])
}

clean_ret <- function(x, eps = 0.05) {
  bad_col <- colSums(is.na(x)) / nrow(x) > eps
  miss_dat <- x[, bad_col]
  dat <- x[, !bad_col]
  fill_dat <- dat[is.na(dat)] <- 0
  res <- list()
  res$ret <- fill_dat
  res$miss_ret <- miss_dat
  return(res)
} 