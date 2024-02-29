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
