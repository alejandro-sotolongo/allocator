
# load('tax-test.RData')

# db <- Database$new(api_file = '/home/rstudio/invdb/api_keys.RData')
# db$read_all_ret()
# xdf <- read_parquet(db$bucket$path('xdf.parquet'))
# mdf <- merge_msl(xdf, db$msl)
# wgt <- pivot_wider(mdf$match, id_cols = returnInfo, names_from = DTCName, values_from = pctVal)
# wgt[is.na(wgt)] <- 0
# wgt <- df_to_xts(wgt)
# wgt <- wgt / rowSums(wgt)
# 
# ix <- match(colnames(wgt), colnames(db$ret$d))
# r <- db$ret$d[, ix]

#' @export
rebal <- function(wgt, r, gain_thresh) { 
  
  cr <- ret_to_price(r)
  
  cost_basis <- matrix(nrow = nrow(wgt) + 1, ncol = ncol(wgt))
  shares <- matrix(nrow = nrow(wgt) + 1, ncol = ncol(wgt))
  
  asset_idx <- matrix(nrow = nrow(r) + 1, ncol = ncol(r))
  
  asset_idx[1, ] <- 100 * wgt[1, ]
  asset_wgt <- matrix(nrow = nrow(r), ncol = ncol(r))
  
  cost_basis[1, ] <- cr[1, ]
  shares[1, ] <- asset_idx[1, ] / cost_basis[1, ]
  
  reb_counter <- 1
  
  reb_dt <- as.Date(zoo::index(wgt))
  ret_dt <- as.Date(zoo::index(r))
  
  stock_list <- list()
  day_1_buys <- wgt[1, wgt[1, ] > 0]
  for (i in 1:ncol(day_1_buys)) {
    xdf <- data.frame(
      Date = zoo::index(r)[1],
      BasisPerShare = 1,
      SharesBought = 100 * day_1_buys[, i],
      SharesSold = 0,
      CurrentShares = 100 * day_1_buys[, i]
    )
    colnames(xdf) <- c('Date', 'BasisPerShare', 'SharesBought', 'SharesSold',
                       'CurrentShares')
    stock_list[[colnames(day_1_buys)[i]]] <- xdf
  }

  for (i in 1:nrow(r)) {
    # grow assets by end of previous period's value * current period's return
    asset_idx[i + 1, ] <- asset_idx[i, ] * (1 + r[i, ])
    asset_wgt[i, ] <- asset_idx[i, ] / sum(asset_idx[i, ])
    # check two conditions to see if we need to rebalance:
    # 1) are we on, or did we just pass a rebalance date (in the case where the
    #    rebalance weight was specified on a weekend)
    # 2) using a counter each time we rebalance to move on to the next rebalance
    #    date to check, did we exhaust all rebalance days (to avoid putting a
    #    rebalance index that exceeds the number of rebalance days)
    if (reb_counter <= length(reb_dt) & reb_dt[reb_counter] <= ret_dt[i]) {
      # assumption is we rebalance at the beginning of period i, so the target
      # value is the previous period's portfolio value * current period's target weight
      tgt_asset <- as.numeric(sum(asset_idx[i, ])) * as.numeric(wgt[reb_counter, ])
      val_delta <- tgt_asset - asset_idx[i, ]
      is_sell <- val_delta < 0
      sell_value <- val_delta[is_sell]
      sell_name <- colnames(wgt)[is_sell]
      val_sold <- rep(0, length(sell_name))
      if (any(is_sell)) {
        for (stock in 1:length(sell_name)) {
          xdf <- stock_list[[sell_name[stock]]]
          xdf <- xdf[order(xdf$BasisPerShare, decreasing = FALSE), ]
          price <- cr[i, sell_name[stock]]
          for (basis in 1:nrow(xdf)) {
            if (abs(val_sold[stock] + sell_value[stock]) < 0.0001) {
              break
            }
            obs <- xdf[basis, ]
            if (obs$CurrentShares <= 0.00001) {
              next
            }
            if (price - gain_thresh <= obs$BasisPerShare) {
              shares_sold <- min(obs$CurrentShares, -sell_value[stock] / price)
              xdf[basis, 'SharesSold'] <- obs$SharesSold + shares_sold
              xdf[basis, 'CurrentShares'] <- obs$CurrentShares - shares_sold
              val_sold[stock] <- val_sold[stock] + shares_sold * price
              stock_list[[sell_name[stock]]] <- xdf
            }
          }
        }
      }
      tot_sold <- sum(val_sold)
      if (tot_sold == 0) {
        reb_counter <- reb_counter + 1
        next
      }
      is_buy <- val_delta > 0
      buy_value <- val_delta[is_buy] / sum(val_delta[is_buy]) * tot_sold
      buy_name <- colnames(wgt)[is_buy]
      stock <- 1
      for (stock in 1:length(buy_name)) {
        xdf <- stock_list[[buy_name[stock]]]
        new_buy <- data.frame(
          Date = zoo::index(r)[i],
          BasisPerShare = cr[i, buy_name[stock]],
          SharesBought = buy_value[stock] / cr[i, buy_name[stock]],
          SharesSold = 0,
          CurrentShares = buy_value[stock] / cr[i, buy_name[stock]]
        )
        colnames(new_buy) <- c('Date', 'BasisPerShare', 'SharesBought', 
                               'SharesSold', 'CurrentShares')
        xdf <- rbind(xdf, new_buy)
        stock_list[[buy_name[stock]]] <- xdf
      }
      val_delta[is_sell] <- -val_sold
      val_delta[is_buy] <- buy_value
      tgt_asset <- asset_idx[i, ] + val_delta
      # if we need to buy stock (value delta > 0) then keep track of cost basis
      # per share approximated by the cumulative return of the stock
      cost_basis[reb_counter + 1, val_delta > 0] <- cr[i, val_delta > 0]
      # shares are added or subtracted based on the value needed to be bought or 
      # sold divided by the approximate value per share from the cumulative return
      shares[reb_counter + 1, ] <- shares[reb_counter, ] + val_delta / cr[i, ]
      # set the next periods beginning value to the target rebalance and then
      # grow the assets by the period's return
      asset_idx[i + 1, ] <- tgt_asset * (1 + r[i, ])
      # TO-DO check weight with contr to ret
      asset_wgt[i, ] <- asset_idx[i, ] / sum(asset_idx[i, ])
      reb_counter <- reb_counter + 1
      print(i)
    }
  }
  res <- list()
  res$asset_wgt <- asset_wgt
  res$asset_idx <- asset_idx
  res$stock_list <- stock_list
  return(res)
}