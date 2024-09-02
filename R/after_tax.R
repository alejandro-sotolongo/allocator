load('tax-test.RData')
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
    asset_wgt[i, ] <- wgt[reb_counter, ]
    reb_counter <- reb_counter + 1
  }
}
