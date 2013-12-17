# entry - the trade's entry
# exit - the trade's exit
# pos - long (1) or short (-1)
# stop.loss - the stop loss, NA if none
# stop.trailing - a trailing stop, NA if none
# profit.target - a profit targe, NA if none
# max.days - the maximum number of days for this trade, 0 if none
#
# if both stop.loss and stop.trailing are specified, the stop.trailing is used
process.trade <- function(
                     op,
                     hi,
                     lo,
                     cl,
                     entry,
                     exit,
                     pos,
                     stop.loss=NA,
                     stop.trailing=NA,
                     profit.target=NA,
                     max.days=0) {
   # the lower level c++ interface uses ordinary indexes for the trade's entry and exit
   tmp = op[,1]
   tmp[] = 1:NROW(tmp)

   ibeg = as.numeric(tmp[entry])
   iend = as.numeric(tmp[exit])

   return(process.trade.interface(
               op, hi, lo, cl,
               ibeg, iend, pos,
               stopLoss=stop.loss,
               stopTrailing=stop.trailing,
               profitTarget=profit.target,
               maxDays=max.days))
}

# trades is a data frame - easier to extract the vectors in R. the format is:
#     entry | exit | position | stop.loss | stop.trailing | profit.target | max.days
# where:
#     entry - the trade's entry
#     exit - the trade's exit
#     position - long (1) or short (-1)
#     stop.loss - the stop loss, NA if none
#     stop.trailing - a trailing stop, NA if none
#     profit.target - a profit targe, NA if none
#     max.days - maximum days to stay in the trade, less or equal to 0 if none
# if both stop.loss and stop.trailing are specified, the stop.trailing is used
process.trades <- function(ohlc, trades) {
   # the lower level c++ interface uses ordinary indexes for the trade's entry and exit
   tmp = ohlc[,1]
   tmp[] = 1:NROW(tmp)
   
   stopifnot(NCOL(trades) < 3)

   if(NCOL(trades) < 4) {
      # Append a stop loss column
      trades = cbind(trades, rep(NA, NROW(trades)))
   }

   if(NCOL(trades) < 5) {
      # Append a trailing stop column
      trades = cbind(trades, rep(NA, NROW(trades)))
   }

   if(NCOL(trades) < 6) {
      # Append a profit target column
      trades = cbind(trades, rep(NA, NROW(trades)))
   }
   
   if(NCOL(trades) < 7) {
      # Append a max days column
      trades = cbind(trades, rep(0, NROW(trades)))
   }

   ibeg = as.integer(tmp[trades[,1]])
   iend = as.integer(tmp[trades[,2]])
   
   res = process.trades.interface(
               ohlc,          # OHLC
               ibeg,          # start index
               iend,          # end index
               trades[,3],    # position
               trades[,4],    # stop loss
               trades[,5],    # stop trailing
               trades[,6],    # profit target
               trades[,7])    # max days

   # print(head(res))
   res = data.frame(res)
   
   # convert back from ordinary indexes to time indexes
   ohlc.index = index(ohlc)
   res[,1] = ohlc.index[res[,1]]
   res[,2] = ohlc.index[res[,2]]

   return(res)
}

# given a signal (weights) as an xts, returns trades as a data frame:
#     entry | exit | position
trades.from.signal = function(sig) {
   res = trades.from.signal.interface(sig)
   res = data.frame(res)
   sig.index = index(sig)
   res[,1] = sig.index[res[,1]]
   res[,2] = sig.index[res[,2]]
   return(res)
}

# trades a signal with the same stop/profit settings for all trades
simple.trade.signal = function(ohlc, signal, stop.loss=NA, stop.trailing=NA, profit.target=NA, max.days=0) {
   trades = trades.from.signal(signal)
   trades[,4] = rep(stop.loss, nrow(trades))
   trades[,5] = rep(stop.trailing, nrow(trades))
   trades[,6] = rep(profit.target, nrow(trades))
   trades[,7] = rep(max.days, nrow(trades))
   colnames(trades) = c("Entry", "Exit", "Position", "StopLoss", "StopTrailing", "ProfitTarget", "MaxDays")
   res = process.trades(ohlc, trades)
   return(res)
}

get.returns = function(trades, returns) {
   # the lower level c++ interface uses ordinary indexes for the trade's entry and exit
   tmp = returns[,1]
   tmp[] = 1:NROW(tmp)
   
   ibeg = as.integer(tmp[trades[,1]])
   iend = as.integer(tmp[trades[,2]])

   return(reclass(get.returns.interface(ibeg, iend, as.integer(trades[,3]), returns), returns))
}
