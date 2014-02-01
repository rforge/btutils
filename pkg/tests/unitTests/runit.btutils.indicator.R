require(quantmod)
require(RUnit)

require(btutils)

load("unitTests/indicator.RData")

test.cap.trade.duration = function() {
   # Noop
   checkEqualsNumeric(cap.trade.duration(indicator), indicator)
   
   # No shorts
   ind = cap.trade.duration(indicator, short.duration.cap=0)
   checkTrue(all(ind >= 0, na.rm=T))
   
   # No longs
   ind = cap.trade.duration(indicator, long.duration.cap=0)
   checkTrue(all(ind <= 0, na.rm=T))
   
   # Limit short positions
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, short.duration.cap=2))))
   checkTrue(all(rr$lengths[which(rr$values==-1, arr.ind=T)] <= 2, na.rm=T))
   
   # Limit long positions
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, long.duration.cap=2))))
   checkTrue(all(rr$lengths[which(rr$values==1, arr.ind=T)] <= 2, na.rm=T))
   
   # Limit both
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, long.duration.cap=2, short.duration.cap=3))))
   checkTrue(all(rr$lengths[which(rr$values==1, arr.ind=T)] <= 2, na.rm=T))
   checkTrue(all(rr$lengths[which(rr$values==-1, arr.ind=T)] <= 3, na.rm=T))
}

test.indicator.from.trendline = function() {
   trendline = c(1, 2, 3, 2, 3, 1, 2)
   checkEqualsNumeric(indicator.from.trendline(trendline), c(0, 1, 1, -1, 1, -1, 1), tolerance=0, msg=" *** test 1")

   thresholds = rep(0, NROW(trendline))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 1, 1, -1, 1, -1, 1), tolerance=0, msg=" *** test 2")

   thresholds = rep(1, NROW(trendline))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 1, 1, -1, 1, -1, 1), tolerance=0, msg=" *** test 3")

   thresholds = rep(1.1, NROW(trendline))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 1, 1, 1, 1, -1, -1), tolerance=0, msg=" *** test 4")

   trendline = c(NA, NA, NA, 1, 2, 3, 2, 3, 1, 2)
   thresholds = rep(1.1, NROW(trendline))
   # print(indicator.from.trendline(trendline, thresholds))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 0, 0, 0, 1, 1, 1, 1, -1, -1), tolerance=0, msg=" *** test 5")
}