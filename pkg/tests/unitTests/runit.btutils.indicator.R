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