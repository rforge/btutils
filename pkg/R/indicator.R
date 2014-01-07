cap.trade.duration = function(indicator, short.duration.cap=-1, long.duration.cap=-1) {
   return(reclass(cap.trade.duration.interface(indicator, short.duration.cap, long.duration.cap), indicator))
}