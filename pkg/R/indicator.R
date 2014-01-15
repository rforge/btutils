cap.trade.duration = function(indicator, short.duration.cap=-1, long.duration.cap=-1) {
   return(reclass(cap.trade.duration.interface(indicator, short.duration.cap, long.duration.cap), indicator))
}

construct.indicator = function(long.entries, long.exits, short.entries, short.exits) {
   return(reclass(construct.indicator.interface(long.entries, long.exits, short.entries, short.exits), long.entries))
}
