load("unitTests/drm.RData")

test.process.trade = function() {
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1)
   
   # 0 - exit on the last day
   checkEqualsNumeric(0, df$exit.reason, "001: Bad exit.reason", tolerance=0)

   gain = as.numeric(Cl(drm[5225])) / as.numeric(Cl(drm[5205])) - 1
   checkEqualsNumeric(gain, df$gain, "002: Bad gain", tolerance=0)
   
   # For this specific subsequence, we know that the min and the max are not on the first Close
   max.price = as.numeric(max(Hi(drm[5206:5225])))
   min.price = as.numeric(min(Lo(drm[5206:5225])))
   mae = min.price / as.numeric(Cl(drm[5205])) - 1
   mfe = max.price / as.numeric(Cl(drm[5205])) - 1
   checkEqualsNumeric(mae, df$mae, tolerance=0.00001)
   checkEqualsNumeric(mfe, df$mfe, tolerance=0.00001)
   
   # Add a stop loss which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, 0.05)
   
   # 0 - exit on the last day
   checkEqualsNumeric(0, df$exit.reason, "003: Bad exit.reason", tolerance=0)
   
   # Add a stop loss which is hit at the low
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, 0.01)
   
   # 3 - stop limit on low
   checkEqualsNumeric(3, df$exit.reason, "004: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(-0.01, df$gain, "005: Bad gain")
   checkEqualsNumeric(5206, df$exit.index, "006: Bad exit.index")
   
   # Add a stop loss which is hit at the open
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, 0.007)
   
   # 1 - stop limit on open
   checkEqualsNumeric(1, df$exit.reason, "007: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(as.numeric(Op(drm[5206])) / as.numeric(Cl(drm[5205])) - 1, df$gain, "008: Bad gain")
   checkEqualsNumeric(5206, df$exit.index, "009: Bad exit.index")
   
   # Add a trailing stop loss which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, NA, 0.05)
   
   # 0 - exit on the last day
   checkEqualsNumeric(0, df$exit.reason, "010: Bad exit.reason", tolerance=0)
   
   # Add a trailing stop loss which is  hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, 1, NA, 0.05)

   # 6 - stop trailing on low
   checkEqualsNumeric(6, df$exit.reason, "011: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5213, df$exit.index, "012: Bad exit.index", tolerance=0)
   checkEqualsNumeric(164.92, df$exit.price, "013: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(-0.0067, round(df$gain, 4), "014: Bad gain")
   checkEqualsNumeric(-0.0067, round(df$mae, 4), "015: Bad mae")
   checkEqualsNumeric(0.04553, round(df$mfe, 5), "016: Bad mfe")
   
#    print(df$exit.reason)
#    print(df$exit.index)
#    print(df$exit.price)
#    print(df$gain)
#    print(df$mae)
#    print(df$mfe)
}