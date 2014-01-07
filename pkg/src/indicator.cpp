#include <Rcpp.h>
#include "common.h"

using namespace Rcpp;

void capTradeDuration(std::vector<double> & indicator, int shortDurationCap, int longDurationCap)
{
   if(shortDurationCap < 0 && longDurationCap < 0) return;

   std::vector<double>::size_type ii = 0;

   // Skip leading NAs
   while(ii < indicator.size() && isNA(indicator[ii])) ++ii;
   
   while(ii < indicator.size()) {
      // Find the beginning of a position
      while(ii < indicator.size() && indicator[ii] == 0) ++ii;
      
      if(ii == indicator.size()) break;
      
      // Apply caps to this position
      int ss = sign(indicator[ii]);
      int cap;
      if(ss == -1 && shortDurationCap >= 0) {
         cap = shortDurationCap;
      } else if(ss == 1 && longDurationCap >= 0) {
         cap = longDurationCap;
      } else {
         cap = -1;
      }

      if(cap != -1) {
         int daysIn = 1;
         while(ii < indicator.size() && sign(indicator[ii]) == ss) {
            if(daysIn > cap) {
               indicator[ii] = 0;
            }
            
            ++daysIn;
            ++ii;
         }
      } else {
         while(ii < indicator.size() && sign(indicator[ii]) == ss) ++ii;
      }
   }
}

// [[Rcpp::export("cap.trade.duration.interface")]]
Rcpp::NumericVector capTradeDurationInterface(SEXP indicatorIn, int shortDurationCap, int longDurationCap)
{
   // Convert ohlc into std vectors
   std::vector<double> indicator = Rcpp::as< std::vector<double> >(indicatorIn);
   capTradeDuration(indicator, shortDurationCap, longDurationCap);

   return Rcpp::NumericVector(indicator.begin(), indicator.end());
}