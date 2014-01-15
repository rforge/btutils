//  Copyright (c) 2013-2014, Ivan Popivanov
//  
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//  
//      Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//  
//      Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in
//      the documentation and/or other materials provided with the
//      distribution.
//  
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

void constructIndicator(
         const std::vector<bool> & longEntries,
         const std::vector<bool> & longExits,
         const std::vector<bool> & shortEntries,
         const std::vector<bool> & shortExits,
         std::vector<double> & indicator)
{
   indicator.resize(longEntries.size(), 0.0);

   int ii = 0;

   while(ii < indicator.size() && !longEntries[ii] && !shortEntries[ii]) ++ii;

   int pos = 0;
   while(ii < indicator.size()) {
      switch(pos) {
         case -1:
            if(longEntries[ii]) pos = 1;
            else if(shortExits[ii]) pos = 0;
            break;
            
         case 0:
            if(longEntries[ii]) pos = 1;
            else if(shortEntries[ii]) pos = -1;
            break;
            
         case 1:
            if(shortEntries[ii]) pos = -1;
            else if(longExits[ii]) pos = 0;
            break;
      }
      
      indicator[ii++] = pos;
   }
}

// [[Rcpp::export("construct.indicator.interface")]]
Rcpp::NumericVector constructIndicatorInterface(SEXP longEntriesIn, SEXP longExitsIn, SEXP shortEntriesIn, SEXP shortExitsIn)
{
   std::vector<bool> longEntries = Rcpp::as<std::vector<bool> >(longEntriesIn);
   std::vector<bool> longExits  = Rcpp::as<std::vector<bool> >(longExitsIn);
   std::vector<bool> shortEntries = Rcpp::as<std::vector<bool> >(shortEntriesIn);
   std::vector<bool> shortExits  = Rcpp::as<std::vector<bool> >(shortExitsIn);
   
   std::vector<double> indicator;
   constructIndicator(longEntries, longExits, shortEntries, shortExits, indicator);

   return Rcpp::NumericVector(indicator.begin(), indicator.end());
}
