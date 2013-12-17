#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <cassert>

using namespace Rcpp;

namespace
{
   char buf[4096];
}

// This needs to be changed if the c++ code is used outside R
inline bool isNA(double d) { return R_IsNA(d); }

#define EXIT_ON_LAST             0
#define STOP_LIMIT_ON_OPEN       1
#define STOP_LIMIT_ON_HIGH       2
#define STOP_LIMIT_ON_LOW        3
#define STOP_TRAILING_ON_OPEN    4
#define STOP_TRAILING_ON_HIGH    5
#define STOP_TRAILING_ON_LOW     6
#define STOP_TRAILING_ON_CLOSE   7
#define PROFIT_TARGET_ON_OPEN    8
#define PROFIT_TARGET_ON_HIGH    9
#define PROFIT_TARGET_ON_LOW    10
#define MAX_DAYS_LIMIT          11

void debugMessageFunc(const char * str)
{
   FILE * file = fopen("/home/ivannp/ttt/debug.txt", "a");
   if(file != NULL)
   {
      fprintf(file, "%s\n", str);
      fclose(file);
   }
}

#define DEBUG

#ifdef DEBUG
#define DEBUG_MSG(ss) debugMessageFunc((ss))
#else
#define DEBUG_MSG(ss)
#endif

// The actual workhorse used by the interface functions
void processTrade(
         const std::vector<double> & op,
         const std::vector<double> & hi,
         const std::vector<double> & lo,
         const std::vector<double> & cl,
         int ibeg,
         int iend,
         int pos,
         double stopLoss,
         double stopTrailing,
         double profitTarget,
         int maxDays,
         int & exitIndex,
         double & exitPrice,
         int & exitReason,
         double & gain,
         double & mae,  // maximum adverse excursion
         double & mfe)  // maximum favorable excursion
{
   double entryPrice;
   double stopPrice;
   double targetPrice;
   double minPrice;
   double maxPrice;

   int ii;

   bool hasStopLoss = false;
   bool hasStopTrailing = false;
   bool hasProfitTarget = false;

   DEBUG_MSG("processTrade: entered");
   char buf[4096];
   snprintf(buf,
            sizeof(buf),
            "%d - %d: stopLoss = %f, stopTrailing = %f, profitTarget = %f", 
            ibeg, iend, stopLoss, stopTrailing, profitTarget);
   DEBUG_MSG(buf);

   // Currently positions are initiated only at the close
   minPrice = maxPrice = entryPrice = cl[ibeg];
   
   if(pos < 0) {
      // Short position
      if(!isNA(stopTrailing)) {
         hasStopTrailing = true;
         stopPrice = entryPrice*(1.0 + std::abs(stopLoss));
      } else if(!isNA(stopLoss)) {
         hasStopLoss = true;
         stopPrice = entryPrice*(1.0 + std::abs(stopLoss));
      }

      if(!isNA(profitTarget)) {
         targetPrice = entryPrice*(1.0 - std::abs(profitTarget));
         hasProfitTarget = true;
      }
      
      for(ii = ibeg + 1; ii <= iend; ++ii) {
         // Stop trailing orders and stop loss orders are mutually
         // exclusive, with stop trailing having precedence.
         if(hasStopTrailing) {
            if(op[ii] > stopPrice ) {
               exitPrice = op[ii];
               exitReason = STOP_TRAILING_ON_OPEN;

               // Update min and max price
               if(op[ii] < minPrice) minPrice = op[ii];
               if(op[ii] > maxPrice) maxPrice = op[ii];

               break;
            } 
            
            // No exit at the open, chech whether we need to update the trailing stop
            if(op[ii] < minPrice) {
               minPrice = op[ii];
               stopPrice = minPrice*(1.0 + std::abs(stopTrailing));
            }

            // Next check the high
            if(hi[ii] > stopPrice) {
               exitPrice = stopPrice;
               exitReason = STOP_TRAILING_ON_HIGH;
               
               // Update max price. We are making the assumption that the high happened
               // before the low. Thus, we don't need to update the min price.
               if(stopPrice > maxPrice) maxPrice = stopPrice;

               break;
            }
            
            // Before handling the close, update the stop if necessary
            if(lo[ii] < minPrice) {
               minPrice = lo[ii];
               stopPrice = minPrice*(1.0 - std::abs(stopTrailing));
            }
            
            // Check the close
            if(cl[ii] > stopPrice) {
               exitPrice = stopPrice;
               exitReason = STOP_TRAILING_ON_CLOSE;
               
               // Update max price, min price is already up-to-date (the preivous if-statement)
               if(stopPrice > maxPrice) maxPrice = stopPrice;
               
               break;
            }
         } else if(hasStopLoss) {
            if(op[ii] > stopPrice ) {
               exitPrice = op[ii];
               exitReason = STOP_LIMIT_ON_OPEN;
               
               // Update min and max price
               if(op[ii] < minPrice) minPrice = op[ii];
               if(op[ii] > maxPrice) maxPrice = op[ii];

               break;
            } else if(hi[ii] > stopPrice) {
               exitPrice = stopPrice;
               exitReason = STOP_LIMIT_ON_HIGH;

               // Update min and max price
               if(lo[ii] < minPrice) minPrice = lo[ii];
               if(stopPrice > maxPrice) maxPrice = stopPrice;
               
               break;
            }
         }

         // Profit target is checked after stop orders
         if(hasProfitTarget) {
            if(op[ii] < targetPrice) {
               exitPrice = op[ii];
               exitReason = PROFIT_TARGET_ON_OPEN;
                                          
               // Update min and max price
               if(op[ii] < minPrice) minPrice = op[ii];
               if(op[ii] > maxPrice) maxPrice = op[ii];

               break;
            } else if(lo[ii] < targetPrice) {
               exitPrice = targetPrice;
               exitReason = PROFIT_TARGET_ON_LOW;
                                          
               // Update min and max price
               if(targetPrice < minPrice) minPrice = targetPrice;
               if(hi[ii] > maxPrice) maxPrice = hi[ii];

               break;
            }
         }

         // Update min and max price
         if(lo[ii] < minPrice) minPrice = lo[ii];
         if(hi[ii] > maxPrice) maxPrice = hi[ii];

         // Maximum days for the trade reached
         if(maxDays > 0 && (ii - ibeg) == maxDays) {
            exitPrice = cl[ii];
            exitReason = MAX_DAYS_LIMIT;
            
            break;
         }
      }
      
      if(ii > iend) {
         exitPrice = cl[iend];
         exitReason = EXIT_ON_LAST;
         
         // Update min and max price
         if(lo[iend] < minPrice) minPrice = lo[iend];
         if(hi[iend] > maxPrice) maxPrice = hi[iend];
         
         ii = iend;
      }

      gain = 1.0 - exitPrice/entryPrice;

      mae = 1.0 - maxPrice/entryPrice;
      mfe = 1.0 - minPrice/entryPrice;
   } else {
      // Long position
      if(!isNA(stopTrailing)) {
         hasStopTrailing = true;
         stopPrice = entryPrice*(1.0 - std::abs(stopTrailing));
      } else if(!isNA(stopLoss)) {
         hasStopLoss = true;
         stopPrice = entryPrice*(1.0 - std::abs(stopLoss));
      }

      if(!isNA(profitTarget)) {
         hasProfitTarget = true;
         targetPrice = entryPrice*(1.0 + std::abs(profitTarget));
      }
      
      for(ii = ibeg + 1; ii <= iend; ++ii) {
         if(hasStopTrailing) {
            if(op[ii] < stopPrice) {
               exitPrice = op[ii];
               exitReason = STOP_TRAILING_ON_OPEN;

               // Update min and max price
               if(op[ii] < minPrice) minPrice = op[ii];
               if(op[ii] > maxPrice) maxPrice = op[ii];

               break;
            }

            // No exit on the open. Check whether we need to update
            // the trailing stop taking the open into account.
            if(op[ii] > maxPrice) {
               maxPrice = op[ii];
               stopPrice = maxPrice * (1.0 - std::abs(stopTrailing));
            }

            // Next check the low
            if(lo[ii] < stopPrice) {
               exitPrice = stopPrice;
               exitReason = STOP_TRAILING_ON_LOW;
               
               // Update min price. We are making the assumption that the low happened
               // before the high. Thus, we don't need to update the max price.
               if(stopPrice < minPrice) minPrice = stopPrice;

               break;
               
            }

            // Before handling the close, update the stop if necessary
            if(hi[ii] > maxPrice) {
               maxPrice = hi[ii];
               stopPrice = maxPrice*(1.0 - std::abs(stopTrailing));
            }
            
            // Check the close
            if(cl[ii] < stopPrice) {
               exitPrice = stopPrice;
               exitReason = STOP_TRAILING_ON_CLOSE;
               
               // Update min price, max price is already up-to-date (the preivous if-statement)
               if(stopPrice < minPrice) minPrice = stopPrice;
               
               break;
            }
         } else if(hasStopLoss) {
            if(op[ii] < stopPrice ) {
               exitPrice = op[ii];
               exitReason = STOP_LIMIT_ON_OPEN;
               
               // Update min and max price
               if(op[ii] < minPrice) minPrice = op[ii];
               if(op[ii] > maxPrice) maxPrice = op[ii];

               break;
            } else if(lo[ii] < stopPrice) {
               exitPrice = stopPrice;
               exitReason = STOP_LIMIT_ON_LOW;

               // Update min and max price
               if(stopPrice < minPrice) minPrice = stopPrice;
               if(hi[ii] > maxPrice) maxPrice = hi[ii];

               break;
            }
         }
         
         if(hasProfitTarget) {
            if(op[ii] > targetPrice) {
               exitPrice = op[ii];
               exitReason = PROFIT_TARGET_ON_OPEN;
               
               // Update min and max price
               if(op[ii] < minPrice) minPrice = op[ii];
               if(op[ii] > maxPrice) maxPrice = op[ii];

               break;
            } else if(hi[ii] > targetPrice) {
               exitPrice = targetPrice;
               exitReason = PROFIT_TARGET_ON_HIGH;

               // Update min and max price
               if(lo[ii] < minPrice) minPrice = lo[ii];
               if(targetPrice > maxPrice) maxPrice = targetPrice;

               break;
            }
         }

         // Update min and max price
         if(lo[ii] < minPrice) minPrice = lo[ii];
         if(hi[ii] > maxPrice) maxPrice = hi[ii];
         
         // Maximum days for the trade reached
         if(maxDays > 0 && (ii - ibeg) == maxDays) {
            exitPrice = cl[ii];
            exitReason = MAX_DAYS_LIMIT;
            
            break;
         }
      }

      if(ii > iend) {
         exitPrice = cl[iend];
         exitReason = EXIT_ON_LAST;
         
         // Update min and max price
         if(lo[iend] < minPrice) minPrice = lo[iend];
         if(hi[iend] > maxPrice) maxPrice = hi[iend];

         ii = iend;
      }

      gain = exitPrice/entryPrice - 1.0;

      mae = minPrice/entryPrice - 1.0;
      mfe = maxPrice/entryPrice - 1.0;
   }

   exitIndex = ii;
   
   DEBUG_MSG("processTrade: exited");
}

// [[Rcpp::export("process.trade.interface")]]
Rcpp::List processTradeInterface(
               SEXP opIn,
               SEXP hiIn,
               SEXP loIn,
               SEXP clIn,
               int ibeg,
               int iend,
               int pos,
               double stopLoss,
               double stopTrailing,
               double profitTarget,
               int maxDays)
{
   std::vector<double> op = Rcpp::as< std::vector<double> >(opIn);
   std::vector<double> hi = Rcpp::as< std::vector<double> >(hiIn);
   std::vector<double> lo = Rcpp::as< std::vector<double> >(loIn);
   std::vector<double> cl = Rcpp::as< std::vector<double> >(clIn);

   double exitPrice;
   double gain, mae, mfe;
   int exitIndex;
   int exitReason;
   
   // Call the actuall function to do the work. ibeg and iend are 0 based in cpp and 1 based in R.
   processTrade(
      op, hi, lo, cl,
      ibeg-1, iend-1, pos, stopLoss, stopTrailing, profitTarget, maxDays,
      exitIndex, exitPrice, exitReason, gain, mae, mfe);
   
   // Build and return the result
   return Rcpp::List::create(
                        Rcpp::Named("exit.index") = exitIndex+1,
                        Rcpp::Named("exit.price") = exitPrice,
                        Rcpp::Named("exit.reason") = exitReason,
                        Rcpp::Named("gain") = gain,
                        Rcpp::Named("mae") = mae,
                        Rcpp::Named("mfe") = mfe);
}

void processTrades(
         const std::vector<double> & op,
         const std::vector<double> & hi,
         const std::vector<double> & lo,
         const std::vector<double> & cl,
         const std::vector<int> & ibeg,
         const std::vector<int> & iend,
         const std::vector<int> & position,
         const std::vector<double> & stopLoss,
         const std::vector<double> & stopTrailing,
         const std::vector<double> & profitTarget,
         const std::vector<int> & maxDays,
         std::vector<int> & iendOut,
         std::vector<double> & exitPriceOut,
         std::vector<double> & gainOut,
         std::vector<double> & maeOut,
         std::vector<double> & mfeOut,
         std::vector<int> & exitReasonOut )
{
   DEBUG_MSG("processTrades: entered");

   // The number of rows in the output is known, reserve the space required.
   iendOut.resize(0);
   iendOut.reserve(ibeg.size());

   exitPriceOut.resize(0);
   exitPriceOut.reserve(ibeg.size());

   gainOut.resize(0);
   gainOut.reserve(ibeg.size());

   maeOut.resize(0);
   maeOut.reserve(ibeg.size());

   mfeOut.resize(0);
   mfeOut.reserve(ibeg.size());

   exitReasonOut.resize(0);
   exitReasonOut.reserve(ibeg.size());

   for(int ii = 0; ii < ibeg.size(); ++ii )
   {
      double exitPrice;
      double gain;
      double mae;
      double mfe;
      int exitIndex;
      int exitReason;

      char buf[4096];
      processTrade(
            op, hi, lo, cl,
            ibeg[ii], iend[ii], position[ii], stopLoss[ii], stopTrailing[ii], profitTarget[ii], maxDays[ii],
            exitIndex, exitPrice, exitReason, gain, mae, mfe);
      snprintf(buf, sizeof(buf), "%d: exitIndex = %d, exitPrice = %f, exitReason = %d, gain = %f, mae = %f, mfe = %f", 
               ii, exitIndex, exitPrice, exitReason, gain, mae, mfe);
      DEBUG_MSG(buf);

      iendOut.push_back(exitIndex);
      exitPriceOut.push_back(exitPrice);
      gainOut.push_back(gain);
      maeOut.push_back(mae);
      mfeOut.push_back(mfe);
      exitReasonOut.push_back(exitReason);
   }
   DEBUG_MSG("processTrades: exited");
}

// [[Rcpp::export("process.trades.interface")]]
Rcpp::List processTradesInterface(
                     SEXP ohlcIn,
                     SEXP ibegsIn,
                     SEXP iendsIn,
                     SEXP positionIn,
                     SEXP stopLossIn,
                     SEXP stopTrailingIn,
                     SEXP profitTargetIn,
                     SEXP maxDaysIn)
{
   DEBUG_MSG("processTradesInterface: entered");
   std::vector<int> ibeg = Rcpp::as< std::vector<int> >( ibegsIn );
   std::vector<int> iend = Rcpp::as< std::vector<int> >( iendsIn );
   std::vector<int> position = Rcpp::as< std::vector<int> >( positionIn );
   std::vector<double> stopLoss = Rcpp::as< std::vector<double> >( stopLossIn );
   std::vector<double> stopTrailing = Rcpp::as< std::vector<double> >( stopTrailingIn );
   std::vector<double> profitTarget = Rcpp::as< std::vector<double> >( profitTargetIn );
   std::vector<int> maxDays  = Rcpp::as< std::vector<int> >( maxDaysIn );

   // Convert ohlc into std vectors
   Rcpp::NumericMatrix ohlcMatrix(ohlcIn);
   std::vector<double> op;
   std::vector<double> hi;
   std::vector<double> lo;
   std::vector<double> cl;
   
   int rows = ohlcMatrix.nrow();
   
   op.reserve(rows);
   hi.reserve(rows);
   lo.reserve(rows);
   cl.reserve(rows);
   
   for(int ii = 0; ii < rows; ++ii)
   {
      op.push_back(ohlcMatrix(ii, 0));
      hi.push_back(ohlcMatrix(ii, 1));
      lo.push_back(ohlcMatrix(ii, 2));
      cl.push_back(ohlcMatrix(ii, 3));

   }
   
   assert(false);
   assert(ibeg.size() == iend.size());

   // vectors in c++ are zero based and in R are one based. convert
   // to the c++ format before calling the workhorse function.
   for(int ii = 0; ii < ibeg.size(); ++ii)
   {
      ibeg[ii] -= 1;
      iend[ii] -= 1;
   }

   // Allocate the output vectors
   std::vector<int> iendOut;
   std::vector<double> exitPrice;
   std::vector<double> gain;
   std::vector<double> mae;
   std::vector<double> mfe;
   std::vector<int> reason;

   // Call the c++ function doing the actual work
   processTrades(
         op, hi, lo, cl,
         ibeg, iend, position, stopLoss, stopTrailing, profitTarget, maxDays,
         iendOut, exitPrice, gain, mae, mfe, reason);

   /* Just some values for testing
   for(int ii = 0; ii < ibeg.size(); ++ii )
   {
      iendOut.push_back(iend[ii]);
      exitPrice.push_back(0.0);
      gain.push_back(0.0);
      mae.push_back(0.0);
      mfe.push_back(0.0);
      reason.push_back(0.0);
   }
   */

   // vectors in c++ are zero based and in R are one based.
   // convert to the R format on the way out.
   for(int ii = 0; ii < ibeg.size(); ++ii )
   {
      ibeg[ii] += 1;
      iend[ii] += 1;
      iendOut[ii] += 1;
   }
   
   assert(ibeg.size() == iend.size());
   
   DEBUG_MSG("processTradesInterface: exited");
   /*
   return Rcpp::List::create(
               Rcpp::Named("Entry") = Rcpp::IntegerVector(ibeg.begin(), ibeg.end()),
               Rcpp::Named("Exit") = Rcpp::IntegerVector(iendOut.begin(),iendOut.end()),
               Rcpp::Named("Position") = Rcpp::IntegerVector(position.begin(), position.end()),
               Rcpp::Named("StopLoss") = Rcpp::NumericVector(stopLoss.begin(), stopLoss.end()),
               Rcpp::Named("StopTrailing") = Rcpp::NumericVector(stopTrailing.begin(), stopTrailing.end()),
               Rcpp::Named("ProfitTarget") = Rcpp::NumericVector(profitTarget.begin(), profitTarget.end()),
               Rcpp::Named("ExitPrice") = Rcpp::NumericVector(exitPrice.begin(), exitPrice.end()),
               Rcpp::Named("Gain") = Rcpp::NumericVector(gain.begin(), gain.end()),
               Rcpp::Named("MAE") = Rcpp::NumericVector(mae.begin(), mae.end()),
               Rcpp::Named("MFE") = Rcpp::NumericVector(mfe.begin(), mfe.end()),
               Rcpp::Named("Reason") = Rcpp::IntegerVector(reason.begin(), reason.end()));
   */
   return Rcpp::DataFrame::create(
               Rcpp::Named("Entry") = ibeg,
               Rcpp::Named("Exit") = iendOut,
               Rcpp::Named("Position") = position,
               Rcpp::Named("StopLoss") = stopLoss,
               Rcpp::Named("StopTrailing") = stopTrailing,
               Rcpp::Named("ProfitTarget") = profitTarget,
               Rcpp::Named("ExitPrice") = exitPrice,
               Rcpp::Named("Gain") = gain,
               Rcpp::Named("MAE") = mae,
               Rcpp::Named("MFE") = mfe,
               Rcpp::Named("Reason") = reason);
}

void tradesFromSignal(
         const std::vector<double> & sig,
         std::vector<int> & ibeg,
         std::vector<int> & iend,
         std::vector<int> & position)
{
   // Process the first element
   if(sig[0] != 0)
   {
      ibeg.push_back(0);
      position.push_back(sig[0]);
   }

   // The last index needs special processing
   int lastId = sig.size() - 1;
   
   for(int ii = 1; ii < lastId; ++ii)
   {
      if(sig[ii] != sig[ii-1])
      {
         if(sig[ii-1] != 0)
         {
            // Close the open position
            iend.push_back(ii);
         }
         
         if(sig[ii] != 0)
         {
            // Open a new position
            ibeg.push_back(ii);
            position.push_back(sig[ii]);
         }
      }
   }

   // On the last index we only close an existing open position
   if(ibeg.size() > iend.size())
   {
      iend.push_back(lastId);
   }
   
   assert(iend.size() == ibeg.size());
}

// [[Rcpp::export("trades.from.signal.interface")]]
Rcpp::List tradesFromSignalInterface(SEXP sigIn)
{
   std::vector<double> sig = Rcpp::as< std::vector<double> >( sigIn );
   std::vector<int> ibeg;
   std::vector<int> iend;
   std::vector<int> position;
   tradesFromSignal(sig, ibeg, iend, position);
   
   // vectors in c++ are zero based and in R are one based.
   // convert to the R format on the way out.
   for(int ii = 0; ii < ibeg.size(); ++ii)
   {
      ++ibeg[ii];
      ++iend[ii];
   }
   
   return Rcpp::List::create(
               Rcpp::Named("Entry") = Rcpp::IntegerVector(ibeg.begin(), ibeg.end()),
               Rcpp::Named("Exit") = Rcpp::IntegerVector(iend.begin(), iend.end()),
               Rcpp::Named("Position") = Rcpp::IntegerVector(position.begin(), position.end()));
}

void getReturns(
         const std::vector<double> & returns,
         const std::vector<int> & ibeg,
         const std::vector<int> & iend,
         const std::vector<int> & position,
         std::vector<double> & result)
{
   DEBUG_MSG("getReturns: entered");
   result.resize(0);
   result.resize(returns.size(), 0.0);
   for(int ii = 0; ii < ibeg.size(); ++ii)
   {
      for(int jj = ibeg[ii] + 1; jj <= iend[ii]; ++jj)
      {
         /*
         if(jj < 0 || jj >= returns.size())
         {
            
            snprintf(buf, sizeof(buf), "BAD INDEX = %d", jj);
            DEBUG_MSG(buf);
         }
         */
         result[jj] = position[ii] * returns[jj];
      }
   }
   DEBUG_MSG("getReturns: exited");
}

// [[Rcpp::export("get.returns.interface")]]
Rcpp::NumericVector getReturnsInterface(SEXP ibegIn, SEXP iendIn, SEXP positionIn, SEXP returnsIn)
{
   // Convert ohlc into std vectors
   std::vector<double> returns = Rcpp::as< std::vector<double> >(returnsIn);
   std::vector<int> ibeg = Rcpp::as< std::vector<int> >(ibegIn);
   std::vector<int> iend = Rcpp::as< std::vector<int> >(iendIn);
   std::vector<int> position = Rcpp::as< std::vector<int> >(positionIn);
 
   // c++ uses 0 based indexes
   for(int ii = 0; ii < ibeg.size(); ++ii)
   {
      ibeg[ii] -= 1;
      iend[ii] -= 1;
   }
   
   std::vector<double> result;
   getReturns(returns, ibeg, iend, position, result);

   return Rcpp::NumericVector(result.begin(), result.end());
}