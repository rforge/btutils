#ifndef COMMON_H_INCLUDED
#define COMMON_H_INCLUDED

#include <Rcpp.h>

// This needs to be changed if the c++ code is used outside R
inline bool isNA(double d) { return R_IsNA(d); }

template <typename T> inline int sign(T t) {
   return (T(0) < t) - (t < T(0));
}

#endif // COMMON_H_INCLUDED