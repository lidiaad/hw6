#include <Rcpp.h>
using namespace Rcpp;

//' @title mode
//' @description Calculates a mode number, which is the most frequently occuring number in a given vector. 
//' If the mode is ambigous returns a smaller number of equal mode.
//' 
//' @param x a vector of integers
//' @return an integer value - mode
//' 
//' @export
// [[Rcpp::export]] 
int mode(IntegerVector x) 
{
  IntegerVector sorted = clone(x);
  R_isort(INTEGER(SEXP(sorted)), sorted.size());
  int a = sorted[0];
  int b = a;
  int counter1 = 0;
  int countermax = 0;
  for (int i = 0; i <= sorted.size(); i++)
  {
    if (sorted[i] == a) 
    {
      counter1 = counter1 + 1;
      if (counter1 > countermax) 
      {
        countermax = counter1;
        b = a;
      }
    }
    else
    {
      counter1 = 1;
      a = sorted[i];
    }
  }
  return b;
}           

