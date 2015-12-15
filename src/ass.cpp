#include <Rcpp.h>
using namespace Rcpp;

unsigned int factorial(unsigned int n) 
{
    if (n == 0)
       return 1;
    return n * factorial(n - 1);
}
IntegerVector fun(int n, int r, IntegerVector arr)
{
  for (int i = n; i >= r; i--) {
    arr[r - 1] = i;
    if (r > 1) 
    { 
      fun(i - 1, r - 1, arr);
    } 
    
  }
  return arr;
}
//' @title ass
//' @description for some given integer n generates all possible 0-1 assignment
//' vectors of 2n survey participants in such a way that exactly n of them are assigned to group 0 (control)
//' and the other n ones are assigned to group 1 (treatment).
//' 
//' @param n an integer, a number of positive responses,
//' @return a matrix with 2n columns and an appropriate
//' number of rows
//' 
//' @export
// [[Rcpp::export]] 
NumericMatrix ass(int n)
{
  NumericMatrix mat(factorial(2*n)/pow((factorial(n)), 2), 2*n);

  for (int i = 0; i < 2*n; i++) 
  {
    for (int j = 0; j < 2*n; j++)
    {
         IntegerVector arr = fun(n, 2*n, mat(i, j));
         mat(i, j) = arr[j];
    }
   }
   return mat;
}
