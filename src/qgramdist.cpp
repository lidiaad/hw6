#include <Rcpp.h>

using namespace Rcpp;

//' @title qgramdist
//' @description Calculates a q-gram distance \url{https://journal.r-project.org/archive/2014-1/loo.pdf} 
//' between two vectors of integers.
//' 
//' @param x a vector of single (<10 if q>1) integers
//' @param y a vector of single (<10 if q>1) integers
//' @param q an integer, q in q-gram distance
//' @return an integer value - q-gram distance, Inf if q is larger then a vector's length
//' 
//' @export
// [[Rcpp::export]] 
double qgramdist(IntegerVector x, IntegerVector y, int q) //returned value is double because Inf is double
{
  if(q < 1)
  throw Rcpp::exception("Invalid q parameter");
  if((q > x.size()) || (q > y.size())) 
  return R_PosInf;
  
  int sizex = x.size() - q + 1;
  int sizey = y.size() - q + 1;
  IntegerVector tmpx(sizex);
  IntegerVector tmpy(sizey);
  tmpy[0] = 0;
  for (int i = 0; i < sizex; ++i)
  {
    int j = q;
    tmpx[i] = 0;
    while(j > 0)
    {
      tmpx[i] = (pow(10, (j-1))*x[i+q-j]) + tmpx[i];
      j = j - 1;
    }
  }
  //redundancy in code is meaningless here
  for (int i = 0; i < sizey; ++i)
  {
    int j = q;
    tmpy[i] = 0;
    while(j > 0)
    {
      tmpy[i] = (pow(10, (j-1))*y[i+q-j]) + tmpy[i];
      j = j - 1;
    }
  }
  
  int dist = 0;
  for (int i = 0; i < sizex; ++i)
  {
    bool p = 0;
    for(int j = 0; j < sizey; ++j)
    {
      if(tmpx[i] == tmpy[j])
      {
        tmpx.erase(i);
        tmpy.erase(j);
        sizex--;
        sizey--;
        p = 1;
        i--;
        break;
      }
      
    }
    if (p == 0) 
    {
      dist++;
    }
  }
  
  for (int i = 0; i < sizey; ++i)
  {
    bool p = 0;
    for(int j = 0; j < sizex; ++j)
    {
      if(tmpy[i] == tmpx[j])
      {
        p = 1;
      }
      
    }
    if (p == 0) 
    {
      dist++;
    }
  }
  return dist;
}    