//#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <math.h> 
#include <utility>
#include <map>
#include <algorithm>

//// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double CramersV_C(IntegerVector x,IntegerVector y){
  //Counts the frequency
  std::map<std::pair<int, int>, int> counts_xy;
  std::map<int,int> counts_x;
  std::map<int,int> counts_y;

  int n = x.size();
  int i=0,j=0;
  
  //Read vectors
  for (i = 0; i <= n-1; ++i) {
    counts_x[x[i]]++;
    counts_y[y[i]]++;
    counts_xy[std::make_pair(x[i],y[i])]++;
  }
  
  //Calculates Chi Square Stats
  int unique_x = counts_x.size() -1, unique_y = counts_y.size() -1;
  IntegerVector uni_x = unique(x);
  IntegerVector uni_y = unique(y);
  
  double Exy = 0.0;
  double chisq=0.0;
  int Oxy = 0;
  int a=0,b=0,c=0,d=0;
  
  if (unique_x==0 || unique_y==0) {
    return 1;
  }
  else if (unique_x==1 && unique_y==1){
    if (counts_xy.find(std::make_pair(uni_x[0],uni_y[0])) != counts_xy.end()) {
      a = counts_xy.find(std::make_pair(uni_x[0],uni_y[0]))->second;
    }
    if (counts_xy.find(std::make_pair(uni_x[0],uni_y[1])) != counts_xy.end()) {
      b = counts_xy.find(std::make_pair(uni_x[0],uni_y[1]))->second;
    }
    if (counts_xy.find(std::make_pair(uni_x[1],uni_y[0])) != counts_xy.end()) {
      c = counts_xy.find(std::make_pair(uni_x[1],uni_y[0]))->second;
    }
    if (counts_xy.find(std::make_pair(uni_x[1],uni_y[1])) != counts_xy.end()) {
      d = counts_xy.find(std::make_pair(uni_x[1],uni_y[1]))->second;
    }
    
    Exy = counts_x.find(uni_x[0])->second * counts_x.find(uni_x[1])->second * counts_y.find(uni_y[0])->second * counts_y.find(uni_y[1])->second;
    chisq = (a*d-b*c)* n / Exy;
  }
  else {
    for (i=0;i<=unique_x;++i){
      for (j=0;j<=unique_y;++j){
        Exy = counts_x.find(uni_x[i])->second * counts_y.find(uni_y[j])->second / (double)n;
        if (counts_xy.find(std::make_pair(uni_x[i],uni_y[j])) != counts_xy.end()) {
          Oxy = counts_xy.find(std::make_pair(uni_x[i],uni_y[j]))->second;
        }
        else {
          Oxy=0;
        }
        chisq  = chisq +(Oxy - Exy)*(Oxy - Exy)/Exy;
      }
    }
  }
  
  return std::sqrt(abs(chisq)/((double)n * std::min(unique_x, unique_y)));
}

// [[Rcpp::export]]
Rcpp::NumericMatrix CramersV_DF(Rcpp::IntegerMatrix dm) {
  int iCol = dm.ncol();
  int i=0,j=0;
  
  Rcpp::NumericMatrix ResCV(iCol,iCol);
  
  for (i=0;i<iCol;i++){
    for (j=i+1;j<iCol;j++){
        ResCV(i,j) = CramersV_C((IntegerVector)dm(_,i),(IntegerVector)dm(_,j));
    }
  }
  
  for (i=0;i<iCol;i++){
    for (j=0;j<=i;j++){
      if (i==j) ResCV(i,j) = 1;
      else ResCV(i,j) = ResCV(j,i);
    }
  }
  
  Rcpp::List dimnms = Rcpp::List::create(VECTOR_ELT(dm.attr("dimnames"), 1),
                                         VECTOR_ELT(dm.attr("dimnames"), 1));
  ResCV.attr("dimnames")=dimnms;

  return ResCV;
}