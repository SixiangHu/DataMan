#include <Rcpp.h>
#include <math.h> 
#include <utility>
#include <map>
#include <stdio.h>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
double CramersV_C(IntegerVector x,IntegerVector y, bool Bias_Cor){
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
    // only 1 unique value then return 1
    return 1;
  }
  else if (unique_x==1 && unique_y==1){
    // if there is only 2 unique value then use special method.
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
        Exy = (double)counts_x.find(uni_x[i])->second * (double)counts_y.find(uni_y[j])->second / (double)n ;
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
  
  if (Bias_Cor) {
    chisq = std::max((double)0,chisq-(double)(unique_x)*(unique_y)/(n-1) );
    unique_x = unique_x - (unique_x*unique_x) /(n-1);
    unique_y = unique_y - (unique_y*unique_y) /(n-1);
  }
  return std::sqrt(abs(chisq)/((double)n * std::min(unique_x, unique_y)));
}

// [[Rcpp::export]]
Rcpp::NumericMatrix CramersV_DF(Rcpp::IntegerMatrix dm, bool Bias_Cor) {
  int iCol = dm.ncol();
  int i=0,j=0;
  
  Rcpp::NumericMatrix ResCV(iCol,iCol);
  
  for (i=0;i<iCol;i++){
    for (j=i;j<iCol;j++){
        if(i==j) {ResCV(i,j)=1;}
        else {
          ResCV(i,j) = CramersV_C((IntegerVector)dm(_,i),(IntegerVector)dm(_,j),(bool)Bias_Cor);
          ResCV(j,i) = ResCV(i,j);
        }
    }
  }

  Rcpp::List dimnms = Rcpp::List::create(VECTOR_ELT(dm.attr("dimnames"), 1),
                                         VECTOR_ELT(dm.attr("dimnames"), 1));
  ResCV.attr("dimnames")=dimnms;

  return ResCV;
}