#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
double F1ScoreErrCpp(StringVector y, StringVector prediction) // The function to predict F1 Score
{
  StringVector unique_y = unique(y);
  int counterMax = unique_y.size();
  int stringLength = y.size();
  
  //NumericVector TP_vec(counterMax);
  //NumericVector FP_vec(counterMax);
  //NumericVector FN_vec(counterMax);
  DoubleVector F1ScoreVector(counterMax);
  
  for (int counter01=0; counter01 < counterMax; ++counter01) 
  {
    StringVector selectedValue(1);
    selectedValue[0] = unique_y[counter01];
    StringVector selectedVector = rep_each(selectedValue,stringLength);
    
    int TP = sum((y == selectedVector) & (prediction == selectedVector));
    int FP = sum((y != selectedVector) & (prediction == selectedVector));
    int FN = sum((y == selectedVector) & (prediction != selectedVector));
    
    //TP_vec[counter01]=TP;
    //FP_vec[counter01]=FP;
    //FN_vec[counter01]=FN;
    
    F1ScoreVector[counter01] = 2*TP/(double)((2*TP)+FP+FN);
  }
  
  //print(TP_vec);
  //print(FP_vec);
  //print(FN_vec);
  //print(F1ScoreVector);
  
  double F1_err = 1-mean(F1ScoreVector);
  
  return F1_err;
}