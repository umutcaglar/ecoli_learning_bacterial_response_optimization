#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
double F1ScoreErrCppCorrected(StringVector y, StringVector prediction) // The function to predict F1 Score
{
  
  int ySize = y.size
  int predictionSize = y.prediction
  int combSize = ySize + predictionSize
  StringVector combined(combSize);
  
  
  printf("\n");
  printf("combined\n");
  print(combined);
  
  StringVector unique_y = unique(y);
  int counterMax = unique_y.size();
  int stringLength = y.size();
  
  DoubleVector TP_vec(counterMax);
  DoubleVector FP_vec(counterMax);
  DoubleVector FN_vec(counterMax);
  DoubleVector PRE_vec(counterMax);
  DoubleVector REC_vec(counterMax);
  double beta = 1;
  
  for (int counter01=0; counter01 < counterMax; ++counter01) 
  {
    StringVector selectedValue(1);
    selectedValue[0] = unique_y[counter01];
    StringVector selectedVector = rep_each(selectedValue,stringLength);
    
    double TP = sum((y == selectedVector) & (prediction == selectedVector));
    double FP = sum((y != selectedVector) & (prediction == selectedVector));
    double FN = sum((y == selectedVector) & (prediction != selectedVector));
    
    TP_vec[counter01]=TP;
    FP_vec[counter01]=FP;
    FN_vec[counter01]=FN;
    
    PRE_vec[counter01] = (double) TP/(double)(TP+FP);
    REC_vec[counter01] = (double) TP/(double)(TP+FN);
  }
  
printf("\n");
printf("TP\n");
print(TP_vec);

printf("\n");
printf("FP\n");
print(FP_vec);

printf("\n");
printf("FN\n");
print(FN_vec);

printf("\n");
printf("Precision\n");
print(PRE_vec);

printf("\n");
printf("Recall\n");
print(REC_vec);
  
DoubleVector PRE_vec_ = na_omit(PRE_vec);
DoubleVector REC_vec_ = na_omit(REC_vec);
    
printf("\n");
printf("Precision_\n");
print(PRE_vec_);

printf("\n");
printf("Recall_\n");
print(REC_vec_);

double PRE_m = mean(PRE_vec_);
double REC_m = mean(REC_vec_);
double Fscore_m = ((beta*beta+1)*PRE_m*REC_m)/((beta*beta)*PRE_m+REC_m);
double F1_err = 1-Fscore_m;
  
return F1_err;
}
