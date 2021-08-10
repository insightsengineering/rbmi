int<lower=1> J;  // number of visits
int<lower=1> N;  // number of patients
int<lower=0> P;  // number of covariates (number of columns of design matrix)
int<lower=1> n_missingness_patterns; // number of missingness patterns
int<lower=1> M[N];  // missingness pattern each patient belongs to
vector[J] y[N];  // outcome variable
matrix[N*J,P] Q;  // Q matrix in QR decomposition of the design matrix (intercept + covariates)
matrix[P, P] R; // R matrix in QR decomposition
int<lower=0, upper=1> y_observed[n_missingness_patterns, J]; // missing values detection matrix
