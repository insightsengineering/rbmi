# as_stan_fragments works as expected

    Code
      result
    Output
      $data
      [1] "  int<lower=0> N;" "  vector[N] y;"   
      
      $parameters
      [1] "  real mu;"
      
      $model
      [1] "  y ~ normal(mu, 1);"
      
      $functions
      [1] ""
      
      $transformed_parameters
      [1] ""
      

# get_stan_model works as expected depending on covariance and prior on parameters

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_us_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
      
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] matrix[n_visit, n_visit] Sigma_par; // Used as center for the inverse Wishart prior.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] cov_matrix[n_visit] Sigma; // Covariance matrices per group.
      }
      transformed parameters {
      
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              Sigma[g] ~ inv_wishart(n_visit+2, Sigma_par[g]);
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_ar1_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the AR(1) correlation matrix of dimension n with correlation rho.
          matrix ar1_correlation_matrix(int n, real rho) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in 1:n) {
                      L[i, j] = rho^abs(j - i);
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] real<lower=2.2204e-16> sd_par; // Standard deviation prior parameter.
          array[G] real<lower=-1, upper=1> rho_par; // Correlation prior parameter.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] real<lower=-1,upper=1> rho; // AR(1) correlation coefficient.
          array[G] real<lower=2.2204e-16> var_const; // Homogeneous variance across visits.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and homogeneous variance.
          for(g in 1:G){
              Sigma[g] = var_const[g] * ar1_correlation_matrix(n_visit, rho[g]);
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rho[g] ~ uniform(-1, 1);
              // Note that we pass the estimated sd, not sd^2 here as
              // the scale parameter of the scaled inverse Chi-Square distribution.
              var_const[g] ~ scaled_inv_chi_square(3, sd_par[g]);
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_ar1h_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the AR(1) correlation matrix of dimension n with correlation rho.
          matrix ar1_correlation_matrix(int n, real rho) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in 1:n) {
                      L[i, j] = rho^abs(j - i);
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] vector<lower=2.2204e-16>[n_visit] sds_par; // Standard deviations for each visit.
          array[G] real<lower=-1, upper=1> rho_par; // Correlation prior parameter.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] vector<lower=2.2204e-16>[n_visit] vars; // Variances for each visit.
          array[G] real<lower=-1,upper=1> rho; // AR(1) correlation coefficient.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and variances vector.
          for(g in 1:G){
              Sigma[g] = quad_form_diag(
                  ar1_correlation_matrix(n_visit, rho[g]),
                  sqrt(vars[g])
              );
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rho[g] ~ uniform(-1, 1);
              // Scaled inverse chi-square priors for the variances.
              for(i in 1:n_visit) {
                  // Note that we need to pass the estimated standard deviation as
                  // the scale parameter.
                  vars[g][i] ~ scaled_inv_chi_square(3, sds_par[g][i]);
              }
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_cs_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the compound symmetry correlation matrix of dimension n with correlation rho.
          matrix cs_correlation_matrix(int n, real rho) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in 1:n) {
                      L[i, j] = (i == j) ? 1 : rho;
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] real<lower=2.2204e-16> sd_par; // Standard deviation prior parameter.
          array[G] real<lower=-1/(n_visit - 1), upper=1> rho_par; // Correlation prior parameter.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] real<lower=-1/(n_visit - 1), upper=1> rho; // Compound symmetry correlation coefficient.
          array[G] real<lower=2.2204e-16> var_const; // Homogeneous variance across visits.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and homogeneous variance.
          for(g in 1:G){
              Sigma[g] = var_const[g] * cs_correlation_matrix(n_visit, rho[g]);
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rho[g] ~ uniform(-1/(n_visit - 1), 1);
              // Note that we pass the estimated sd, not sd^2 here as
              // the scale parameter of the scaled inverse Chi-Square distribution.
              var_const[g] ~ scaled_inv_chi_square(3, sd_par[g]);
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_csh_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the compound symmetry correlation matrix of dimension n with correlation rho.
          matrix cs_correlation_matrix(int n, real rho) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in 1:n) {
                      L[i, j] = (i == j) ? 1 : rho;
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] vector<lower=2.2204e-16>[n_visit] sds_par; // Standard deviations for each visit.
          array[G] real<lower=-1/(n_visit - 1), upper=1> rho_par; // Correlation prior parameter.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] vector<lower=2.2204e-16>[n_visit] vars; // Variances for each visit.
          array[G] real<lower=-1,upper=1> rho; // Compound symmetry correlation coefficient.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and variances vector.
          for(g in 1:G){
              Sigma[g] = quad_form_diag(
                  cs_correlation_matrix(n_visit, rho[g]),
                  sqrt(vars[g])
              );
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rho[g] ~ uniform(-1/(n_visit - 1), 1);
              // Scaled inverse chi-square priors for the variances.
              for(i in 1:n_visit) {
                  // Note that we need to pass the estimated standard deviation as
                  // the scale parameter.
                  vars[g][i] ~ scaled_inv_chi_square(3, sds_par[g][i]);
              }
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_ad_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the antedependence correlation matrix of dimension n with
          // (n-1) correlation parameters rhos.
          matrix ad_correlation_matrix(int n, vector rhos) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in i:n) {
                      if (i == j) {
                          L[i, j] = 1;
                      } else {
                          L[i, j] = prod(rhos[i:(j-1)]);
                          L[j, i] = L[i, j];
                      }
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] real<lower=2.2204e-16> sd_par; // Standard deviation prior parameter.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos_par; // Correlation prior parameters.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] real<lower=2.2204e-16> var_const; // Homogeneous variance across visits.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos; // Antedependence correlation coefficients.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and homogeneous variance.
          for(g in 1:G){
              Sigma[g] = var_const[g] * ad_correlation_matrix(n_visit, rhos[g]);
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rhos[g] ~ uniform(-1, 1);
              // Note that we pass the estimated sd, not sd^2 here as
              // the scale parameter of the scaled inverse Chi-Square distribution.
              var_const[g] ~ scaled_inv_chi_square(3, sd_par[g]);
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_adh_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the antedependence correlation matrix of dimension n with
          // (n-1) correlation parameters rhos.
          matrix ad_correlation_matrix(int n, vector rhos) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in i:n) {
                      if (i == j) {
                          L[i, j] = 1;
                      } else {
                          L[i, j] = prod(rhos[i:(j-1)]);
                          L[j, i] = L[i, j];
                      }
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] vector<lower=2.2204e-16>[n_visit] sds_par; // Standard deviations for each visit.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos_par; // Correlation prior parameters.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] vector<lower=2.2204e-16>[n_visit] vars; // Variances for each visit.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos; // Antedependence correlation coefficients.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and variances vector.
          for(g in 1:G){
              Sigma[g] = quad_form_diag(
                  ad_correlation_matrix(n_visit, rhos[g]),
                  sqrt(vars[g])
              );
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rhos[g] ~ uniform(-1, 1);
              // Scaled inverse chi-square priors for the variances.
              for(i in 1:n_visit) {
                  // Note that we need to pass the estimated standard deviation as
                  // the scale parameter.
                  vars[g][i] ~ scaled_inv_chi_square(3, sds_par[g][i]);
              }
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_toep_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the Toeplitz correlation matrix of dimension n with
          // (n-1) correlation parameters rhos.
          matrix toep_correlation_matrix(int n, vector rhos) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in 1:n) {
                      if (i == j) {
                          L[i, j] = 1;
                      } else {
                          L[i, j] = rhos[abs(i - j)];
                      }
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] real<lower=2.2204e-16> sd_par; // Standard deviation prior parameter.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos_par; // Correlation prior parameters.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] real<lower=2.2204e-16> var_const; // Homogeneous variance across visits.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos; // Toeplitz correlation coefficients.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and homogeneous variance.
          for(g in 1:G){
              Sigma[g] = var_const[g] * toep_correlation_matrix(n_visit, rhos[g]);
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rhos[g] ~ uniform(-1, 1);
              // Note that we pass the estimated sd, not sd^2 here as
              // the scale parameter of the scaled inverse Chi-Square distribution.
              var_const[g] ~ scaled_inv_chi_square(3, sd_par[g]);
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_toeph_default' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
              // Create the Toeplitz correlation matrix of dimension n with
          // (n-1) correlation parameters rhos.
          matrix toep_correlation_matrix(int n, vector rhos) {
              matrix[n, n] L;
              for (i in 1:n) {
                  for (j in 1:n) {
                      if (i == j) {
                          L[i, j] = 1;
                      } else {
                          L[i, j] = rhos[abs(i - j)];
                      }
                  }
              }
              return L;
          }
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] vector<lower=2.2204e-16>[n_visit] sds_par; // Standard deviations for each visit.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos_par; // Correlation prior parameters.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] vector<lower=2.2204e-16>[n_visit] vars; // Variances for each visit.
          array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos; // Toeplitz correlation coefficients.
      }
      transformed parameters {
              array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from correlation and variances vector.
          for(g in 1:G){
              Sigma[g] = quad_form_diag(
                  toep_correlation_matrix(n_visit, rhos[g]),
                  sqrt(vars[g])
              );
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              rhos[g] ~ uniform(-1, 1);
              // Scaled inverse chi-square priors for the variances.
              for(i in 1:n_visit) {
                  // Note that we need to pass the estimated standard deviation as
                  // the scale parameter.
                  vars[g][i] ~ scaled_inv_chi_square(3, sds_par[g][i]);
              }
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

---

    Code
      model
    Output
      S4 class stanmodel 'rbmi_MMRM_us_lkj' coded as follows:
      functions {
          int integer_division(int a, int b) {
              // perform a/b ensuring return value is also an int
              int i = 0;
              while(b*(i+1) <= a) {
                  i = i + 1;
              }
              return(i);
          }
      
          array[] vector to_vector_of_arrays(vector vec, int length_array) {
              // treansform a vector into a vector of arrays. Example: vec = [1,2,3,4,5,6] and
              // length_array = 2, then output = [1,2; 3,4; 5,6]
              array[integer_division(num_elements(vec),length_array)] vector[length_array] res;
              int j = 1;
              int i = 1;
              while(j <= num_elements(vec)) {
                  res[i,] = vec[j:(j+length_array-1)];
                  i = i+1;
                  j = j + length_array;
              }
              return(res);
          }
      
      }
      data {
          int<lower=1> N;                         // number of observations
          int<lower=1> P;                         // number of covariates (number of columns of design matrix)
          int<lower=1> G;                         // number of Sigma Groups
          int<lower=1> n_visit;                   // number of visits
          int<lower=1> n_pat;                     // number of pat groups (number of missingness patterns * groups)
      
          array[n_pat] int<lower=1> pat_G;              // Index for which Sigma the pat group should use
          array[n_pat] int<lower=1> pat_n_pt;           // number of patients in each pat group
          array[n_pat] int<lower=1> pat_n_visit;        // number of non-missing visits in each pat group
          array[n_pat, n_visit] int<lower=1> pat_sigma_index;    // rows/cols from sigma to subset on for the pat group
      
          vector[N] y;                            // outcome variable
          matrix[N,P] Q;                          // design matrix (After QR decomp)
          matrix[P,P] R;                          // R matrix (from QR decomp)
              array[G] matrix[n_visit, n_visit] Sigma_par; // Used to extract variance prior parameters.
      }
      transformed data {
         matrix[P, P] R_inverse = inverse(R);
      }
      parameters {
          vector[P] theta;              // coefficients of linear model on covariates
              array[G] cholesky_factor_corr[n_visit] corr_chol; // Cholesky factors for correlation matrix.
          array[G] vector<lower=2.2204e-16>[n_visit] vars; // One variance for each visit.
      }
      transformed parameters {
              array[G] vector<lower=2.2204e-16>[n_visit] sds;
          array[G] cov_matrix[n_visit] Sigma;
      
          // Construct covariance matrix from Cholesky factors and variances.
          for(g in 1:G){
              sds[g] = sqrt(vars[g]);
              Sigma[g] = multiply_lower_tri_self_transpose(diag_pre_multiply(sds[g], corr_chol[g]));
          }
      }
      model {
          int data_start_row = 1;
      
          vector[N] mu = Q * theta;
      
              for(g in 1:G){
              corr_chol[g] ~ lkj_corr_cholesky(1.0); // LKJ prior on correlation matrix.
              // Scaled inverse chi-square priors for the variances.
              for(i in 1:n_visit) {
                  // Note that we need to pass the estimated sigma, not sigma^2 here as
                  // the scale parameter.
                  vars[g][i] ~ scaled_inv_chi_square(3, sqrt(Sigma_par[g][i,i]));
              }
          }
      
          for(i in 1:n_pat) {
              // Index + size variables for current pat group
              int nvis = pat_n_visit[i]; // number of visits
              int npt = pat_n_pt[i];     // number of patients
              int g = pat_G[i];          // Sigma index
      
              // Get required/reduced Sigma for current pat group
              array[nvis] int sig_index = pat_sigma_index[i, 1:nvis];
              matrix[nvis,nvis] sig = Sigma[g][sig_index, sig_index];
      
              // Derive data indcies for current pat group
              int data_stop_row = data_start_row + ((nvis * npt)  -1);
      
              // Extract required data for the current pat group
              array[npt] vector[nvis] y_obs = to_vector_of_arrays(y[data_start_row:data_stop_row], nvis);
              array[npt] vector[nvis] mu_obs = to_vector_of_arrays(mu[data_start_row:data_stop_row], nvis);
      
              y_obs ~ multi_normal(mu_obs, sig);
      
              // Update data index for next pat group
              data_start_row = data_stop_row + 1;
          }
      }
      generated quantities {
         vector[P] beta = R_inverse * theta;
      } 

