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
    array[G] matrix[n_visit, n_visit] Sigma_init; // covariance matrix estimated from MMRM
}


transformed data {
   matrix[P, P] R_inverse = inverse(R);
}



parameters {
    vector[P] theta;              // coefficients of linear model on covariates
    array[G] cov_matrix[n_visit] Sigma; // covariance matrix(s)
}


model {
    int data_start_row = 1;
    
    vector[N] mu =  Q * theta;
    
    for(g in 1:G){
        Sigma[g] ~ inv_wishart(n_visit+2, Sigma_init[g]);
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
