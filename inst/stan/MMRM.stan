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

    {% if covariance == "ar1" %}
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
    {% endif %}
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

    {% if covariance == "us" %}    
        array[G] matrix[n_visit, n_visit] Sigma_par; // covariance matrix
    {% else if covariance == "ar1" %}
        array[G] real<lower={{ machine_double_eps }}> sd_par; // standard deviation
        array[G] real<lower=-1, upper=1> rho_par; // correlation
    {% endif %}
}


transformed data {
   matrix[P, P] R_inverse = inverse(R);
}


parameters {
    vector[P] theta;              // coefficients of linear model on covariates
    {% if covariance == "us" %}
        {% if prior_cov == "default" %}
            array[G] cov_matrix[n_visit] Sigma; // covariance matrix(s)
        {% else if prior_cov == "lkj" %}
            array[G] cholesky_factor_corr[n_visit] corr_chol; // Cholesky factors for correlation matrix
            array[G] vector<lower={{ machine_double_eps }}>[n_visit] vars; // one variance for each visit
        {% endif %}
    {% else if covariance == "ar1" %}
        array[G] real<lower=-1,upper=1> rho; // AR(1) correlation coefficient
        array[G] real<lower={{ machine_double_eps }}> var_const; // homogeneous variance across visits
    {% endif %}
}

{% if covariance != "us" or prior_cov != "default" %}
transformed parameters {
    array[G] cov_matrix[n_visit] Sigma;
    
    {% if covariance == "us" and prior_cov == "lkj" %}
        array[G] vector<lower={{ machine_double_eps }}>[n_visit] sds;
        for (g in 1:G) {
            sds[g] = sqrt(vars[g]); 
        }
    {% endif %}

    for(g in 1:G){
        {% if covariance == "us" and prior_cov == "lkj" %}
            Sigma[g] = multiply_lower_tri_self_transpose(diag_pre_multiply(sds[g], corr_chol[g]));
        {% else if covariance == "ar1" %}
            Sigma[g] = var_const[g] * ar1_correlation_matrix(n_visit, rho[g]);
        {% endif %}
    }
}
{% endif %}

model {
    int data_start_row = 1;
    
    vector[N] mu = Q * theta;
    
    for(g in 1:G){
        {% if covariance == "us" %}  
            {% if prior_cov == "default" %}
                Sigma[g] ~ inv_wishart(n_visit+2, Sigma_par[g]);
            {% else if prior_cov == "lkj" %}
                corr_chol[g] ~ lkj_corr_cholesky(1.0); // LKJ prior on correlation matrix
                for(i in 1:n_visit) {
                    // Note that we pass the estimated sigma, not sigma^2 here as 
                    // the scale parameter.
                    vars[g][i] ~ scaled_inv_chi_square(1, sqrt(Sigma_par[g][i,i]));
                }
            {% endif %}            
        {% else if covariance == "ar1" %}
            // We use an implicit uniform prior on rho.
            // Note that we pass the estimated sd, not sd^2 here as 
            // the scale parameter of the scaled inverse Chi-Square distribution.
            var_const[g] ~ scaled_inv_chi_square(1, sd_par[g]);
        {% endif %}
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
