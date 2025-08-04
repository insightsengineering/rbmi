// Heterogeneous AR1 covariance model with default prior.

functions {
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
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] sds_par; // Standard deviations for each visit.
    array[G] real<lower=-1, upper=1> rho_par; // Correlation prior parameter.
}

parameters {
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] vars; // Variances for each visit.    
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
    for(g in 1:G){
        rho[g] ~ uniform(-1, 1);
        // Scaled inverse chi-square priors for the variances.
        for(i in 1:n_visit) {
            // Note that we need to pass the estimated standard deviation as 
            // the scale parameter.
            vars[g][i] ~ scaled_inv_chi_square(3, sds_par[g][i]);
        }
    }
}