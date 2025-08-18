// Heterogeneous Toeplitz covariance model with default prior.

functions {
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
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] sds_par; // Standard deviations for each visit.
    array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos_par; // Correlation prior parameters.
}

parameters {
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] vars; // Variances for each visit.
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
    for(g in 1:G) {
        rhos[g] ~ uniform(-1, 1);

        // Note that we need to pass the estimated standard deviation as 
        // the scale parameter.
        // Note also the parallel vectorization in the vars[g]/sds_par[g] elements.
        vars[g] ~ scaled_inv_chi_square(3, sds_par[g]);
    }
}
