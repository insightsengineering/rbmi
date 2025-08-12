// Toeplitz covariance model with default prior.

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
    array[G] real<lower={{ machine_double_eps }}> sd_par; // Standard deviation prior parameter.
    array[G] vector<lower=-1, upper=1>[n_visit - 1] rhos_par; // Correlation prior parameters.
}

parameters {
    array[G] real<lower={{ machine_double_eps }}> var_const; // Homogeneous variance across visits.
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
    for(g in 1:G) {
        rhos[g] ~ uniform(-1, 1);
    }

    // Note that we pass the estimated sd, not sd^2 here as
    // the scale parameter of the scaled inverse Chi-Square distribution.
    // Note also the parallel vectorization in the var_const/sd_par elements.
    var_const ~ scaled_inv_chi_square(3, sd_par);
}
