// AR1 covariance model with default prior.

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
    array[G] real<lower={{ machine_double_eps }}> sd_par; // Standard deviation prior parameter.
    array[G] real<lower=-1, upper=1> rho_par; // Correlation prior parameter.
}

parameters {
    array[G] real<lower=-1,upper=1> rho; // AR(1) correlation coefficient.
    array[G] real<lower={{ machine_double_eps }}> var_const; // Homogeneous variance across visits.
}

transformed parameters {
    array[G] cov_matrix[n_visit] Sigma;
    
    // Construct covariance matrix from correlation and homogeneous variance.
    for(g in 1:G){
        Sigma[g] = var_const[g] * ar1_correlation_matrix(n_visit, rho[g]);
    }
}

model {
    for(g in 1:G){
        rho[g] ~ uniform(-1, 1);
        // Note that we pass the estimated sd, not sd^2 here as 
        // the scale parameter of the scaled inverse Chi-Square distribution.
        var_const[g] ~ scaled_inv_chi_square(3, sd_par[g]);
    }
}