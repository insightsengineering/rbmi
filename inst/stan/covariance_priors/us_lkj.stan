// Unstructured covariance model with LKJ prior.

data {
    array[G] matrix[n_visit, n_visit] Sigma_par; // Used to extract variance prior parameters.
}

parameters {
    array[G] cholesky_factor_corr[n_visit] corr_chol; // Cholesky factors for correlation matrix.
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] vars; // One variance for each visit.
}

transformed parameters {
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] sds;
    array[G] cov_matrix[n_visit] Sigma;
    
    // Construct covariance matrix from Cholesky factors and variances.
    for(g in 1:G){
        sds[g] = sqrt(vars[g]);
        Sigma[g] = multiply_lower_tri_self_transpose(diag_pre_multiply(sds[g], corr_chol[g]));
    }
}

model {
    for(g in 1:G){
        corr_chol[g] ~ lkj_corr_cholesky(1.0); // LKJ prior on correlation matrix.
        // Scaled inverse chi-square priors for the variances.
        for(i in 1:n_visit) {
            // Note that we need to pass the estimated sigma, not sigma^2 here as 
            // the scale parameter.
            vars[g][i] ~ scaled_inv_chi_square(1, sqrt(Sigma_par[g][i,i]));
        }
    }
}