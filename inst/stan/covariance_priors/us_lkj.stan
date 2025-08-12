// Unstructured covariance model with LKJ prior.

data {
    array[G] matrix[n_visit, n_visit] Sigma_par; // Used to extract variance prior parameters.
}

transformed data {
   array[G] vector<lower={{ machine_double_eps }}>[n_visit] sds_par; // Standard deviations for each visit.
   for(g in 1:G){
       sds_par[g] = sqrt(diag(Sigma_par[g]));
   }
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
        // Note that this is not vectorized in the matrices so we have to do this
        // inside this loop.
        corr_chol[g] ~ lkj_corr_cholesky(1.0); 
        // Note that we need to pass the estimated sigma, not sigma^2 here as 
        // the scale parameter.
        // Note also the parallel vectorization in the vars[g]/sds_par[g] elements.
        vars[g] ~ scaled_inv_chi_square(3, sds_par[g]);
    }
}