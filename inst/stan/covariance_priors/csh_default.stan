// Heterogeneous compound symmetry covariance model with default prior.

functions {
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
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] sds_par; // Standard deviations for each visit.
    array[G] real<lower=-1/(n_visit - 1), upper=1> rho_par; // Correlation prior parameter.
}

parameters {
    array[G] vector<lower={{ machine_double_eps }}>[n_visit] vars; // Variances for each visit.
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
    // iid uniform prior for rho.
    rho ~ uniform(-1/(n_visit - 1), 1);
    for(g in 1:G){
        // Note that we need to pass the estimated standard deviation as 
        // the scale parameter.
        // Note also the parallel vectorization in the vars[g]/sds_par[g] elements.
        vars[g] ~ scaled_inv_chi_square(3, sds_par[g]);
    }
}