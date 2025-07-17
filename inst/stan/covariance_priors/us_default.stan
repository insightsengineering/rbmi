// Unstructured covariance model with default, i.e. inverse Wishart, prior.

data {
    array[G] matrix[n_visit, n_visit] Sigma_par; // Used as center for the inverse Wishart prior.
}

parameters {
    array[G] cov_matrix[n_visit] Sigma; // Covariance matrices per group.
}

model {
    for(g in 1:G){
        Sigma[g] ~ inv_wishart(n_visit+2, Sigma_par[g]);
    }
}