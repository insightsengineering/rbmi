#include /common_stan_code/user_defined_functions.stan

data {

    #include /common_stan_code/data_common.stan
    matrix[J, J] Sigma_reml; // covariance matrix estimated from MMRM
}

transformed data{
    #include /common_stan_code/transformed_data_common.stan
}

parameters {
    #include /common_stan_code/regr_coef_def.stan

    cov_matrix[J] Sigma; // covariance matrix
}

model {

    #include /common_stan_code/compute_mu.stan

    Sigma ~ inv_wishart(J+2, Sigma_reml);

    for (m in 1:n_missingness_patterns) {

        int curr_y_obs[J] = y_observed[m,]; // get current missingness pattern
        int n_curr_obs_index = sum(curr_y_obs); // number of non-missing components

        if(n_curr_obs_index>0) { // if there is at least one observed data for the current missingness pattern

        int n_patients_same_missingness = get_number_indexes_same_value(M, m);

        if(n_patients_same_missingness > 0) { // if there is at least one patient with the current missingness pattern
        int curr_obs_index[n_curr_obs_index] = get_indexes_same_value(curr_y_obs, n_curr_obs_index, 1);
        int patients_same_missingness[n_patients_same_missingness]
        = get_indexes_same_value(M, n_patients_same_missingness, m); // get all the patients with current missingness pattern

        // access to non-missing components of outcome, mean and covariance matrix
        vector[n_curr_obs_index] y_obs[n_patients_same_missingness] = y[patients_same_missingness, curr_obs_index];
        vector[n_curr_obs_index] mu_obs[n_patients_same_missingness] = mu[patients_same_missingness, curr_obs_index];
        matrix[n_curr_obs_index, n_curr_obs_index] Sigma_obs = get_submatrix_bylogical(Sigma, curr_y_obs);

        y_obs ~ multi_normal(mu_obs, Sigma_obs); // fit multivariate normal on non-missing components.
        }
        }
    }
}

generated quantities {
    #include /common_stan_code/regr_coef_recover.stan
}
