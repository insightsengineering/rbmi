#include /common_stan_code/user_defined_functions.stan

data {
#include /common_stan_code/data_common.stan
    int<lower=1> G;  // number of arms
    int<lower=1> which_arm[N]; // which arm each patient belongs to (levels = from 1 to G)
    matrix[J, J] Sigma_reml[G]; // covariance matrix estimated from MMRM
}

transformed data{
#include /common_stan_code/transformed_data_common.stan
}

parameters {
#include /common_stan_code/regr_coef_def.stan

    cov_matrix[J] Sigma[G]; // covariance matrix
}

model {

#include /common_stan_code/compute_mu.stan

    for(g in 1:G) {
        Sigma[g] ~ inv_wishart(J+2, Sigma_reml[g]);
    }

    for(g in 1:G) {
        matrix[J,J] curr_Sigma = Sigma[g];

        int n_patients = get_number_indexes_same_value(which_arm, g);
        int curr_patients[n_patients] = get_indexes_same_value(which_arm, n_patients, g);
        int curr_M[n_patients] = M[curr_patients];
        vector[J] curr_mu[n_patients] = mu[curr_patients];
        vector[J] curr_y[n_patients] = y[curr_patients];

        for (m in 1:n_missingness_patterns) {

            int curr_y_obs[J] = y_observed[m,]; // get current missingness pattern
            int n_curr_obs_index = sum(curr_y_obs); // number of non-missing components

            if(n_curr_obs_index>0) { // if there is at least one observed data for the current missingness pattern

            int n_patients_same_missingness = get_number_indexes_same_value(curr_M, m);

            if(n_patients_same_missingness > 0) { // if there is at least one patient with the current missingness pattern

            int patients_same_missingness[n_patients_same_missingness]
            = get_indexes_same_value(curr_M, n_patients_same_missingness, m); // get all the patients with current missingness pattern
            int curr_obs_index[n_curr_obs_index] = get_indexes_same_value(curr_y_obs, n_curr_obs_index, 1);

            // access to non-missing components of outcome, mean and covariance matrix
            vector[n_curr_obs_index] y_obs[n_patients_same_missingness] = curr_y[patients_same_missingness, curr_obs_index];
            vector[n_curr_obs_index] mu_obs[n_patients_same_missingness] = curr_mu[patients_same_missingness, curr_obs_index];
            matrix[n_curr_obs_index, n_curr_obs_index] Sigma_obs = get_submatrix_bylogical(curr_Sigma, curr_y_obs);

            y_obs ~ multi_normal(mu_obs, Sigma_obs); // fit multivariate normal on non-missing components.
            }
            }
        }
    }
}

generated quantities {
#include /common_stan_code/regr_coef_recover.stan
}
