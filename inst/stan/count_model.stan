data {
  int<lower=1> N;                       // Number of patients
  int<lower=1> R;                       // Number of observed cells
  int<lower=1> P;                       // Number of regression coefficients
  int<lower=1> G;                       // Number of dispersion parameter groups
  array[N] int<lower=1, upper=G> group; // Dispersion parameter group by patient
  array[R] int<lower=1, upper=N> subject; // Patient index for each observed cell
  array[R] int<lower=0> y;              // Observed cell counts
  matrix[R, P] X;                       // Cell-level design matrix
  vector[R] log_offset;                 // log cell exposure
}

transformed data {
  array[N] int<lower=0> y_sum;
  for (n in 1:N) {
    y_sum[n] = 0;
  }
  for (r in 1:R) {
    y_sum[subject[r]] += y[r];
  }
}

parameters {
  vector[P] beta;                       // Regression coefficients
  // Values below 1e-6 are effectively the Poisson limit for these models. The
  // floor avoids the extreme near-zero geometry of the Gamma(.0001, .0001)
  // prior while retaining the SAS GENMOD dispersion parameterization.
  vector<lower=1e-6>[G] phi;
}

transformed parameters {
  vector[R] eta = log_offset + X * beta;
  vector<lower=0>[G] inv_phi;

  inv_phi = inv(phi);
}

model {
  vector[N] mu_sum = rep_vector(0, N);

  // No explicit prior for beta, which corresponds to an improper uniform prior, matching SAS defaults.

  // Match SAS default prior on dispersion parameter (shape/rate)
  phi ~ gamma(0.0001, 0.0001);

  for (r in 1:R) {
    mu_sum[subject[r]] += exp(eta[r]);
    target += y[r] * eta[r] - lgamma(y[r] + 1);
  }

  for (n in 1:N) {
    real shape = inv_phi[group[n]];
    target += lgamma(shape + y_sum[n]) - lgamma(shape)
      + shape * log(shape)
      - (shape + y_sum[n]) * log(shape + mu_sum[n]);
  }
}
