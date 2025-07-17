# as_stan_fragments works as expected

    Code
      result
    Output
      $data
      [1] "  int<lower=0> N;" "  vector[N] y;"   
      
      $parameters
      [1] "  real mu;"
      
      $model
      [1] "  y ~ normal(mu, 1);"
      
      $functions
      [1] ""
      
      $transformed_parameters
      [1] ""
      

