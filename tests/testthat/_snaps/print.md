# print - Approx Bayes

    Code
      print(drawobj_ab)
    Output
      
      Draws Object
      ------------
      Number of Samples: 5
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: random
      Method:
          Type: Approximate Bayes
          covariance: us
          threshold: 0.5
          same_cov: TRUE
          REML: TRUE
          n_samples: 5
      

---

    Code
      print(impute_ab)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 5
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  54%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(analysis_ab)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 5
      Analysis Function: ancova
      Delta Applied: FALSE
      Analysis Parameters:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_2
          lsm_ref_visit_2
          lsm_alt_visit_2
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

---

    Code
      print(pool_ab)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 5
      Method: rubin
      Confidence Level: 0.9
      Alternative: less
      
      Results:
      
        ===================================================
            parameter      est     se     lci   uci   pval 
        ---------------------------------------------------
           trt_visit_1    7.313   0.418  <NA>   <NA>  <NA> 
         lsm_ref_visit_1  7.204   0.274  <NA>   <NA>  <NA> 
         lsm_alt_visit_1  14.516  0.315  <NA>   <NA>  <NA> 
           trt_visit_2    8.015   0.193  <NA>   <NA>  <NA> 
         lsm_ref_visit_2  6.768   0.126  <NA>   <NA>  <NA> 
         lsm_alt_visit_2  14.784  0.145  <NA>   <NA>  <NA> 
           trt_visit_3    3.595   0.559  2.873  Inf    1   
         lsm_ref_visit_3   6.82   0.375  6.335  Inf    1   
         lsm_alt_visit_3  10.415  0.424  9.867  Inf    1   
        ---------------------------------------------------
      

# print - Bayes

    Code
      print(drawobj_b)
    Output
      
      Draws Object
      ------------
      Number of Samples: 11
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: random
      Method:
          Type: Bayes
          burn_in: 200
          burn_between: 2
          same_cov: TRUE
          n_samples: 11
          verbose: FALSE
          seed: 859
      

---

    Code
      print(impute_b)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 11
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  54%
      References:
          TRT     -> TRT
          Placebo -> Placebo
      

---

    Code
      print(analysis_b)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 11
      Analysis Function: rbmi::ancova
      Delta Applied: TRUE
      Analysis Parameters:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

---

    Code
      print(pool_b)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 11
      Method: rubin
      Confidence Level: 0.95
      Alternative: two.sided
      
      Results:
      
        =======================================================
            parameter      est     se     lci    uci     pval  
        -------------------------------------------------------
           trt_visit_1    7.313   0.418  <NA>    <NA>    <NA>  
         lsm_ref_visit_1  7.204   0.274  <NA>    <NA>    <NA>  
         lsm_alt_visit_1  14.516  0.315  <NA>    <NA>    <NA>  
           trt_visit_3     7.96   0.225  7.485  8.434   <0.001 
         lsm_ref_visit_3  6.812   0.115  6.581  7.044   <0.001 
         lsm_alt_visit_3  14.772  0.183  14.38  15.165  <0.001 
        -------------------------------------------------------
      

# print - condmean (bootstrap)

    Code
      print(drawobj_cmb)
    Output
      
      Draws Object
      ------------
      Number of Samples: 6
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: condmean
      Method:
          Type: Conditional Mean
          covariance: ar1
          threshold: 0.2
          same_cov: TRUE
          REML: TRUE
          n_samples: 6
          type: bootstrap
      

---

    Code
      print(impute_cmb)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 6
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  48%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(analysis_cmb)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 6
      Analysis Function: ancova
      Delta Applied: FALSE
      Analysis Parameters:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_2
          lsm_ref_visit_2
          lsm_alt_visit_2
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

---

    Code
      print(pool_cmb_p)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 6
      Method: bootstrap (percentile)
      Confidence Level: 0.95
      Alternative: greater
      
      Results:
      
        ====================================================
            parameter      est     se   lci    uci    pval  
        ----------------------------------------------------
           trt_visit_1    7.584   <NA>  -Inf  8.645   0.143 
         lsm_ref_visit_1  6.937   <NA>  -Inf  7.599   0.143 
         lsm_alt_visit_1  14.522  <NA>  -Inf  16.244  0.143 
           trt_visit_2    8.356   <NA>  -Inf  9.087   0.143 
         lsm_ref_visit_2  6.583   <NA>  -Inf  6.937   0.143 
         lsm_alt_visit_2  14.94   <NA>  -Inf  16.025  0.143 
           trt_visit_3    4.397   <NA>  -Inf  6.073   0.143 
         lsm_ref_visit_3  6.891   <NA>  -Inf   7.39   0.143 
         lsm_alt_visit_3  11.287  <NA>  -Inf  13.464  0.143 
        ----------------------------------------------------
      

---

    Code
      print(pool_cmb_n)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 6
      Method: bootstrap (normal)
      Confidence Level: 0.95
      Alternative: greater
      
      Results:
      
        ======================================================
            parameter      est     se    lci    uci     pval  
        ------------------------------------------------------
           trt_visit_1    7.584   0.473  -Inf  8.362   <0.001 
         lsm_ref_visit_1  6.937   0.435  -Inf  7.653   <0.001 
         lsm_alt_visit_1  14.522  0.655  -Inf  15.599  <0.001 
           trt_visit_2    8.356   0.411  -Inf  9.033   <0.001 
         lsm_ref_visit_2  6.583   0.227  -Inf  6.956   <0.001 
         lsm_alt_visit_2  14.94   0.414  -Inf  15.621  <0.001 
           trt_visit_3    4.397   0.709  -Inf  5.563   <0.001 
         lsm_ref_visit_3  6.891   0.243  -Inf   7.29   <0.001 
         lsm_alt_visit_3  11.287  0.945  -Inf  12.842  <0.001 
        ------------------------------------------------------
      

# print - Condmean (jackknife)

    Code
      print(drawobj_cmj)
    Output
      
      Draws Object
      ------------
      Number of Samples: 36
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: condmean
      Method:
          Type: Conditional Mean
          covariance: us
          threshold: 0.5
          same_cov: FALSE
          REML: TRUE
          n_samples: NA
          type: jackknife
      

---

    Code
      print(impute_cmj)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 36
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  46%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(analysis_cmj)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 36
      Analysis Function: ancova
      Delta Applied: FALSE
      Analysis Parameters:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_2
          lsm_ref_visit_2
          lsm_alt_visit_2
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

---

    Code
      print(pool_cmj)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 36
      Method: jackknife
      Confidence Level: 0.9
      Alternative: two.sided
      
      Results:
      
        ========================================================
            parameter      est     se     lci     uci     pval  
        --------------------------------------------------------
           trt_visit_1    7.296   0.784  6.006   8.587   <0.001 
         lsm_ref_visit_1  7.318   0.69   6.183   8.454   <0.001 
         lsm_alt_visit_1  14.615  0.695  13.471  15.758  <0.001 
           trt_visit_2    7.363   0.373  6.749   7.977   <0.001 
         lsm_ref_visit_2  7.262   0.514  6.417   8.108   <0.001 
         lsm_alt_visit_2  14.625  0.563  13.699  15.551  <0.001 
           trt_visit_3    4.593   1.063  2.844   6.342   <0.001 
         lsm_ref_visit_3  6.637   0.772  5.366   7.907   <0.001 
         lsm_alt_visit_3  11.229  0.972   9.63   12.829  <0.001 
        --------------------------------------------------------
      

