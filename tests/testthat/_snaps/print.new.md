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
           trt_visit_3    3.533   0.573  2.794  Inf    1   
         lsm_ref_visit_3  6.772   0.387  6.272  Inf    1   
         lsm_alt_visit_3  10.306  0.427  9.755  Inf    1   
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
      
        ========================================================
            parameter      est     se     lci     uci     pval  
        --------------------------------------------------------
           trt_visit_1    7.313   0.418   <NA>    <NA>    <NA>  
         lsm_ref_visit_1  7.204   0.274   <NA>    <NA>    <NA>  
         lsm_alt_visit_1  14.516  0.315   <NA>    <NA>    <NA>  
           trt_visit_3     7.99   0.212  7.549   8.432   <0.001 
         lsm_ref_visit_3  6.764   0.151  6.443   7.085   <0.001 
         lsm_alt_visit_3  14.754  0.183  14.364  15.145  <0.001 
        --------------------------------------------------------
      

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
           trt_visit_1    7.584   <NA>  -Inf  9.055   0.143 
         lsm_ref_visit_1  6.937   <NA>  -Inf  7.737   0.143 
         lsm_alt_visit_1  14.522  <NA>  -Inf  15.297  0.143 
           trt_visit_2    8.356   <NA>  -Inf   9.31   0.143 
         lsm_ref_visit_2  6.583   <NA>  -Inf  7.426   0.143 
         lsm_alt_visit_2  14.94   <NA>  -Inf  15.62   0.143 
           trt_visit_3    4.397   <NA>  -Inf  4.603   0.143 
         lsm_ref_visit_3  6.891   <NA>  -Inf  7.486   0.143 
         lsm_alt_visit_3  11.287  <NA>  -Inf  12.09   0.143 
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
           trt_visit_1    7.584   0.785  -Inf  8.875   <0.001 
         lsm_ref_visit_1  6.937   0.569  -Inf  7.874   <0.001 
         lsm_alt_visit_1  14.522  0.577  -Inf  15.472  <0.001 
           trt_visit_2    8.356   0.778  -Inf  9.636   <0.001 
         lsm_ref_visit_2  6.583   0.526  -Inf  7.449   <0.001 
         lsm_alt_visit_2  14.94   0.49   -Inf  15.745  <0.001 
           trt_visit_3    4.397   0.495  -Inf  5.211   <0.001 
         lsm_ref_visit_3  6.891   0.397  -Inf  7.544   <0.001 
         lsm_alt_visit_3  11.287  0.61   -Inf  12.29   <0.001 
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
      

