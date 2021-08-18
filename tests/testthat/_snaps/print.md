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
           trt_visit_3    3.487   0.573  2.747  Inf    1   
         lsm_ref_visit_3  6.819   0.389  6.315  Inf    1   
         lsm_alt_visit_3  10.306  0.433  9.746  Inf    1   
        ---------------------------------------------------
      

# print - Bayes

    Code
      print(drawobj_b)
    Output
      
      Draws Object
      ------------
      Number of Samples: 11
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
           trt_visit_3    8.054   0.223  7.585   8.523   <0.001 
         lsm_ref_visit_3  6.732   0.174  6.353    7.11   <0.001 
         lsm_alt_visit_3  14.785  0.126  14.533  15.038  <0.001 
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
           trt_visit_1    7.584   <NA>  -Inf  7.935   0.143 
         lsm_ref_visit_1  6.937   <NA>  -Inf  7.733   0.143 
         lsm_alt_visit_1  14.522  <NA>  -Inf  15.292  0.143 
           trt_visit_2    8.356   <NA>  -Inf  8.365   0.143 
         lsm_ref_visit_2  6.583   <NA>  -Inf  7.566   0.143 
         lsm_alt_visit_2  14.94   <NA>  -Inf  15.231  0.143 
           trt_visit_3    4.397   <NA>  -Inf  4.397   0.143 
         lsm_ref_visit_3  6.891   <NA>  -Inf  7.604   0.143 
         lsm_alt_visit_3  11.287  <NA>  -Inf  11.575  0.143 
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
           trt_visit_1    7.584   0.869  -Inf  9.014   <0.001 
         lsm_ref_visit_1  6.937   0.305  -Inf   7.44   <0.001 
         lsm_alt_visit_1  14.522  0.788  -Inf  15.818  <0.001 
           trt_visit_2    8.356   0.592  -Inf   9.33   <0.001 
         lsm_ref_visit_2  6.583   0.397  -Inf  7.237   <0.001 
         lsm_alt_visit_2  14.94   0.386  -Inf  15.575  <0.001 
           trt_visit_3    4.397   0.76   -Inf  5.647   <0.001 
         lsm_ref_visit_3  6.891   0.293  -Inf  7.373   <0.001 
         lsm_alt_visit_3  11.287  0.657  -Inf  12.367  <0.001 
        ------------------------------------------------------
      

# print - Condmean (jackknife)

    Code
      print(drawobj_cmj)
    Output
      
      Draws Object
      ------------
      Number of Samples: 61
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: condmean
      Method:
          Type: Conditional Mean
          covariance: us
          threshold: 0.5
          same_cov: FALSE
          REML: TRUE
          n_samples: 10
          type: jackknife
      

---

    Code
      print(impute_cmj)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 61
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  52%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(analysis_cmj)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 61
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
      Number of Results Combined: 61
      Method: jackknife
      Confidence Level: 0.9
      Alternative: two.sided
      
      Results:
      
        ========================================================
            parameter      est     se     lci     uci     pval  
        --------------------------------------------------------
           trt_visit_1    7.403   0.627  6.371   8.435   <0.001 
         lsm_ref_visit_1  6.688   0.663  5.598   7.778   <0.001 
         lsm_alt_visit_1  14.091  0.522  13.233  14.95   <0.001 
           trt_visit_2    7.647   0.307  7.142   8.152   <0.001 
         lsm_ref_visit_2   6.72   0.444  5.991    7.45   <0.001 
         lsm_alt_visit_2  14.368  0.377  13.748  14.988  <0.001 
           trt_visit_3    3.023   0.732  1.819   4.226   <0.001 
         lsm_ref_visit_3  6.761   0.423  6.065   7.457   <0.001 
         lsm_alt_visit_3  9.784   0.883  8.332   11.236  <0.001 
        --------------------------------------------------------
      

