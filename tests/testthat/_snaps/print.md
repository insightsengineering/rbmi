# print - Pool Method

    Code
      print(.test_print$bayes$pool)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 50
      Method: rubin
      Confidence Level: 0.95
      Alternative: two.sided
      
      Results:
      
        ========================================================
            parameter      est     se     lci     uci     pval  
        --------------------------------------------------------
           trt_visit_1    7.253   0.781  5.665   8.842   <0.001 
         lsm_ref_visit_1  7.318   0.574   6.15   8.485   <0.001 
         lsm_alt_visit_1  14.571  0.474  13.607  15.534  <0.001 
           trt_visit_3    7.984   0.258  7.448    8.52   <0.001 
         lsm_ref_visit_3  7.074   0.205  6.642   7.505   <0.001 
         lsm_alt_visit_3  15.058  0.164  14.716   15.4   <0.001 
        --------------------------------------------------------
      

---

    Code
      print(.test_print$approxbayes$pool)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 5
      Method: rubin
      Confidence Level: 0.9
      Alternative: less
      
      Results:
      
        ===================================================
            parameter      est     se     lci    uci  pval 
        ---------------------------------------------------
           trt_visit_1    7.253   0.781  6.232   Inf   1   
         lsm_ref_visit_1  7.318   0.574  6.567   Inf   1   
         lsm_alt_visit_1  14.571  0.474  13.952  Inf   1   
           trt_visit_2    7.406   0.388  6.898   Inf   1   
         lsm_ref_visit_2  7.088   0.285  6.715   Inf   1   
         lsm_alt_visit_2  14.494  0.235  14.186  Inf   1   
           trt_visit_3    5.359   1.092  3.929   Inf   1   
         lsm_ref_visit_3  6.801   0.842  5.693   Inf   1   
         lsm_alt_visit_3  12.16   0.65   11.31   Inf   1   
        ---------------------------------------------------
      

---

    Code
      print(.test_print$condmean_boot$pool$percentile)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 1 + 5
      Method: bootstrap (percentile)
      Confidence Level: 0.95
      Alternative: greater
      
      Results:
      
        =====================================================
            parameter      est     se   lci    uci     pval  
        -----------------------------------------------------
           trt_visit_1    6.643   <NA>  -Inf  7.383   <0.001 
         lsm_ref_visit_1  7.656   <NA>  -Inf  8.126   <0.001 
         lsm_alt_visit_1  14.299  <NA>  -Inf  15.066  <0.001 
           trt_visit_2    6.906   <NA>  -Inf  7.944   <0.001 
         lsm_ref_visit_2  7.364   <NA>  -Inf  7.666   <0.001 
         lsm_alt_visit_2  14.271  <NA>  -Inf  14.923  <0.001 
           trt_visit_3    4.118   <NA>  -Inf  4.257   <0.001 
         lsm_ref_visit_3  7.603   <NA>  -Inf  8.083   <0.001 
         lsm_alt_visit_3  11.721  <NA>  -Inf  11.837  <0.001 
        -----------------------------------------------------
      

---

    Code
      print(.test_print$condmean_boot$pool$normal)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 1 + 5
      Method: bootstrap (normal)
      Confidence Level: 0.95
      Alternative: greater
      
      Results:
      
        ======================================================
            parameter      est     se    lci    uci     pval  
        ------------------------------------------------------
           trt_visit_1    6.643   0.561  -Inf  7.565   <0.001 
         lsm_ref_visit_1  7.656   0.945  -Inf   9.21   <0.001 
         lsm_alt_visit_1  14.299  1.023  -Inf  15.982  <0.001 
           trt_visit_2    6.906   0.852  -Inf  8.308   <0.001 
         lsm_ref_visit_2  7.364   1.015  -Inf  9.034   <0.001 
         lsm_alt_visit_2  14.271  0.817  -Inf  15.614  <0.001 
           trt_visit_3    4.118   0.663  -Inf  5.208   <0.001 
         lsm_ref_visit_3  7.603   0.85   -Inf  9.001   <0.001 
         lsm_alt_visit_3  11.721  1.121  -Inf  13.565  <0.001 
        ------------------------------------------------------
      

---

    Code
      print(.test_print$condmean_jack$pool)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 1 + 35
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
      

---

    Code
      print(.test_print$bmlmi$pool)
    Output
      
      Pool Object
      -----------
      Number of Results Combined: 24
      Method: bmlmi
      Confidence Level: 0.9
      Alternative: two.sided
      
      Results:
      
        ========================================================
            parameter      est     se     lci     uci     pval  
        --------------------------------------------------------
           trt_visit_1    7.039    0.5   6.032   8.047   <0.001 
         lsm_ref_visit_1  7.151   1.257  4.619   9.683   0.002  
         lsm_alt_visit_1  14.19   1.069  12.036  16.345  <0.001 
           trt_visit_2    7.494   0.403  6.681   8.306   <0.001 
         lsm_ref_visit_2  6.842   1.151  4.523    9.16   0.002  
         lsm_alt_visit_2  14.335  0.871  12.581  16.09   <0.001 
           trt_visit_3    4.737   1.142   2.43   7.044   0.009  
         lsm_ref_visit_3  6.743   0.853   5.02   8.465   0.001  
         lsm_alt_visit_3  11.48   1.545  8.365   14.595  0.001  
        --------------------------------------------------------
      

# print - approx bayes

    Code
      print(drawobj_ab)
    Output
      
      Draws Object
      ------------
      Number of Samples: 3
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: random
      Method:
          name: Approximate Bayes
          covariance: ar1
          threshold: 0.5
          same_cov: TRUE
          REML: TRUE
          n_samples: 3
      

---

    Code
      print(impute_ab)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 3
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  42%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(analysis_ab)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 3
      Analysis Function: ancova
      Delta Applied: FALSE
      Analysis Estimates:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_2
          lsm_ref_visit_2
          lsm_alt_visit_2
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

# print - bayesian

    Code
      print(drawobj_b)
    Output
      
      Draws Object
      ------------
      Number of Samples: 50
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: random
      Method:
          name: Bayes
          burn_in: 200
          burn_between: 1
          same_cov: TRUE
          n_samples: 50
          seed: 859
      

---

    Code
      print(impute_b)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 50
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  42%
      References:
          TRT     -> TRT
          Placebo -> Placebo
      

---

    Code
      print(analysis_b)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 50
      Analysis Function: rbmi::ancova
      Delta Applied: TRUE
      Analysis Estimates:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

# print - condmean bootstrap

    Code
      print(drawobj_cmb)
    Output
      
      Draws Object
      ------------
      Number of Samples: 1 + 0
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: condmean
      Method:
          name: Conditional Mean
          covariance: ar1
          threshold: 0.2
          same_cov: TRUE
          REML: TRUE
          type: bootstrap
          n_samples: 0
      

---

    Code
      print(impute_cmb)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 1 + 0
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  42%
      References:
          TRT     -> TRT
          Placebo -> Placebo
      

---

    Code
      print(analysis_cmb)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 1 + 0
      Analysis Function: ancova
      Delta Applied: FALSE
      Analysis Estimates:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_2
          lsm_ref_visit_2
          lsm_alt_visit_2
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

# print - condmean jackknife

    Code
      print(drawobj_cmj)
    Output
      
      Draws Object
      ------------
      Number of Samples: 1 + 35
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: condmean
      Method:
          name: Conditional Mean
          covariance: us
          threshold: 0.5
          same_cov: FALSE
          REML: TRUE
          type: jackknife
      

---

    Code
      print(impute_cmj)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 1 + 35
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
      Number of Results: 1 + 35
      Analysis Function: ancova
      Delta Applied: FALSE
      Analysis Estimates:
          trt_visit_1
          lsm_ref_visit_1
          lsm_alt_visit_1
          trt_visit_2
          lsm_ref_visit_2
          lsm_alt_visit_2
          trt_visit_3
          lsm_ref_visit_3
          lsm_alt_visit_3
      

# print - bmlmi

    Code
      print(drawobj_bml)
    Output
      
      Draws Object
      ------------
      Number of Samples: 6
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: random
      Method:
          covariance: cs
          threshold: 0.05
          same_cov: TRUE
          REML: TRUE
          B: 6
          D: 4
      

---

    Code
      print(impute_bml)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 24
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  42%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(analysis_bml)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 24
      Analysis Function: compare_prop_lastvisit
      Delta Applied: FALSE
      Analysis Estimates:
          trt
      

