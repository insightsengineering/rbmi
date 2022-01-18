# print - Bayes

    Code
      print(.test_print$bayes$draws)
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
          verbose: TRUE
          seed: 859
      

---

    Code
      print(.test_print$bayes$impute)
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
      print(.test_print$bayes$analysis)
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
      

---

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
      

# print - Approx Bayes

    Code
      print(.test_print$approxbayes$draws)
    Output
      
      Draws Object
      ------------
      Number of Samples: 5
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: random
      Method:
          name: Approximate Bayes
          covariance: us
          threshold: 0.5
          same_cov: TRUE
          REML: TRUE
          n_samples: 5
      

---

    Code
      print(.test_print$approxbayes$impute)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 5
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  42%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(.test_print$approxbayes$analysis)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 5
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
           trt_visit_3    5.303   1.089  3.879   Inf   1   
         lsm_ref_visit_3  6.838   0.821  5.761   Inf   1   
         lsm_alt_visit_3  12.141  0.654  11.285  Inf   1   
        ---------------------------------------------------
      

# print - Condmean Bootstrap

    Code
      print(.test_print$condmean_boot$draws)
    Output
      
      Draws Object
      ------------
      Number of Samples: 1 + 5
      Number of Failed Samples: 0
      Model Formula: outcome ~ 1 + group + visit + age + sex + visit * group
      Imputation Type: condmean
      Method:
          name: Conditional Mean
          covariance: ar1
          threshold: 0.2
          same_cov: TRUE
          REML: TRUE
          n_samples: 5
          type: bootstrap
      

---

    Code
      print(.test_print$condmean_boot$impute)
    Output
      
      Imputation Object
      -----------------
      Number of Imputed Datasets: 1 + 5
      Fraction of Missing Data (Original Dataset):
          visit_1:   0%
          visit_2:   0%
          visit_3:  42%
      References:
          TRT     -> Placebo
          Placebo -> Placebo
      

---

    Code
      print(.test_print$condmean_boot$analysis)
    Output
      
      Analysis Object
      ---------------
      Number of Results: 1 + 5
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
      
        ====================================================
            parameter      est     se   lci    uci    pval  
        ----------------------------------------------------
           trt_visit_1    6.643   <NA>  -Inf  7.346   0.167 
         lsm_ref_visit_1  7.656   <NA>  -Inf  8.157   0.167 
         lsm_alt_visit_1  14.299  <NA>  -Inf  14.443  0.167 
           trt_visit_2    6.906   <NA>  -Inf  7.365   0.167 
         lsm_ref_visit_2  7.364   <NA>  -Inf  7.911   0.167 
         lsm_alt_visit_2  14.271  <NA>  -Inf  14.576  0.167 
           trt_visit_3    4.118   <NA>  -Inf  4.398   0.167 
         lsm_ref_visit_3  7.603   <NA>  -Inf  8.114   0.167 
         lsm_alt_visit_3  11.721  <NA>  -Inf  11.613  0.167 
        ----------------------------------------------------
      

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
           trt_visit_1    6.643   0.758  -Inf  7.889   <0.001 
         lsm_ref_visit_1  7.656   0.602  -Inf  8.646   <0.001 
         lsm_alt_visit_1  14.299  0.496  -Inf  15.115  <0.001 
           trt_visit_2    6.906   0.586  -Inf   7.87   <0.001 
         lsm_ref_visit_2  7.364   0.443  -Inf  8.094   <0.001 
         lsm_alt_visit_2  14.271  0.416  -Inf  14.955  <0.001 
           trt_visit_3    4.118   0.754  -Inf  5.359   <0.001 
         lsm_ref_visit_3  7.603   0.413  -Inf  8.283   <0.001 
         lsm_alt_visit_3  11.721  0.729  -Inf  12.919  <0.001 
        ------------------------------------------------------
      

# print - Condmean Jack

    Code
      print(.test_print$condmean_jack$draws)
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
          n_samples: NULL
          type: jackknife
      

---

    Code
      print(.test_print$condmean_jack$impute)
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
      print(.test_print$condmean_jack$analysis)
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
      

