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
         lsm_ref_visit_1  7.254   0.566  6.102   8.406   <0.001 
         lsm_alt_visit_1  14.507  0.479  13.533  15.481  <0.001 
           trt_visit_3    7.984   0.258  7.448    8.52   <0.001 
         lsm_ref_visit_3  7.005   0.205  6.575   7.436   <0.001 
         lsm_alt_visit_3  14.989  0.167  14.641  15.338  <0.001 
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
         lsm_ref_visit_1  7.254   0.566  6.513   Inf   1   
         lsm_alt_visit_1  14.507  0.479  13.881  Inf   1   
           trt_visit_2    7.406   0.388  6.898   Inf   1   
         lsm_ref_visit_2  7.011   0.282  6.643   Inf   1   
         lsm_alt_visit_2  14.417  0.238  14.106  Inf   1   
           trt_visit_3    5.359   1.092  3.929   Inf   1   
         lsm_ref_visit_3  6.723   0.835  5.624   Inf   1   
         lsm_alt_visit_3  12.082  0.658  11.222  Inf   1   
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
         lsm_ref_visit_1  7.605   <NA>  -Inf  8.126   <0.001 
         lsm_alt_visit_1  14.248  <NA>  -Inf  15.088  <0.001 
           trt_visit_2    6.906   <NA>  -Inf  7.944   <0.001 
         lsm_ref_visit_2  7.299   <NA>  -Inf  7.666   <0.001 
         lsm_alt_visit_2  14.205  <NA>  -Inf  14.977  <0.001 
           trt_visit_3    4.118   <NA>  -Inf  4.257   <0.001 
         lsm_ref_visit_3  7.514   <NA>  -Inf  8.083   <0.001 
         lsm_alt_visit_3  11.632  <NA>  -Inf  11.837  <0.001 
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
         lsm_ref_visit_1  7.605   1.057  -Inf  9.343   <0.001 
         lsm_alt_visit_1  14.248  1.163  -Inf  16.161  <0.001 
           trt_visit_2    6.906   0.852  -Inf  8.308   <0.001 
         lsm_ref_visit_2  7.299   1.114  -Inf   9.13   <0.001 
         lsm_alt_visit_2  14.205  0.984  -Inf  15.823  <0.001 
           trt_visit_3    4.118   0.663  -Inf  5.208   <0.001 
         lsm_ref_visit_3  7.514   1.003  -Inf  9.165   <0.001 
         lsm_alt_visit_3  11.632  1.339  -Inf  13.834  <0.001 
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
         lsm_ref_visit_1  7.051   0.766  5.792   8.311   <0.001 
         lsm_alt_visit_1  14.348  0.74   13.131  15.564  <0.001 
           trt_visit_2    7.363   0.373  6.749   7.977   <0.001 
         lsm_ref_visit_2  7.085   0.555  6.173   7.997   <0.001 
         lsm_alt_visit_2  14.448  0.599  13.463  15.433  <0.001 
           trt_visit_3    4.593   1.063  2.844   6.342   <0.001 
         lsm_ref_visit_3  6.469   0.815  5.129   7.809   <0.001 
         lsm_alt_visit_3  11.062  0.929  9.534   12.59   <0.001 
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
         lsm_ref_visit_1  6.993   1.38   4.212   9.773   0.004  
         lsm_alt_visit_1  14.032  1.178  11.658  16.406  <0.001 
           trt_visit_2    7.494   0.403  6.681   8.306   <0.001 
         lsm_ref_visit_2  6.694   1.278  4.119    9.27   0.003  
         lsm_alt_visit_2  14.188  1.013  12.146  16.23   <0.001 
           trt_visit_3    4.737   1.142   2.43   7.044   0.009  
         lsm_ref_visit_3   6.53   1.097  4.318   8.742   0.002  
         lsm_alt_visit_3  11.267  1.753  7.734    14.8   0.001  
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
          same_cov: TRUE
          n_samples: 50
      Controls:
          warmup: 200
          thin: 1
          chains: 1
          init: function (chain_id) 
          {
              list(b0 = chain_id, b1 = chain_id)
          }
          seed: 791990519
          control: list(adapt_delta = 0.95, max_treedepth = 15)
      

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
      

