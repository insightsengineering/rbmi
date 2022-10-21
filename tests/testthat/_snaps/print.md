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
      
        =========================================================
          name     visit    est     lci     uci     se    pvalue 
        ---------------------------------------------------------
         lsm_alt  visit_1  14.507  13.533  15.481  0.479  <0.001 
         lsm_ref  visit_1  7.254   6.102   8.406   0.566  <0.001 
           trt    visit_1  7.253   5.665   8.842   0.781  <0.001 
         lsm_alt  visit_3    15    14.671  15.328  0.158  <0.001 
         lsm_ref  visit_3  7.005   6.579   7.431   0.202  <0.001 
           trt    visit_3  7.995   7.451   8.538   0.261  <0.001 
        ---------------------------------------------------------
      

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
      
        ======================================================
          name     visit    est     lci    uci   se    pvalue 
        ------------------------------------------------------
         lsm_alt  visit_1  14.507  13.881  Inf  0.479    1    
         lsm_ref  visit_1  7.254   6.513   Inf  0.566    1    
           trt    visit_1  7.253   6.232   Inf  0.781    1    
         lsm_alt  visit_2  14.417  14.106  Inf  0.238    1    
         lsm_ref  visit_2  7.011   6.643   Inf  0.282    1    
           trt    visit_2  7.406   6.898   Inf  0.388    1    
         lsm_alt  visit_3  12.082  11.222  Inf  0.658    1    
         lsm_ref  visit_3  6.723   5.624   Inf  0.835    1    
           trt    visit_3  5.359   3.929   Inf  1.092    1    
        ------------------------------------------------------
      

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
      
        ======================================================
          name     visit    est    lci    uci     se   pvalue 
        ------------------------------------------------------
         lsm_alt  visit_1  14.248  -Inf  15.088  <NA>  <0.001 
         lsm_ref  visit_1  7.605   -Inf  8.126   <NA>  <0.001 
           trt    visit_1  6.643   -Inf  7.383   <NA>  <0.001 
         lsm_alt  visit_2  14.205  -Inf  14.977  <NA>  <0.001 
         lsm_ref  visit_2  7.299   -Inf  7.666   <NA>  <0.001 
           trt    visit_2  6.906   -Inf  7.944   <NA>  <0.001 
         lsm_alt  visit_3  11.632  -Inf  11.837  <NA>  <0.001 
         lsm_ref  visit_3  7.514   -Inf  8.083   <NA>  <0.001 
           trt    visit_3  4.118   -Inf  4.257   <NA>  <0.001 
        ------------------------------------------------------
      

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
      
        =======================================================
          name     visit    est    lci    uci     se    pvalue 
        -------------------------------------------------------
         lsm_alt  visit_1  14.248  -Inf  16.161  1.163  <0.001 
         lsm_ref  visit_1  7.605   -Inf  9.343   1.057  <0.001 
           trt    visit_1  6.643   -Inf  7.565   0.561  <0.001 
         lsm_alt  visit_2  14.205  -Inf  15.823  0.984  <0.001 
         lsm_ref  visit_2  7.299   -Inf   9.13   1.114  <0.001 
           trt    visit_2  6.906   -Inf  8.308   0.852  <0.001 
         lsm_alt  visit_3  11.632  -Inf  13.834  1.339  <0.001 
         lsm_ref  visit_3  7.514   -Inf  9.165   1.003  <0.001 
           trt    visit_3  4.118   -Inf  5.208   0.663  <0.001 
        -------------------------------------------------------
      

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
      
        =========================================================
          name     visit    est     lci     uci     se    pvalue 
        ---------------------------------------------------------
         lsm_alt  visit_1  14.348  13.131  15.564  0.74   <0.001 
         lsm_ref  visit_1  7.051   5.792   8.311   0.766  <0.001 
           trt    visit_1  7.296   6.006   8.587   0.784  <0.001 
         lsm_alt  visit_2  14.448  13.463  15.433  0.599  <0.001 
         lsm_ref  visit_2  7.085   6.173   7.997   0.555  <0.001 
           trt    visit_2  7.363   6.749   7.977   0.373  <0.001 
         lsm_alt  visit_3  11.062  9.534   12.59   0.929  <0.001 
         lsm_ref  visit_3  6.469   5.129   7.809   0.815  <0.001 
           trt    visit_3  4.593   2.844   6.342   1.063  <0.001 
        ---------------------------------------------------------
      

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
      
        =========================================================
          name     visit    est     lci     uci     se    pvalue 
        ---------------------------------------------------------
         lsm_alt  visit_1  14.032  11.658  16.406  1.178  <0.001 
         lsm_ref  visit_1  6.993   4.212   9.773   1.38   0.004  
           trt    visit_1  7.039   6.032   8.047    0.5   <0.001 
         lsm_alt  visit_2  14.188  12.146  16.23   1.013  <0.001 
         lsm_ref  visit_2  6.694   4.119    9.27   1.278  0.003  
           trt    visit_2  7.494   6.681   8.306   0.403  <0.001 
         lsm_alt  visit_3  11.267  7.734    14.8   1.753  0.001  
         lsm_ref  visit_3   6.53   4.318   8.742   1.097  0.002  
           trt    visit_3  4.737    2.43   7.044   1.142  0.009  
        ---------------------------------------------------------
      

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
      
        =====================================
          name     est     se    df   visit  
        -------------------------------------
           trt    7.253   0.781  35  visit_1 
         lsm_ref  7.254   0.566  35  visit_1 
         lsm_alt  14.507  0.479  35  visit_1 
           trt    7.406   0.388  35  visit_2 
         lsm_ref  7.011   0.282  35  visit_2 
         lsm_alt  14.417  0.238  35  visit_2 
           trt    5.037   1.128  35  visit_3 
         lsm_ref  6.942   0.818  35  visit_3 
         lsm_alt  11.978  0.692  35  visit_3 
        -------------------------------------
      

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
      
        =====================================
          name     est     se    df   visit  
        -------------------------------------
           trt    7.253   0.781  35  visit_1 
         lsm_ref  7.254   0.566  35  visit_1 
         lsm_alt  14.507  0.479  35  visit_1 
           trt    7.929   0.184  35  visit_3 
         lsm_ref  6.966   0.134  35  visit_3 
         lsm_alt  14.895  0.113  35  visit_3 
        -------------------------------------
      

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
      
        =====================================
          name     est     se    df   visit  
        -------------------------------------
           trt    6.643   1.26   37  visit_1 
         lsm_ref  7.605   0.955  37  visit_1 
         lsm_alt  14.248  0.821  37  visit_1 
           trt    6.906   0.941  37  visit_2 
         lsm_ref  7.299   0.713  37  visit_2 
         lsm_alt  14.205  0.613  37  visit_2 
           trt    7.181   0.917  37  visit_3 
         lsm_ref   7.51   0.696  37  visit_3 
         lsm_alt  14.691  0.598  37  visit_3 
        -------------------------------------
      

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
      
        =====================================
          name     est     se    df   visit  
        -------------------------------------
           trt    7.296   0.657  30  visit_1 
         lsm_ref  7.051   0.501  30  visit_1 
         lsm_alt  14.348  0.406  30  visit_1 
           trt    7.363   0.37   30  visit_2 
         lsm_ref  7.085   0.282  30  visit_2 
         lsm_alt  14.448  0.229  30  visit_2 
           trt    4.593   1.169  30  visit_3 
         lsm_ref  6.469   0.892  30  visit_3 
         lsm_alt  11.062  0.722  30  visit_3 
        -------------------------------------
      

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
      
        ========================
         name   est    se   df  
        ------------------------
         trt   2.005  0.73  Inf 
        ------------------------
      

