#
#
# ### mmrm function requirements
# fit_mmrm(
#     designmat = matrix(),
#     outcome = vector(),
#     subjid = vector(),
#     visit = vector(),
#     group = vector(),
#     cov_struc = "un",
#     REML = TRUE,
#     same_cov = True
# )
#
#
#
# ### Wrapper mmrm function to call inner function on different structures
# fit_mmrm_covs(
#     designmat = matrix(),
#     outcome = vector(),
#     subjid = vector(),
#     visit = vector(),
#     group = vector(),
#     REML = TRUE,
#     cov_struc = c("un", "ar1", "comp"),
#     same_cov = True
# )
#
#
#
# ## outcome ~ trt + covariates + trt*cov
# rbmi::mmrm(
#     data = data.frame(),
#     vars = list(
#         visit = "visit",                 # Factor J-levels
#         group = "arm",                   # Factor 2-levels
#         subjid = "subjid",               # Character or factor ?
#         outcome = "outcome",             # Numeric continuous
#         covariates = c("c1", "c2", "c1*c3", "base_out*visit")
#     ),
#     ...
# )
