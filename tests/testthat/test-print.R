






# test_that("print",{

#     sigma <- as_covmat(c(2, 1, 0.7), c(0.5, 0.3, 0.2))

#     set.seed(1518)

#     dat <- get_sim_data(500, sigma, trt = 8) %>%
#         mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
#         mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
#         select(-is_miss)


#     dat_ice <- dat %>%
#         group_by(id) %>%
#         arrange(id, visit) %>%
#         filter(is.na(outcome)) %>%
#         slice(1) %>%
#         ungroup() %>%
#         select(id, visit) %>%
#         mutate(method = "JR")


#     vars <- list(
#         outcome = "outcome",
#         group = "group",
#         method = "method",
#         subjid = "id",
#         visit = "visit",
#         covariates = c("age", "sex", "visit * group")
#     )

#     drawobj <- draws(
#         data = dat,
#         data_ice = dat_ice,
#         vars = vars,
#         method = method_approxbayes(
#             n_samples = 10,
#             threshold = 0.5,
#             same_cov = FALSE,
#             REML = TRUE,
#             covariance = "toep"
#         )
#     )

# dat2 <- dat %>%
#     mutate(GA = if_else(group == "A", 1, 0)) %>%
#     mutate(GB = if_else(group == "B", 1, 0))


# glmmTMB(
#     outcome ~ 1 + group + visit + age + sex + visit * group + toep(0 + GA:visit | id) + toep(0 + GB:visit | id),
#     data = dat2,
#     dispformula = ~0,
#     REML = TRUE,
#     control = glmmTMBControl(
#         optimizer = optim,
#         optArgs = list(method = "L-BFGS-B")
#     )
# )

#     expect_snapshot(print(drawobj))


#     # imputeobj <- impute(
#     #     draws = drawobj,
#     #     references = c("A" = "B", "B" = "B")
#     # )

#     # vars2 <- vars
#     # vars2$covariates <- c("age", "sex")

#     # anaobj <- analyse(
#     #     imputeobj,
#     #     fun = rbmi::ancova,
#     #     vars = vars2,
#     #     visits = "visit_3"
#     # )

#     # poolobj <- pool(
#     #     results = anaobj,
#     #     conf.level = 0.99,
#     #     alternative = "two.sided"
#     # )
    
    
    
    
    
# })
