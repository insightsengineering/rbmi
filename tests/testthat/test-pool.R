


# ########################
# #
# #  Bootstrap
# #
# #


# vals <- rnorm(500, 0, 1)
# samp <- lapply(
#     seq_len(length(vals)),
#     function(x){
#         vals2 <- sample(vals, size = length(vals), replace = TRUE)
#         list("p1" = list("est" = mean(vals2), "se" = sd(vals2), "df" = NA))
#     }
# )
# real <- list(list("p1" = list("est" = mean(vals), "se" = sd(vals), "df" = NA)))
# results <- append(real, samp) 

# results <- results %>% as_class("bootstrap")

# ### reference CIs 
#  c(-1, 1) * 1.96 * sqrt(1 / 500) + mean(vals)

# pool(results, type = "normal")
# pool(results, type = "percentile")

# pool(results, type = "normal", alternative = "less")
# pool(results, type = "percentile", alternative = "less")

# pool(results, type = "normal", alternative = "greater")
# pool(results, type = "percentile", alternative = "greater")


# results <- results %>% as_class("rubin")
# pool(results)


# ########################
# #
# #  Jackknife
# #
# #


# vals <- rnorm(500, 0, 1)
# samp <- lapply(
#     seq_len(length(vals)),
#     function(x){
#         vals2 <- vals[-x]
#         list("p1" = list("est" = mean(vals2), "se" = sd(vals2), "df" = NA))
#     }
# )
# real <- list(list("p1" = list("est" = mean(vals), "se" = sd(vals), "df" = NA)))
# results <- append(real, samp) %>% as_class("jackknife")

# ### reference CIs 
#  c(-1, 1) * 1.96 * sqrt(1 / 500) + mean(vals)

# pool(results)
# pool(results, alternative = "less")
# pool(results, alternative = "greater")













# x <- rnorm(1000000, 0)
# x <- c(mean(x), x)

# pool_bootstrap_normal(x, 0.95, "less")
# pool_bootstrap_percentile(x, 0.95, "less")

