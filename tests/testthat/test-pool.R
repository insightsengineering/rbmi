



results <- list(
    list(
        "p1" = list(
            "est" = 1.23,
            "se" = 1.13,
            "df" = 12
        ),
        "p2" = list(
            "est" = 1.23,
            "se" = 1.13,
            "df" = 12
        )
    ),
    list(
        "p1" = list(
            "est" = 1.23,
            "se" = 1.13,
            "df" = 12
        ),
        "p2" = list(
            "est" = 1.23,
            "se" = 1.13,
            "df" = 12
        )
    )
)


x <- rnorm(10000000, 1.96)
x <- c(mean(x), x)

pool_bootstrap_normal(x, 0.95, "two.sided")
pool_bootstrap_percentile(x, 0.95, "two.sided")





