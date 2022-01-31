library(dplyr)
library(tidyr)


set.seed(123)
mu <- c(1,2,3)
sigma <- rbind(c(3,0.5,0.3),c(0.5,4,0.5),c(0.3,0.5,5))
baseline_mean <- mu[1]
n <- 20
n_visits <- length(mu)
outcome <- c(replicate(n, sample_mvnorm(mu,sigma)))
ids <- as.factor(rep(paste0("id_", seq.int(n)), each = n_visits))
visits <- as.factor(rep(seq.int(n_visits)-1, n))

test_data_basics <- function(res, n_visits, n) {

    expect_true(is.data.frame(res))
    expect_true(all(sapply(res[,c("id", "group", "visit")], is.factor)))
    expect_true(all(sapply(res[, ! colnames(res) %in% c("id", "group", "visit")], is.numeric)))
    expect_equal(nrow(res), n_visits*n)
    expect_equal(nlevels(res$visit), n_visits)
    expect_length(unique(res$id), n)

    expect_true(
        all(
            sapply(
                res[, c("ind_ice1", "dropout_ice1", "dropout2", "ind_ice2")],
                function(x) all(x %in% c(0, 1))
            )
        )
    )

    expect_true(all(!is.na(res$outcome_noICE)))

    outcome_baseline <- unique(res$outcome_bl)
    expect_equal(res$outcome_noICE[seq(from = 1, to = n_visits*n, by = n_visits)], outcome_baseline)
    expect_equal(res$outcome[seq(from = 1, to = n_visits*n, by = n_visits)], outcome_baseline)
}


test_that("set_simul_pars", {

    pars <- set_simul_pars(
        mu = mu,
        sigma = sigma,
        n = n)

    expect_equal(list(pars$mu, pars$sigma, pars$n) , list(mu, sigma, n))

    expect_true(
        all(c(pars$prob_ice1, pars$prob_post_ice1_dropout, pars$prob_dropout, pars$prob_miss) == 0)
    )
    expect_true(
        pars$or_outcome_ice1 == 1
    )

    expect_true(validate(pars))

    expect_equal(class(pars), "simul_pars")

    pars$prob_ice1 <- rep(0.5, n_visits)
    expect_true(validate(pars))

    pars$prob_dropout <- NULL
    expect_error(validate(pars))

    pars$prob_dropout <- 2
    expect_error(validate(pars))

    pars$prob_dropout <- "0.5"
    expect_error(validate(pars))

    pars$prob_dropout <- 0.5
    pars$sigma[1,1] <- NA
    expect_error(validate(pars))

    expect_error(set_simul_pars(mu = mu))

})


test_that("simulate_dropout", {

    prob_dropout <- 0
    res <- simulate_dropout(prob_dropout, ids)
    expect_equal(res, rep(0, length(ids)))

    prob_dropout <- 1
    res <- simulate_dropout(prob_dropout, ids)
    expect_equal(res, rep(c(0,1,1), n))

    prob_dropout <- 0.5
    res <- simulate_dropout(prob_dropout, ids)
    expect_true(all(res %in% c(0,1)))
    expect_length(res, length(ids))

    prob_dropout <- 1
    subset <- rep(0, length(ids))
    res <- simulate_dropout(prob_dropout, ids, subset)
    expect_equal(res, rep(0, length(ids)))

    subset <- rep(c(0,0,1), n)
    res <- simulate_dropout(prob_dropout, ids, subset)
    expect_length(res, length(ids))
    expect_equal(res[subset == 0], subset[subset == 0])
    expect_true(all(res[subset == 1] == 1))

    subset <- rep(c(1,1,1), n)
    res <- simulate_dropout(prob_dropout, ids, subset)
    expect_equal(res, rep(c(0,1,1), n))

    prob_dropout <- 0
    subset <- rep(0, length(ids))
    res <- simulate_dropout(prob_dropout, ids, subset)
    expect_equal(res, rep(0, length(ids)))

    subset <- rep(c(0,0,1), n)
    res <- simulate_dropout(prob_dropout, ids, subset)
    expect_equal(res, rep(0, length(ids)))

    subset <- rep(c(1,1,1), n)
    res <- simulate_dropout(prob_dropout, ids, subset)
    expect_equal(res, rep(0, length(ids)))

})


test_that("simulate_ice", {

    pars <- set_simul_pars(mu, sigma, n)
    res <- simulate_ice(outcome, visits, ids, pars$prob_ice, pars$or_outcome_ice, baseline_mean)
    expect_equal(res, rep(0, length(ids)))

    pars$prob_ice1 <- 1
    res <- simulate_ice(outcome, visits, ids, pars$prob_ice, pars$or_outcome_ice, baseline_mean)
    expect_equal(res, rep(c(0,1,1), n))

    pars$prob_ice1 <- rep(1, n_visits)
    res <- simulate_ice(outcome, visits, ids, pars$prob_ice, pars$or_outcome_ice, baseline_mean)
    expect_equal(res, rep(c(0,1,1), n))

    pars$prob_ice1 <- 0.1
    pars$or_outcome_ice1 <- 3
    res <- simulate_ice(outcome, visits, ids, pars$prob_ice, pars$or_outcome_ice, baseline_mean)
    expect_length(res, length(ids))
    expect_true(all(res %in% c(0,1)))
    expect_true(all(res[seq(from = 1, to = length(ids), by = n_visits)] == 0))

})


test_that("adjust_trajectories_single", {

    outcome <- outcome[1:3]
    distr_pars_group <- list(mu = mu, sigma = sigma)
    strategy_fun <- getStrategies()$CIR

    # no missing values
    res <- adjust_trajectories_single(distr_pars_group, outcome, strategy_fun, distr_pars_ref = NULL)
    expect_equal(res, outcome)

    distr_pars_ref <- distr_pars_group
    distr_pars_ref$sigma[1, 1] <- 1
    distr_pars_ref$mu <- c(3, 6, 9)
    res <- adjust_trajectories_single(distr_pars_group, outcome, strategy_fun, distr_pars_ref)
    expect_equal(res, outcome)

    # missing values
    outcome[2:3] <- NA
    res <- adjust_trajectories_single(distr_pars_group, outcome, strategy_fun, distr_pars_ref)
    expect_true(all(!is.na(res)))
    expect_equal(res[1], outcome[1])

})


test_that("adjust_trajectories", {

    ind_ice <- unlist(
        tapply(
            rep(0, length(ids)),
            ids,
            function(x) {
                p <- runif(1)
                if (p <= 0.33) {
                    x[2:3] <- 1
                } else if (p <= 0.66) {
                    x[3] <- 1
                }
                return(x)
            }
        ),
        use.names = FALSE
    )

    distr_pars_group <- list(mu = mu, sigma = sigma)
    strategy_fun <- getStrategies()$JR
    res <- adjust_trajectories(
        distr_pars_group, 
        outcome,
        ids,
        ind_ice = rep(0, length(ids)),
        strategy_fun, distr_pars_ref = NULL
    )
    expect_equal(res, outcome)

    distr_pars_ref <- distr_pars_group
    distr_pars_ref$sigma[1,1] <- 1
    distr_pars_ref$mu <- c(3,6,9)
    res <- adjust_trajectories(
        distr_pars_group,
        outcome,
        ids,
        ind_ice,
        strategy_fun,
        distr_pars_ref
    )
    expect_length(res, length(ids))
    expect_true(all(!is.na(res)))
    expect_false(identical(res, outcome))

    outcome[2] <- NA
    expect_error(
        adjust_trajectories(
            distr_pars_group,
            outcome,
            ids,
            ind_ice,
            strategy_fun,
            distr_pars_ref
        )
    )
})


test_that("generate_data_single", {

    pars_group <- set_simul_pars(
        mu = mu,
        sigma = sigma,
        n = n
    )
    strategy_fun <- getStrategies()$CR

    # no ICEs
    res <- generate_data_single(pars_group, strategy_fun, distr_pars_ref = NULL)
    test_data_basics(res, n_visits, n)
    expect_true(all(!is.na(res)))
    expect_true(identical(res$outcome_noICE, res$outcome))
    expect_true(all(c(res$ind_ice1, res$dropout_ice1, res$dropout2, res$ind_ice2) == 0))

    # only ICE1
    pars_group$prob_ice1 <- 1
    res <- generate_data_single(pars_group, strategy_fun, distr_pars_ref = NULL)
    test_data_basics(res, n_visits, n)
    expect_equal(res$ind_ice1, rep(c(0,1,1), n))
    expect_false(identical(res$outcome_noICE, res$outcome))

    # add dropout
    pars_group$prob_post_ice1_dropout <- 1
    res <- generate_data_single(pars_group, strategy_fun, distr_pars_ref = NULL)
    test_data_basics(res, n_visits, n)
    expect_equal(res$dropout_ice1, rep(c(0,1,1), n))
    expect_true(all(is.na(res$outcome[-seq(from = 1, to = n_visits*n, by = n_visits)])))

    # only dropout2
    pars_group$prob_post_ice1_dropout <- 0
    pars_group$prob_dropout <- 1
    res <- generate_data_single(pars_group, strategy_fun, distr_pars_ref = NULL)
    test_data_basics(res, n_visits, n)
    expect_equal(res$dropout2, rep(c(0,1,1), n))
    expect_true(all(is.na(res$outcome[-seq(from = 1, to = n_visits*n, by = n_visits)])))

})


test_that("simulate_data", {

    pars_t <- set_simul_pars(
        mu = mu,
        sigma = sigma,
        n = n
    )
    pars_c <- pars_t
    pars_c$mu <- c(3,6,9)
    pars_c$sigma[3,3] <- 10
    post_ice_traj <- "JR"

    # no ICEs
    res <- simulate_data(pars_c, pars_t, post_ice_traj)
    test_data_basics(res, n_visits, 2*n)

    # ICE1
    pars_c$prob_ice1 <- pars_t$prob_ice1 <- c(0.4, 0.5, 0.6)
    pars_c$or_outcome_ice1 <- pars_t$or_outcome_ice1 <- 3
    pars_c$prob_post_ice1_dropout <- pars_t$prob_post_ice1_dropout <- 0.5

    post_ice_traj <- "CIR"
    set.seed(123)
    res <- simulate_data(pars_c, pars_t, post_ice_traj)
    test_data_basics(res, n_visits, 2*n)

    # custom function
    myfun <- function(pars_group, pars_ref, index_mar) return(strategy_CIR(pars_group, pars_ref, index_mar))
    post_ice_traj <- "myfun"
    set.seed(123)
    res_cir <- simulate_data(pars_c, pars_t, post_ice_traj, strategies = getStrategies(myfun = myfun))
    test_data_basics(res, n_visits, 2*n)
    expect_equal(res, res_cir) # custom function is just CIR

    pars_c <- pars_t <- set_simul_pars(
        mu = mu,
        sigma = sigma,
        n = n,
        prob_miss = 1
    )
    post_ice_traj <- "JR"
    res <- simulate_data(pars_c, pars_t, post_ice_traj)
    expect_true(all(is.na(res$outcome[-seq(from = 1, to = n_visits*2*n, by = n_visits)])))

    pars_c <- pars_t <- set_simul_pars(
        mu = mu,
        sigma = sigma,
        n = n,
        prob_dropout = 1
    )
    res <- simulate_data(pars_c, pars_t, post_ice_traj)
    expect_true(all(is.na(res$outcome[-seq(from = 1, to = n_visits*2*n, by = n_visits)])))

    class(pars_c) <- NULL
    expect_error(simulate_data(pars_c, pars_t, post_ice_traj))

})





test_that("generic tests to ensure simulate_data is working as expected", {

    expect_around <- function(x, e, margin = 0.05) {
        expect_true(
            all(c(
                x <= e * (1 + margin),
                x >= e * (1 - margin)
            ))
        )
    }

    set.seed(1241)

    simpar_c_sigma <- list(
        sd = c(5, 4, 2),
        cor = c(0.1, 0.2, 0.3)
    )

    simpar_c <- set_simul_pars(
        mu = c(10, 10, 10),
        sigma = as_vcov(simpar_c_sigma$sd, simpar_c_sigma$cor),
        n = 9000,
        prob_ice1 = c(0.25),
        or_outcome_ice1 = 1,
        prob_post_ice1_dropout = 0,
        prob_dropout = 0,
        prob_miss = 0.2
    )

    simpar_t_sigma <- list(
        sd = c(1, 2, 3),
        cor = c(0.3, 0.4, 0.5)
    )

    simpar_t <- set_simul_pars(
        mu = c(10, 30, 60),
        sigma = as_vcov(simpar_t_sigma$sd, simpar_t_sigma$cor),
        n = 7000,
        prob_ice1 = c(0.5, 0.75, 0),
        or_outcome_ice1 = 1,
        prob_post_ice1_dropout = 0,
        prob_dropout = 0,
        prob_miss = 0
    )

    dat <- simulate_data(
        pars_c = simpar_c,
        pars_t = simpar_t,
        post_ice_traj = "JR"
    ) %>%
        as_tibble()


    count <- dat %>%
        group_by(group) %>%
        tally() %>%
        pull(n)

    expect_equal(
        count,
        c(simpar_c$n, simpar_t$n) * 3
    )


    control_nrow <- dat %>%
        filter(group == "Control") %>%
        filter(!is.na(outcome)) %>%
        filter(outcome_noICE != outcome) %>%
        nrow()

    expect_equal(
        control_nrow,
        0
    )



    stat_c <- dat %>%
        filter(group == "Control") %>%
        group_by(visit) %>%
        summarise(
            m = mean(outcome, na.rm = TRUE),
            v = var(outcome, na.rm = TRUE),
            nd = mean(ind_ice1),
            nm = mean(is.na(outcome))
        )


    stat_t <- dat %>%
        filter(group == "Intervention") %>%
        group_by(visit) %>%
        summarise(
            m = mean(outcome),
            v = var(outcome_noICE),
            nd = mean(ind_ice1)
        )


    pt <- c(0, simpar_t$prob_ice1[-length(simpar_t$prob_ice1)])
    cum_pt <- 1 - cumprod(1 - pt)
    e_mean <- (simpar_t$mu * (1 - cum_pt)) + (simpar_c$mu * cum_pt)


    expect_around(stat_t$m, e_mean)
    expect_around(stat_t$v, diag(simpar_t$sigma))

    expect_around(stat_c$m, simpar_c$mu)
    expect_around(stat_c$v, diag(simpar_c$sigma))

    expect_around(
        stat_c$nm,
        c(0, rep(simpar_c$prob_miss, length(simpar_c$mu) - 1))
    )

    pc <- c(0, rep(simpar_c$prob_ice1, length(simpar_c$mu) - 1))
    cum_pc <- 1 - cumprod(1 - pc)
    expect_around(stat_c$nd, cum_pc)

})
