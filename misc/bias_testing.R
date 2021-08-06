library(parallel)
devtools::load_all()
library(dplyr)


runme <- function(x){

    dat_p <- get_sim_data(n = 100, sigma)

    get_na_prob <- function(x){
        case_when(
            x == "visit_1" ~ 0,
            x == "visit_2" ~ 0.2,
            x == "visit_3" ~ 0.3,
            x == "visit_4" ~ 0.5,
        )
    }

    dat2 <- dat_p %>%
        mutate(mprob = get_na_prob(visit)) %>%
        mutate(naact = runif(n())) %>%
        filter(naact > mprob) %>%
        select(-mprob, -naact)

    dat3 <- expand_locf(
        data = dat2,
        id = unique(dat_p$id),
        visit = unique(dat_p$visit),
        vars = c("age", "group", "sex"),
        group = "id",
        order = "visit"
    )
    vrs <- list(
        subjid = "id",
        visit = "visit",
        outcome = "outcome",
        method = "method",
        covariates = c("age", "sex", "age*sex", "visit*group"),
        group = "group"
    )

    dat_ice <- dat3 %>%
        filter(is.na(outcome)) %>%
        arrange(id, visit) %>%
        group_by(id) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(method = "MAR") %>%
        mutate(as.character(visit))



    drawobj <- draws(
        data = dat3,
        data_ice = dat_ice,
        vars = vrs,
        method = method_condmean(n_samples = 1)
    )


    # drawobj <- as_class(drawobj, "random")

    imputeobj <- impute(
        drawobj,
        references = c("A" = "B", "B" = "B")
    )

    vrs2 <- vrs
    vrs2$covariates <- c("sex*age")
    anaobj <- analyse(
        imputations = imputeobj,
        fun = ancova,
        vars = vrs2,
        visit_level = "visit_4"
    )

    res <- pool(anaobj)

    res$pars$trt$est

}

sigma <- as_covmat(
    c(3, 4, 5, 6),
    c(0.3, 0.4, 0.45, 0.4, 0.3, 0.2)
)

cl <- makeCluster(6)
dn <- clusterEvalQ(cl, {library(dplyr); devtools::load_all()})
dn <- clusterExport(cl, c("sigma"))
clusterSetRNGStream(cl = cl, 101)
x <- parLapply(cl , 1:1000,runme)
stopCluster(cl)
x2 <- unlist(x)
mean(x2)
