library(parallel)
devtools::load_all()
library(dplyr)


get_sim_data <- function(n, sigma){

    set_col_names <- function(x, nam) {
        colnames(x) <- nam
        return(x)
    }
    f2n <- function(x) as.numeric(x) - 1
    nv <- ncol(sigma)
    covars <- tibble(
        id = 1:n,
        age = rnorm(n),
        group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F"))
    )

    dat <- mvtnorm::rmvnorm(n, sigma = sigma) %>%
        set_col_names(paste0("visit_", 1:nv)) %>%
        as_tibble() %>%
        mutate(id = 1:n()) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        mutate(visit = factor(visit)) %>%
        arrange(id, visit) %>%
        left_join(covars, by = "id") %>%
        mutate(outcome = outcome + 5 + 3 * age + 3 * f2n(sex) + 0 * f2n(group)) %>%
        mutate(id = as.character(id))

    return(dat)
}





dat <- get_sim_data(n = 50000, sigma)


get_na_prob <- function(x){
    case_when(
        x == "visit_1" ~ 0,
        x == "visit_2" ~ 0.2,
        x == "visit_3" ~ 0.3,
        x == "visit_4" ~ 0.5,
    )
}

dat2 <- dat %>%
    mutate(mprob = get_na_prob(visit)) %>%
    mutate(naact = runif(n())) %>%
    filter(naact > mprob) %>%
    select(-mprob, -naact)

dat3 <- expand_locf(
    data = dat2,
    id = unique(dat$id),
    visit = unique(dat$visit),
    vars = c("age", "group", "sex"),
    group = "id",
    order = "visit"
)


# dat3 %>% filter(is.na(outcome))
# dat3 %>% summarise(m = mean(is.na(outcome)) * 100)

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


ancova(
    dat3,
    vars = vrs2,
    visit_level = "visit_4"
)

ancova(
    dat,
    vars = vrs2,
    visit_level = "visit_4"
)




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
dn <- clusterExport(cl, c("get_sim_data", "sigma"))
clusterSetRNGStream(cl = cl, 101)
x <- parLapply(cl , 1:1000,runme)
stopCluster(cl)
x2 <- unlist(x)
mean(x2)


### 500 subjects with 5 visits  with  2 covariates (and interaction) using JR
#
#  20 samples = 18sec  (0.9 sec per samp)
#  50 samples = 45sec  (0.9 sec per samp)
# 100 samples = 88sec  (0.9 sec per samp)
#



### 1000 subjects with 5 visits  with  2 covariates (and interaction) using JR
#
#  20 samples = 37sec   (1.9 sec per samp)
#  50 samples = 85sec   (1.7 sec per samp)
# 100 samples = 155sec  (1.5 sec per samp)
#
# The rest (impute/analyse/pool) on 100 samples = 13.2sec

