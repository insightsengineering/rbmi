
suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})


ld_2_list <- function(ld) {
    list(
        visits = ld$visits,
        is_mar = ld$is_mar,
        data = ld$data,
        ids = ld$ids,
        group = ld$group,
        indexes = ld$indexes,
        vars = ld$vars,
        strata = ld$strata,
        strategies = ld$strategies,
        strat_lock = ld$strategy_lock,
        values = ld$values,
        visit_ice = ld$visit_ice,
        is_missing = ld$is_missing,
        is_post_ice = ld$is_post_ice
    )
}


get_ld <- function(){
    n <- 4
    nv <- 3

    covars <- tibble(
        subjid = 1:n,
        age = rnorm(n),
        group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
        strata = c("A","A", "A", "B")
    )

    dat <- tibble(
        subjid = rep.int(1:n, nv)
    ) %>%
        left_join(covars, by = "subjid") %>%
        mutate( outcome = rnorm(
            n(),
            age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
            sd = 3
        )) %>%
        arrange(subjid) %>%
        group_by(subjid) %>%
        mutate( visit = factor(paste0("Visit ", 1:n())))  %>%
        ungroup() %>%
        mutate(subjid = factor(subjid))

    dat[c(1,2,3,4,6,7), "outcome"] <- NA


    vars <- list(
        outcome = "outcome",
        visit = "visit",
        subjid = "subjid",
        group = "group",
        strata = "strata",
        covariates = c("sex", "age"),
        method = "method"
    )

    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )
    
    return(list(ld = ld, dat = dat, n = n, nv =nv))
}



test_that("longData - Basics", {

    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    subject_names <- as.character(unique(dat$subjid))
    expect_equal( names(ld$is_mar), subject_names)
    expect_equal( names(ld$is_missing), subject_names)
    expect_equal( ld$ids, subject_names)

    expect_equal( ld$visits,  levels(dat$visit))
    expect_length( ld$strata, length(unique(dat$subjid)))

    expect_equal(
        unlist(ld$is_missing, use.names = FALSE),
        c(T,T,T,   T,F,T,   T,F,F,   F,F,F)
    )

    expect_equal(
        unlist(ld$is_mar, use.names = FALSE),
        rep(TRUE, dobj$n * dobj$nv)
    )

})



test_that("longData - Sampling",{

    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    set.seed(101)
    samps <- replicate(
        n = 1000,
        ld$sample_ids()
    )

    expect_true( "1" %in% samps[1,])
    expect_true( "2" %in% samps[1,])
    expect_true( "3" %in% samps[1,])

    ## Subject "4" is the only subject in their strata so they must be sampled
    expect_true( all(samps[4,] == "4"))

    ### Looking to see that re-sampling is working i.e. samples contain duplicates
    expect_true(any( apply(samps, 2, function(x) length(unique(x))) %in% c(1,2)))

    expect_error(ld$get_data("-1231"))

    x <- ld$get_data(c("1", "1", "3"))

    y <- bind_rows(
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "3")
    )
    expect_equal( select(x, -subjid) , select(y,-subjid))
    expect_true(all(x$subjid != y$subjid))



    imputes <- as_imputation_list(list(
        as_imputation_single(id = "1", values = c(1, 2, 3)),
        as_imputation_single(id = "4", values = c()),
        as_imputation_single(id = "1", values = c(4, 5, 6)),
        as_imputation_single(id = "2", values = c(7, 8))
    ))
    x <- ld$get_data(imputes)
    pt2_val <- dat %>%
        filter(subjid == "2") %>%
        pull(outcome)

    pt2_val[is.na(pt2_val)] <- c(7, 8)

    y <- bind_rows(
        dat %>% filter(subjid == "1") %>% mutate(outcome = c(1, 2, 3)),
        dat %>% filter(subjid == "4"),
        dat %>% filter(subjid == "1") %>% mutate(outcome = c(4, 5, 6)),
        dat %>% filter(subjid == "2") %>% mutate(outcome = pt2_val)
    )

    expect_equal(select(x, -subjid), select(y, -subjid))
    expect_true(all(x$subjid != y$subjid))



    x <- ld$get_data(c("1","1","1","2"), na.rm = TRUE)

    pt2_val <- dat %>% filter(subjid == "2") %>% pull(outcome)
    y <- bind_rows(
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "2"),
    ) %>%
        filter(!is.na(outcome))
    expect_equal( select(x, -subjid) , select(y,-subjid))
    expect_true(all(x$subjid != y$subjid))
})





test_that("Strategies",{

    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        rep("MAR", dobj$n)
    )

    dat_ice <- tribble(
        ~visit, ~subjid, ~method,
        "Visit 1" , "1",  "ABC",
        "Visit 2",  "2",  "MAR",
        "Visit 3",  "3",  "XYZ"
    )

    ld$set_strategies(dat_ice)

    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        c("ABC", "MAR", "XYZ", "MAR")
    )

    expect_equal(
        unlist(ld$strategy_lock, use.names = FALSE),
        c(FALSE, TRUE, TRUE, FALSE)
    )


    expect_equal(
        unlist(ld$is_mar, use.names = FALSE),
        c(F,F,F,  T,T,T,  T,T,F,  T,T,T)
    )

    pre_update_ice_visit <- ld$visit_ice

    dat_ice <- tribble(
        ~subjid, ~method,
          "1",  "ABC",
          "2",  "MAR",
          "3",  "ABC"
    )
    ld$update_strategies(dat_ice)

    post_update_ice_visit <- ld$visit_ice

    expect_equal(pre_update_ice_visit, post_update_ice_visit)

    expect_equal(
        unlist(ld$is_mar, use.names = FALSE),
        c(F,F,F,  T,T,T,  T,T,F,  T,T,T)
    )

    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        c("ABC", "MAR", "ABC", "MAR")
    )

    dat_ice <- tribble(
        ~visit, ~subjid, ~method,
        "Visit 1" , "2",  "ABC",
    )
    expect_error(ld$update_strategies(dat_ice), "Unable to change from MAR to non-MAR")

    dat_ice <- tribble(
         ~subjid, ~method,
          "3",  "MAR",
    )
    expect_error(ld$update_strategies(dat_ice), "Unable to change from non-MAR to MAR")
})


test_that("strategies part 2",{

    # Here we check to see that using `update_strategies` only updates the method and not
    # the visits (or anything else for that matter)

    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat


    dat_ice <- tribble(
        ~visit, ~subjid, ~method,
        "Visit 1" , "1",  "ABC",
        "Visit 2",  "2",  "MAR",
        "Visit 3",  "3",  "XYZ"
    )

    ld$set_strategies(dat_ice)
    pre_update_ld <- ld_2_list(ld)


    dat_ice <- tribble(
        ~subjid, ~method, ~visit,
        "1", "ABC", "Visit 2",
        "2", "MAR", "Visit 7",
        "3", "XYZ", "Visit 1"
    )
    ld$update_strategies(dat_ice)
    expect_equal(ld_2_list(ld), pre_update_ld)


    dat_ice <- tribble(
        ~subjid, ~method, ~visit,
        "1", "LKJ", "Visit 2",
        "2", "MAR", "Visit 7",
        "3", "XYZ", "Visit 1"
    )
    ld$update_strategies(dat_ice)
    expect_equal(ld$is_mar, pre_update_ld$is_mar)
    expect_equal(ld$visit_ice, pre_update_ld$visit_ice)
    expect_equal(unlist(ld$strategies, use.names = FALSE), c("LKJ", "MAR", "XYZ", "MAR"))
})





test_that("sample_ids", {
    set.seed(101)
    x <- sample_ids(c(1,2,3))
    set.seed(101)
    y <- sample_ids(c(1,2,3))
    set.seed(7)
    z <- sample_ids(c(1,2,3))

    expect_equal(x,y)
    expect_true(all(x %in% c(1,2,3)))
    expect_true(all(z %in% c(1,2,3)))
    expect_length(x, 3)
    expect_length(y, 3)
    expect_length(z, 3)

    set.seed(200)
    samps <- replicate(
        n = 10000,
        sample_ids(c(1,2,3))
    )

    ### Looking to see that re-sampling is working i.e. samples contain duplicates
    expect_true(any( apply(samps, 2, function(x) length(unique(x))) %in% c(1,2)))

    ### Assuming random sampling the mean should converge to ~2
    samps_mean <- apply(samps, 1, mean)
    expect_true(all( samps_mean >= 1.95 & samps_mean <= 2.05))
})


test_that("as_strata", {
    expect_equal(as_strata( c(1,2,3), c(1,2,3)), c(1,2,3))
    expect_equal(as_strata( c(1,1,2), c(5,5,6)), c(1,1,2))
    expect_equal(as_strata( c(1,1,1), c("a","a","a")), c(1,1,1))
    expect_equal(as_strata( c("a","b","c"), c("a","a","a")), c(1,2,3))
    expect_equal(as_strata( c("a","a","c"), c("a","a","a")), c(1,1,2))
})



test_that("idmap", {
    # The idmap option provides a mapping vectoring linking new_ids to old_ids
    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    x <- ld$get_data(c("1", "1", "3"), idmap = TRUE)
    expect_equal(
        attr(x, "idmap"),
        c("new_pt_1" = "1", "new_pt_2" = "1", "new_pt_3" = "3")
    )

    x <- ld$get_data(c("1", "1", "3"), idmap = TRUE, na.rm = TRUE)
    expect_equal(
        attr(x, "idmap"),
        c("new_pt_1" = "1", "new_pt_2" = "1", "new_pt_3" = "3")
    )

    imps <- as_imputation_list(list(
        as_imputation_single(id = "1", values = c(1, 2, 3)),
        as_imputation_single(id = "3", values = c(4)),
        as_imputation_single(id = "3", values = 5)
    ))
    x <- ld$get_data(imps, idmap = TRUE)
    expect_equal(
        attr(x, "idmap"),
        c("new_pt_1" = "1", "new_pt_2" = "3", "new_pt_3" = "3")
    )
})



test_that("longdata can handle data that isn't sorted",{
    dat <- tibble(
        visit = factor(c("v1", "v2", "v3", "v3", "v1", "v2"), levels = c("v1", "v2", "v3")),
        id = factor(c("1", "1", "1", "2", "2", "2")),
        group = factor(c("A", "A", "A", "B", "B", "B")),
        outcome = c(1, 2, 3, 4, 5, NA)
    )
    vars <- list(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        method = "method"
    )

    dat_ice <- tibble(
        visit = "v2",
        id = "2",
        method = "JR"
    )

    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )
    ld$set_strategies(dat_ice)

    expect_equal(ld$values, list("1" = c(1,2,3), "2" = c(5,NA,4)))
    expect_equal(ld$is_missing, list("1" = c(F, F, F), "2" = c(F, T, F)))
    expect_equal(ld$is_mar, list("1" = c(T, T, T), "2" = c(T, F, F)))
})


