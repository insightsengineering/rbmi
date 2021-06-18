library(dplyr)
library(testthat)

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
    mutate(subjid = as.character(subjid))

dat[c(1,2,3,4,6,7), "outcome"] <- NA


vars <- list(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    covariates = c("sex", "age")
)

ld <- longDataConstructor$new(
    data = dat,
    vars = vars
)



test_that("longData - Basics",{

    subject_names <- unique(dat$subjid)
    expect_equal( names(ld$is_mar), subject_names)
    expect_equal( names(ld$is_missing), subject_names)
    expect_equal( names(ld$subjects), subject_names)
    expect_equal( ld$ids, subject_names)

    expect_equal( ld$visits,  levels(dat$visit))
    expect_length( ld$strata, length(unique(dat$subjid)))

    expect_equal(
        unlist(ld$is_missing, use.names = FALSE),
        c(T,T,T,   T,F,T,   T,F,F,   F,F,F)
    )

    expect_equal(
        unlist(ld$is_mar, use.names = FALSE) ,
        rep(TRUE, n * nv)
    )

})

test_that("longData - Sampling",{

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


    x <- ld$get_data( c("1","1","3"))
    y <- bind_rows(
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "3")
    )
    expect_equal( select(x, -subjid) , select(y,-subjid))
    expect_true(all(x$subjid != y$subjid))


    imputes <- list(
        list(id = "1", values = c(1,2,3)),
        list(id = "1", values = c(4,5,6)),
        list(id = "2", values = c(7,8)),
        list(id = "4", values = c())
    )
    x <- ld$get_data(imputes)
    pt2_val <- dat %>% filter(subjid == "2") %>% pull(outcome)
    pt2_val[is.na(pt2_val)] <- c(7,8)
    y <- bind_rows(
        dat %>% filter(subjid == "1") %>% mutate(outcome = c(1,2,3)),
        dat %>% filter(subjid == "1") %>% mutate(outcome = c(4,5,6)),
        dat %>% filter(subjid == "2") %>% mutate(outcome = pt2_val),
        dat %>% filter(subjid == "4"),
    )
    expect_equal( select(x, -subjid) , select(y,-subjid))
    expect_true(all(x$subjid != y$subjid))
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
