

#' Title
#'
#' @description
#' TODO
#'
#' @param imputations TODO
#'
#' @export
delta_template <- function(imputations) {
    ld <- imputations$longdata
    x <- lapply(
        ld$ids,
        function(id) {
            strat <- ifelse(ld$is_mar[[id]], "MAR", ld$strategies[[id]])
            strat[!ld$is_missing[[id]]] <- NA_character_

            df <- data.frame(
                "is_mar" = ld$is_mar[[id]],
                "is_missing" = ld$is_missing[[id]],
                "is_post_ice" = ld$is_post_ice[[id]],
                "strategy" = strat,
                "delta" = 0,
                stringsAsFactors = FALSE
            )
            df[[ld$vars$subjid]] <- id
            df[[ld$vars$visit]] <- factor(ld$visits, labels = ld$visits)
            df[[ld$vars$group]] <- ld$impgroup[[id]]

            vars <- c(
                ld$vars$subjid,
                ld$vars$visit,
                ld$vars$group,
                "is_mar",
                "is_missing",
                "is_post_ice",
                "strategy",
                "delta"
            )

            return(df[, vars])
        }
    )
    Reduce(rbind, x)
}

#' Title
#'
#' @description
#' TODO
#'
#' @param delta TODO
#' @param dlag TODO
#' @param is_post_ice TODO
d_lagscale <- function(delta, dlag, is_post_ice) {

    assert_that(
        is.numeric(dlag),
        is.numeric(delta),
        is.logical(is_post_ice),
        msg = "`dlag` and `delta` must be numeric, `is_post_ice` must be logical"
    )

    assert_that(
        length(delta) == length(dlag),
        length(delta) == length(is_post_ice),
        msg = "`delta`, `dlag` and `is_post_ice` must all be the same length"
    )

    result <- rep(0, length(dlag))
    if (all(!is_post_ice)) {
        return(result)
    }
    is_post_ice_scale <- delta[is_post_ice]
    is_post_ice_lag <- dlag[seq_len(length(is_post_ice_scale))]
    vals <- cumsum(is_post_ice_lag * is_post_ice_scale)
    result[is_post_ice] <- vals
    return(result)
}


#' Title
#'
#' @description
#' TODO
#'
#' @param imputations TODO
#' @param delta TODO
#' @param dlag TODO
#' @param missing_only If false delta adjustments will be calculated for non-missing post-ice data. If true then
#' non-missing post-ice data will have a delta of 0 assigned. See details for more information (TODO).
#'
#' @export
delta_lagscale <- function(
    imputations,
    delta,
    dlag =  c(1, rep(0, length(delta) - 1)),
    missing_only = TRUE
){
    dat <- delta_template(imputations)
    ld <- imputations$longdata

    assert_that(
        is.numeric(delta),
        is.numeric(dlag),
        length(delta) == length(dlag),
        length(delta) == length(ld$visits),
        msg = sprintf(
            "`delta` and `dlag` must both be a length %s numeric vector",
            length(ld$visits)
        )
    )

    assert_that(
        is.logical(missing_only) ,
        length(missing_only) == 1,
        msg = "`missing_only` must be TRUE or FALSE"
    )

    delta_list <- lapply(
        ld$ids,
        function(id) {
            d_lagscale(
                delta = delta,
                dlag = dlag,
                is_post_ice = ld$is_post_ice[[id]]
            )
        }
    )
    delta <- unlist(delta_list)
    assert_that(length(delta) == length(dat[["delta"]]))
    dat["delta"] <- delta

    if( missing_only){
        ## Correction to remove any delta assigned to none missing values
        dat$delta[!dat[["is_missing"]] & dat[["is_post_ice"]]] <- 0
    }

    return(dat)
}




#' Title
#'
#' @description
#' TODO
#'
#' @param data TODO
#' @param delta TODO
#' @param group TODO
#' @param outcome TODO
#'
#' @export
apply_delta <- function(data, delta = NULL, group = NULL, outcome = NULL){

    assert_that(
        is.character(group),
        length(group) >= 1,
        is.character(outcome),
        length(outcome) == 1,
        msg = "`group` and `outcome` must be character vectors"
    )

    assert_that(
        is.data.frame(data),
        is.data.frame(delta) | is.null(delta),
        msg = "`dat` and `delta` must be data.frames"
    )

    assert_that(
        !"delta" %in% names(data),
        msg = " `delta` is a reserved variable name should not be already defined in `data`"
    )

    if (is.null(delta)) {
        return(data)
    }
    if( nrow(delta) == 0 ){
        return(data)
    }

    for (var in c(group, outcome)) {
        assert_that(
            var %in% names(data),
            msg = sprintf("Variable `%s` is not in `data`", var)
        )
    }

    for (var in c(group, "delta")) {
        assert_that(
            var %in% names(delta),
            msg = sprintf("Variable `%s` is not in `data`", var)
        )
    }

    delta_min <- delta[, c(group, "delta")]

    data2 <- merge(data, delta_min, all.x = TRUE, by = group)

    data2[is.na(data2[["delta"]]), "delta"] <- 0
    data2[[outcome]] <- data2[[outcome]] + data2[["delta"]]

    data3 <- data2[, names(data)]

    assert_that(
        ncol(data3) == ncol(data),
        nrow(data3) == nrow(data),
        all(names(data3) == names(data)),
        msg = paste0(
            "Data structure has been altered whilst adding delta.",
            "This can happen if you have multiple rows per `group`"
        )
    )
    class(data3) <- class(data)
    return(data3)
}
