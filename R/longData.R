
#' R6 Class for Storing / Accessing & Sampling Longitudinal Data
#'
#' @description
#'
#' A longdata object allows for efficient storage and recall of longitudinal datasets for use in
#' bootstrap sampling. The object works by de-constructing data into lists based upon subject id
#' enabling efficient lookup.
#'
#' @details
#'
#' The object also handles multiple other operations specific to rbmi such as defining whether an
#' outcome value is MAR / Missing or not as well as tracking which imputation strategy is assigned
#' to each subject
#'
#' It is recognised that this objects functionality is fairly overloaded and is hoped that this can
#' be split out into more area specific objects / functions in the future. Further additions of functionality
#' to this object should be avoided.
#'
#' @import R6
#' @export
longDataConstructor <- R6::R6Class(
    classname = "longData",

    public = list(

        #' @field data The original dataset passed to the constructor
        data = NULL,

        #' @field vars The vars object (list of key variables) passed to the constructor
        vars = NULL,

        #' @field visits A character vector containing the distinct visit levels
        visits = NULL,


        #' @field ids A character vector containing the unique ids of each subject in `self$data`
        ids = NULL,

        #' @field ids_levels A character vector containing the exact levels (and order) of the
        #' original `data[[vars$subjid]]` variable
        ids_levels = NULL,

        #' @field strata A numeric vector indicating which strata each corresponding value of `self$ids` belongs to.
        #' If no stratification variable is defined this will default to 1 for all subjects (i.e. same group).
        #' This field is only used as part of the `self$sample_ids()` function to enable stratified bootstrap
        #' sampling
        strata = NULL,


        #' @field visit_ice A list indexed by subject storing the visit which the patient had their ICE on
        visit_ice = list(),


        #' @field values A list indexed by subject storing the original outcome values
        values = list(),

        #' @field group A list indexed by subject storing the a single character indicating which imputation
        #' group the subject belongs to. This is typically the subjects treatment group but can vary. It is used
        #' to determine what reference group should be used when imputing the subjects data.
        group = list(),

        #' @field is_mar A list indexed by subject storing logical values indicating if the subjects outcome values
        #' are MAR or not. This list is defaulted to TRUE for all subjects & outcomes and is then
        #' modified by calls to `self$set_strategies()`.
        is_mar = list(),

        #' @field strategies A list indexed by subject storing a single character value indicating the imputation
        #' strategy assigned to a specific subject. This list is defaulted to "MAR" for all subjects and is then
        #' modified by calls to either `self$set_strategies()` or `self$update_strategies()`.
        strategies = list(),

        #' @field strategy_lock A list indexed by subject storing a single logical value indicating whether a
        #' patients imputation strategy is locked or not. If a strategy is locked it means that it can't change
        #' from MAR to non-MAR or non-MAR to MAR. Strategies are locked if the patient has non-missing after
        #' their ICE. This list is populated by a call to `self$set_strategies()`.
        strategy_lock = list(),

        #' @field indexes A list indexed by subject storing a numeric vector of indexes which specify which rows in the
        #' original dataset below to this subject i.e. to recover the full data for subject "pt3" you can use
        #' `self$data[self$indexes[["pt3"]],]`. This may seem redundant over filtering the data directly
        #' however it enables efficient bootstrap sampling of the data i.e.
        #' ```
        #' indexes <- unlist(self$indexes[c("pt3", "pt3")])
        #' self$data[indexes,]
        #' ```
        #' This list is populated during the object initialisation.
        indexes = list(),

        #' @field is_missing A list indexed by subject storing a logical vector indicating whether the corresponding
        #' outcome of a subject is missing. This list is populated during the object initialisation.
        is_missing = list(),

        #' @field is_post_ice A list indexed by subject storing a logical vector indicating whether the corresponding
        #' outcome of a subject is post the date of their ICE. If no ICE data has been provided this defaults to False
        #' for all observations. This list is populated by a call to `self$set_strategies()`.
        is_post_ice = list(),


        #' @description
        #'
        #' Returns a dataframe based upon required subject IDs. Replaces missing values if new values are provided.
        #'
        #' @param obj Either NULL, a character vector of subjects IDs or a list of lists
        #' with elements "id" and "values". See details.
        #'
        #' @param nmar.rm logical value. If TRUE will remove observations that are not regarded as MAR (as
        #' determined from `self$is_mar`)
        #'
        #' @param na.rm logical value. If TRUE will remove outcome values that are missing (as
        #' determined from `self$is_missing`)
        #'
        #' @param idmap logical value. If TRUE will add an attribute `idmap` which contains a mapping from the
        #' new subject ids to the old subject ids
        #'
        #' @details
        #'
        #' If `obj` is NULL then the full original dataset is returned. If `obj` is a
        #' character vector then a new dataset consisting of just those subjects is returned; if the
        #' character vector contains duplicate entries then that subject will be returned multiple times.
        #' If `obj` is a list of lists with elements `id` and `values` then a dataset of those subjects
        #' will be returned but with missing values filled in by the values in `values`.
        #' i.e.
        #' ```
        #' obj <- list(
        #'   list( id = "pt1", values = c(1,2,3)),
        #'   list( id = "pt1", values = c(4,5,6)),
        #'   list( id = "pt3", values = c(7,8))
        #' )
        #' ld$get_data(obj)
        #' ```
        #' Will return a dataframe consisting of all observations for pt1 twice and all of the
        #' observations for "pt3" once. The first set of observations for "pt1" will have missing
        #' values filled in with `c(1,2,3)` and the second set will be filled in by `c(4,5,6)`. The
        #' length of the values must be equal to `sum(self$is_missing[[id]])`.
        #'
        #' If `obj` is not NULL then all subject IDs will be scrambled in order to ensure that they are unique
        #' i.e. If the "pt2" is requested twice then this process guarantees that each set of observations
        #' be have a unique subject ID number.
        #'
        #' @return
        #'
        #' A dataframe
        get_data = function(obj = NULL, nmar.rm = FALSE, na.rm = FALSE, idmap = FALSE) {

            if (is.null(obj)) return(self$data)

            if (! any(c("imputation_list", "character") %in% class(obj))) {
                stop("Object must be an imputation_list or a character vector")
            }

            list_flag <- "imputation_list" %in% class(obj)

            if (list_flag) {
                obj_expanded <- transpose_imputations(obj)
                ids <- obj_expanded$ids
                values <- obj_expanded$values

                n_miss <- vapply(self$is_missing[ids], function(x) sum(x), numeric(1))
                n_values <- vapply(obj, function(x) length(x$values), numeric(1))
                assert_that(
                    all(n_miss == n_values),
                    msg = "Number of missing values doesn't equal number of imputed values"
                )
            } else {
                ids <- obj
            }

            self$validate_ids(ids)
            indexes <- self$indexes[ids]

            if (list_flag | nmar.rm | na.rm) {
                is_miss <- unlist(self$is_missing[ids], use.names = FALSE)
                is_mar <- unlist(self$is_mar[ids], use.names = FALSE)
            }

            new_ids_full <- mapply(
                function(x, y) rep(paste0("new_pt_", x), times = length(y)),
                seq_along(indexes),
                indexes,
                SIMPLIFY = FALSE
            )

            new_ids_full <- unlist(new_ids_full, use.names = FALSE)
            indexes_vec <- unlist(indexes, use.names = FALSE)

            new_data <- self$data[indexes_vec, ]

            new_data[[self$vars$subjid]] <- new_ids_full

            if (list_flag) {
                new_data[is_miss, self$vars$outcome] <- values
            }

            if (nmar.rm | na.rm) {
                keep <- (is_mar | (!nmar.rm)) & (!is_miss | (!na.rm))
                new_data <- new_data[keep, ]
            }

            if (idmap) {
                new_ids_single <- vapply(seq_along(indexes), function(x) paste0("new_pt_", x), character(1))
                id_map <- ids
                names(id_map) <- new_ids_single
                attr(new_data, "idmap") <- id_map
            }

            return(new_data)
        },


        #' @description
        #' TODO
        #' @param id TODO
        #' @return TODO
        add_subject = function(id) {

            ids <- self$data[[self$vars$subjid]]
            indexes <- which(ids == id)
            data_subject <- self$data[indexes, ]
            values <- data_subject[[self$vars$outcome]]
            is_missing <- is.na(values)
            group <- unique(data_subject[[self$vars$group]])
            existing_id <- id %in% names(self$ids)

            assert_that(
                length(indexes) >= 1,
                msg = sprintf("Subject %s is not found in the dataset", id)
            )

            assert_that(
                length(group) == 1,
                !is.na(group),
                is.factor(group),
                msg = sprintf(
                    paste0(
                        "Subject %s either belongs to multiple groups, or ",
                        "their group is missing, or group is not a factor"
                    ),
                    id
                )
            )

            assert_that(
                length(is_missing) == length(indexes),
                msg = sprintf("Subject %s has a mismatch between number of expected values", id)
            )

            assert_that(
                !existing_id,
                msg = sprintf("Subject %s already exists...", id)
            )

            self$group[[id]] <- group
            self$values[[id]] <- values
            self$is_mar[[id]] <- rep(TRUE, length(indexes))
            self$is_post_ice[[id]] <- rep(FALSE, length(indexes))
            self$strategies[[id]] <- "MAR"
            self$strategy_lock[[id]] <- FALSE
            self$indexes[[id]] <- indexes
            self$is_missing[[id]] <- is_missing
        },


        #' @description
        #' TODO
        #' @param ids TODO
        #' @return TODO
        validate_ids = function(ids) {
            is_in <- ids %in% self$ids
            if (!all(is_in)) {
                stop("subjids are not in self")
            }
            return(invisible(self))
        },


        #' @description
        #' TODO
        #' @return TODO
        sample_ids = function() {
            sample_ids(self$ids, self$strata)
        },


        #' @description
        #' TODO
        #' @param id TODO
        #' @return TODO
        extract_by_id = function(id) {
            list(
                is_mar = self$is_mar[[id]],
                is_missing = self$is_missing[[id]],
                strategy = self$strategies[[id]],
                group = self$group[[id]],
                data = self$get_data(id),
                outcome = self$values[[id]],
                strategy_lock = self$strategy_lock[[id]]
            )
        },

        #' @description
        #' TODO
        #' @param dat_ice TODO
        #' @return TODO
        update_strategies = function(dat_ice) {
            self$set_strategies(dat_ice, update = TRUE)
        },


        #' @description
        #' TODO
        #' @param dat_ice TODO
        #' @param update TODO
        #' @return TODO
        set_strategies = function(dat_ice = NULL, update=FALSE) {

            if (is.null(dat_ice)) {
                return(self)
            }

            validate_dataice(self$data, dat_ice, self$vars, update)

            dat_ice <- sort_by(dat_ice, c(self$vars$subjid))

            for (subject in dat_ice[[self$vars$subjid]]) {

                dat_ice_pt <- dat_ice[dat_ice[[self$vars$subjid]] == subject, ]

                assert_that(
                    nrow(dat_ice_pt) == 1,
                    msg = sprintf("Subject %s has more than 1 row in the ice dataset", subject)
                )

                new_strategy <- dat_ice_pt[[self$vars$strategy]]

                if (!update) {
                    self$visit_ice[[subject]] <- dat_ice_pt[[self$vars$visit]]
                } else {
                    if (self$strategy_lock[[subject]]) {
                        current_strategy <- self$strategies[[subject]]
                        if (current_strategy == "MAR" & new_strategy != "MAR") {
                            stop(paste(
                                "Updating strategies from MAR to non-MAR is invalid for subjects with post-ICE data",
                                "as these data points have already been used in fitting the imputation model"
                            ))
                        }
                        if (current_strategy != "MAR" & new_strategy == "MAR") {
                            warning(paste(
                                "Updating strategies from non-MAR to MAR for subjects with post-ICE data means",
                                "that the imputation model has been fitted without using all of the available data.",
                                "You are advised to re-run `draws()` applying this update there instead"
                            ))
                        }
                    }
                }

                visit <- self$visit_ice[[subject]]

                self$strategies[[subject]] <- new_strategy

                index <- which(self$visits == visit)

                if (new_strategy != "MAR") {
                    self$is_mar[[subject]] <- seq_along(self$visits) < index
                } else {
                    self$is_mar[[subject]] <- rep(TRUE, length(self$visits))
                }

                if (update) next()

                is_post_ice <- seq_along(self$visits) >= index
                self$is_post_ice[[subject]] <- is_post_ice

                # Lock strategy if patient has non-missing data post their ICE
                self$strategy_lock[[subject]] <- !all(
                    self$is_missing[[subject]][is_post_ice]
                )
            }
            self$check_has_data_at_each_visit()
        },


        #' @description
        #' TODO
        #' @return TODO
        check_has_data_at_each_visit = function() {
            is_mar <- unlist(self$is_mar, use.names = FALSE)
            is_not_miss <- !unlist(self$is_missing, use.names = FALSE)
            visits <- rep(self$visits, length(self$ids))
            is_avail <- is_mar & is_not_miss
            x <- tapply(is_avail, visits, sum)
            no_data_visits <- self$visits[x == 0]
            assert_that(
                length(no_data_visits) == 0,
                msg = paste(
                    sprintf(
                        "The data combined with the current ICE strategy has resulted in the %s visit(s)",
                        paste0("`", paste0(no_data_visits, collapse = "`, `"), "`")
                    ),
                    "not having any available observations to construct the imputation model on. Please either drop",
                    "these visit(s) or choose a different ICE strategy."
                )
            )
        },


        #' @description
        #' TODO
        #' @return TODO
        set_strata = function() {
            ## Use first row to determine strata i.e. no time varying strata
            strata_index <- unlist(
                lapply(self$indexes, function(x) x[[1]]),
                use.names = FALSE
            )
            strata_data <- self$data[strata_index, ]
            if (length(self$vars$strata) > 0) {
                self$strata <- as_strata(strata_data[, self$vars$strata])
            } else {
                self$strata <- rep(1, nrow(strata_data))
            }
        },


        #' @description
        #' TODO
        #' @param data TODO
        #' @param vars TODO
        #' @return TODO
        initialize = function(data, vars) {
            validate(vars)
            validate_datalong(data, vars)
            self$data <- sort_by(data, c(vars$subjid, vars$visit))
            self$vars <- vars
            subjects <- as.character(unique(self$data[[self$vars$subjid]]))
            for (id in subjects) self$add_subject(id)
            self$ids <- subjects
            self$ids_levels <- levels(self$data[[self$vars$subjid]])
            self$visits <- levels(self$data[[self$vars$visit]])
            self$set_strata()
            self$check_has_data_at_each_visit()
        }

    )
)




#' Title
#'
#' @param imputations TODO
transpose_imputations <- function(imputations) {
    len <- length(imputations)
    values <- vector(mode = "list", length = len)
    ids <- vector(mode = "list", length = len)

    for (i in seq_len(len)) {
        values[[i]] <- imputations[[i]]$values
        ids[[i]] <- imputations[[i]]$id
    }

    ids <- unlist(ids, use.names = FALSE)
    values <- unlist(values, use.names = FALSE)

    result <- list(
        ids = ids,
        values = values
    )
    return(result)
}
