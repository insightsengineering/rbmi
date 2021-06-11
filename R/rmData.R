

subjectConstructor <- R6::R6Class(
    classname = "subject",
    public = list(
        subjid = NULL,
        indexes = NULL,
        is_missing = NULL,
        is_mar = NULL,
        strategy = NULL,
        group = NULL,
        rmData = NULL,
        initialize = function(
            subjid,
            indexes,
            is_missing,
            group,
            rmData,
            is_mar = NULL,
            strategy = NULL
        ){
            self$subjid <- subjid
            self$indexes <- indexes
            self$is_missing <- is_missing
            self$is_mar <- is_mar
            self$strategy <- strategy
            self$group <- group
            self$rmData <- rmData
        }
    )
)


rmDataConstructor <- R6::R6Class(
    classname = "rmData",
    public = list(
        data = NULL,
        vars = NULL,
        visits = NULL,
        subjects = list(),

        get_data = function(ids) {
            self$validate_ids(ids,throw_error = TRUE)

            count <- tapply(ids, ids, length)
            subjects <- self$subjects
            indexes_list <- lapply(
                names(count),
                function(id) subjects[[id]]$indexes
            )

            indexes_list_rep <- mapply(
                function(x,y) rep(x, times = y),
                indexes_list,
                count,
                SIMPLIFY = FALSE
            )

            lens_list <- lapply(
                indexes_list,
                function(x) length(x)
            )

            lens_list_rep <- mapply(
                function(x,y) rep(x, times = y),
                lens_list,
                count,
                SIMPLIFY = FALSE
            )

            lens <- unlist(lens_list_rep)
            indexes <- unlist(indexes_list_rep)

            new_subjid <- paste0("new_subjid_", seq_along(lens))
            new_subjid_column <- rep(new_subjid, times = lens)

            new_data <- self$data[indexes,]
            new_data[[self$vars$subjid]] <- new_subjid_column

            return(new_data)
        },

        add_subject = function(id) {
            vars <- self$vars
            ids <- self$data[[vars$subjid]]
            indexes <- which(ids == id)
            data_subject <- self$data[indexes,]
            is_missing <- is.na(data_subject[[vars$outcome]])
            group <- unique(data_subject[[vars$group]])
            existing_id <- self$validate_ids(id)
            stopifnot(
                length(indexes) >= 1,
                length(group) == 1,
                length(is_missing) == length(indexes),
                !existing_id
            )
            self$subjects[[id]] <- subjectConstructor$new(
                subjid = id,
                indexes = indexes,
                is_missing = is_missing,
                group = group,
                rmData = self,
            )
        },

        validate_ids = function(ids, throw_error = FALSE){
            is_in <- ids %in% names(self$subjects)
            if( throw_error){
                if(! all(is_in)){
                    stop("subjids are not in self")
                }
            }
            return(invisible(is_in))
        },

        get_subject = function(id) {
            return(self$subjects[[id]])
        },

        add_strategy = function(x) {},
        get_strategy = function(x) {},

        initialize = function(data, vars){
            self$data = data
            self$vars = vars
            self$visits

            for( id in unique(data[[vars$subjid]])){
                self$add_subject(id)
            }
        }
    )
)





# rmd <- rmDataConstructor$new(
#     data = dat,
#     vars = list(
#         outcome = "outcome",
#         visit = "visit",
#         subjid = "pt",
#         group = "group"
#     )
# )
#
#
# rmd$get_data(c(1,2,2,2,3))













)


