

#' @export
rmDataConstructor <- R6::R6Class(
    classname = "rmData",

    public = list(
        data = NULL,
        vars = NULL,
        visits = NULL,
        subjects = list(),
        indexes = list(),
        is_missing = list(),

        get_data = function(obj){
            if("list" %in% class(obj)) {
                return(private$get_data_list(obj))
            } else if( "character" %in% class(obj)){
                return(private$get_data_ids(obj))
            } else {
                stop("Invalid type")
            }
        },


        add_subject = function(id) {
            ids <- self$data[[self$vars$subjid]]
            indexes <- which(ids == id)
            data_subject <- self$data[indexes,]
            is_missing <- is.na(data_subject[[self$vars$outcome]])
            group <- unique(data_subject[[self$vars$group]])
            existing_id <- id %in% names(self$subjects)
            stopifnot(
                length(indexes) >= 1,
                length(group) == 1,
                length(is_missing) == length(indexes),
                !existing_id
            )
            self$subjects[[id]] <- list(
                subjid = id,
                indexes = indexes,
                is_missing = is_missing,
                group = group,
                rmData = self
            )

            self$indexes[[id]] <- indexes
            self$is_missing[[id]] <- is_missing
        },


        validate_ids = function(ids){
            is_in <- ids %in% names(self$subjects)
            if(! all(is_in)){
                stop("subjids are not in self")
            }
            return(invisible(is_in))
        },

        add_strategy = function(x) {},
        get_strategy = function(x) {},

        initialize = function(data, vars){
            self$data = data
            self$vars = vars

            keyvars <- c(
                vars$outcome,
                vars$group,
                vars$visit,
                vars$subjid
            )

            stopifnot(
                all(keyvars %in% names(data)),
                is.character(data[[vars$subjid]]) | is.factor(data[[vars$subjid]])
            )

            subjects = unique(data[[vars$subjid]])
            for( id in subjects){
                self$add_subject(id)
            }
        }

    ),
    private = list(
        get_data_list = function(imputations){
            len <- length(imputations)
            values <- vector(mode = "list", length = len)
            new_ids <- vector(mode = "list", length = len)
            ids <- vector(mode = "list", length = len)

            for( i in seq_len(len)){
                values[[i]] <- imputations[[i]]$values
                ids[[i]] <- imputations[[i]]$id
            }

            ids <- unlist(ids, use.names = FALSE)
            self$validate_ids(ids)

            indexes <- self$indexes[ids]


            new_ids <- mapply(
                function(x,y) rep(paste0("new_pt_",x), times = length(y)),
                seq_along(indexes),
                indexes,
                SIMPLIFY = FALSE
            )

            new_ids <- unlist(new_ids, use.names = FALSE)
            indexes <- unlist(indexes, use.names = FALSE)
            values <- unlist(values, use.names = FALSE)
            missing <- unlist(self$is_missing[ids], use.names = FALSE)

            stopifnot(
                sum(missing) == length(values),
                length(new_ids) == length(indexes)
            )

            dat <- self$data[indexes,]
            dat[missing, self$vars$outcome] <- values
            dat[,self$vars$subjid] <- new_ids
            return(dat)
        },


        get_data_ids = function(ids) {
            self$validate_ids(ids)
            indexes <- self$indexes[ids]

            new_ids <- mapply(
                function(x,y) rep(paste0("new_pt_",x), times = length(y)),
                seq_along(indexes),
                indexes,
                SIMPLIFY = FALSE
            )

            new_ids <- unlist(new_ids, use.names = FALSE)
            indexes <- unlist(indexes, use.names = FALSE)

            new_data <- self$data[indexes,]
            new_data[[self$vars$subjid]] <- new_ids

            return(new_data)
        }
    )
)






















