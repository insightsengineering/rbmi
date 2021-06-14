

#' @export
rmDataConstructor <- R6::R6Class(
    classname = "rmData",

    public = list(
        data = NULL,
        vars = NULL,
        visits = NULL,
        ids = NULL,
        strata = NULL,
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

        sample_ids = function(){
            sample_ids(self$ids, self$strata)
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
                vars$subjid,
                vars$strata
            )

            stopifnot(
                all(keyvars %in% names(data)),
                is.character(data[[vars$subjid]]) | is.factor(data[[vars$subjid]])
            )

            subjects = unique(data[[vars$subjid]])
            for( id in subjects){
                self$add_subject(id)
            }
            self$ids = names(self$subjects)

            strata_index <- unlist(
                lapply(self$indexes, function(x) x[1]),
                use.names = FALSE
            )
            strata_data <- data[strata_index,]

            if(length(vars$strata) > 0){
                self$strata = as_strata(strata_data[,vars$strata])
            } else {
                self$strata = rep(1, nrow(strata_data))
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


as_strata <- function(...){
    x <- list(...)
    df <- as.data.frame(x)
    colnames(df) <- paste0("var", 1:length(x))
    df_unique <- unique(df)
    df_unique[,"ID"] <- seq_len(nrow(df_unique))
    df[,"ORDER"] <- seq_len(nrow(df))
    df_mapped <- merge(df, df_unique)
    df_mapped[["ID"]][order(df_mapped[["ORDER"]])]
}


sample_ids <- function(ids, strata){
    res <- tapply(
        X = ids,
        INDEX = strata,
        FUN = function(x) {
            y <- sample(length(x), size = length(x), replace = TRUE)
            x[y]
        }
    )
    return(unlist(res,use.names = FALSE))
}

# sample_ids( c(1,2,3) , c(1,2,1))
#
# sample(x = 3, size = length(3), replace= TRUE)
#
# as_strata( c(1,2,3), c(1,2,3))
#
# as_strata(
#     c(5,1,1,2,2,2,3,3,3,5),
#     c(5,1,2,2,2,3,3,3,4,5)
# )


















