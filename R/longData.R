

#' @export
longDataConstructor <- R6::R6Class(
    classname = "longData",

    public = list(

        data = NULL,
        vars = NULL,
        visits = NULL,
        ids = NULL,
        strata = NULL,
        is_mar = list(),
        strategies = list(),
        strategy_lock = list(),
        subjects = list(),
        indexes = list(),
        is_missing = list(),


        get_data = function(obj, nmar.rm = FALSE, na.rm = FALSE){

            if( ! any(c("list", "character") %in% class(obj))){
                stop("Invalid Input type")
            }

            listFlag <- "list" %in% class(obj)

            if(listFlag) {
                obj_expanded <- expand_imputations(obj)
                ids <- obj_expanded$ids
                values <- obj_expanded$values
            } else {
                ids <- obj
            }

            self$validate_ids(ids)
            indexes <- self$indexes[ids]

            if( listFlag | nmar.rm | na.rm){
                is_miss <- unlist(self$is_missing[ids], use.names = FALSE)
                is_mar <- unlist(self$is_mar[ids], use.names = FALSE)
            }

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

            if(listFlag){
                new_data[is_miss ,self$vars$outcome] <- values
            }

            if(nmar.rm | na.rm){
                keep <- (is_mar | (!nmar.rm)) & (!is_miss | (!na.rm))
                new_data <- new_data[keep,]
            }

            return(new_data)
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
                is_mar = rep(TRUE, length(indexes)),
                strategy = "MAR",
                group = group,
                longData = self
            )

            self$is_mar[[id]] <- rep(TRUE, length(indexes))
            self$strategies[[id]] <- "MAR"
            self$strategy_lock[[id]] <- FALSE
            self$indexes[[id]] <- indexes
            self$is_missing[[id]] <- is_missing
        },


        validate_ids = function(ids){
            is_in <- ids %in% names(self$subjects)
            if(! all(is_in)){
                stop("subjids are not in self")
            }
            return(invisible(self))
        },


        sample_ids = function(){
            sample_ids(self$ids, self$strata)
        },


        update_strategies = function(dat_ice) {
            self$set_strategies(dat_ice, update = TRUE)
        },


        set_strategies = function(dat_ice, update=FALSE) {

            #validate_data_ice(dat_ice, vars, self$visits)

            for( subject in dat_ice[[self$vars$subjid]]){

                dat_ice_pt <- dat_ice[dat_ice[[self$vars$subjid]] == subject,]
                stopifnot(nrow(dat_ice_pt) == 1)

                new_strategy <- dat_ice_pt[[self$vars$method]]
                visit <- dat_ice_pt[[self$vars$visit]]

                if(update){
                    if( self$strategy_lock[[subject]]){
                        current_strategy <- self$strategies[[subject]]
                        if(current_strategy == "MAR" &  new_strategy != "MAR"){
                            stop("Unable to change from MAR to non-MAR")
                        }
                        if(current_strategy!= "MAR" & new_strategy == "MAR"){
                            stop("Unable to change from non-MAR to MAR")
                        }
                    }
                }

                self$strategies[[subject]] <- new_strategy
                self$subjects[[subject]]$strategy <- new_strategy

                if(update) next()

                index <- which(self$visits == visit)

                if( new_strategy != "MAR"){
                    is_mar <- seq_along(self$visits) < index
                } else {
                    is_mar <- rep(TRUE, length(self$visits))
                }

                self$is_mar[[subject]] <- is_mar
                self$subjects[[subject]]$is_mar <- is_mar

                self$strategy_lock[[subject]] <- any(
                    !self$is_missing[[subject]][seq_along(self$visits) >= index]
                )
            }
        },


        set_strata = function(){
            ## Use first row to determine strata i.e. no time varying strata
            strata_index <- unlist(
                lapply(self$indexes, function(x) x[1]),
                use.names = FALSE
            )
            strata_data <- self$data[strata_index,]

            if(length(vars$strata) > 0){
                self$strata = as_strata(strata_data[,vars$strata])
            } else {
                self$strata = rep(1, nrow(strata_data))
            }
        },


        process_data = function(){
            subjects = unique(self$data[[self$vars$subjid]])
            for( id in subjects) self$add_subject(id)
            self$ids = names(self$subjects)
            self$visits = levels(self$data[[self$vars$visit]])
            self$set_strata()
        },


        initialize = function(data, vars){
            validate_datalong(data, vars)
            self$data = data
            self$vars = vars
            self$process_data()
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


sample_ids <- function(ids, strata = rep(1, length(ids))){
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


expand_imputations = function(imputations){

    len <- length(imputations)
    values <- vector(mode = "list", length = len)
    ids <- vector(mode = "list", length = len)

    for( i in seq_len(len)){
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


















