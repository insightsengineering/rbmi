

#' Title
#'
#' @param ...  TODO
#'
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


#' Title
#'
#' @param ids  TODO
#' @param strata  TODO
#'
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
