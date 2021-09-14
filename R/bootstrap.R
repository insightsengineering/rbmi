

#' Create vector of Stratas
#'
#' Collapse multiple categorical variables into distinct unique categories.
#' e.g.
#' ```
#'
#' ```
#' would return
#' ```
#' c(1,2,3,3,4,1)
#' ```
#'
#' @param ... numeric/character/factor vectors of the same length
#' @examples
#' \dontrun{
#' as_strata(c(1,1,2,2,2,1), c(5,6,5,5,6,5))
#' }
as_strata <- function(...){
    x <- list(...)
    assert_that(length(unique(vapply(x, length, numeric(1)))) == 1 )
    df <- as.data.frame(x)
    colnames(df) <- paste0("var", 1:length(x))
    df_unique <- unique(df)
    df_unique[,"ID"] <- seq_len(nrow(df_unique))
    df[,"ORDER"] <- seq_len(nrow(df))
    df_mapped <- merge(df, df_unique)
    df_mapped[["ID"]][order(df_mapped[["ORDER"]])]
}


#' Sample Patient Ids
#'
#' Performs a stratified bootstrap sample of IDS
#' ensuring the return vector is the same length as the input vector
#'
#' @param ids  vector to sample from
#' @param strata  strata indicator, ids are sampled within each strata
#' ensuring the that the numbers of each strata are maintained
#'
#' @examples
#' \dontrun{
#' sample_ids( c("a", "b", "c", "d"), strata = c(1,1,2,2))
#' }
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
