#
#
#
# ### Example bootstrap function that takes an input grouping vector for the strata and returns indexes to sample
# bootstrap_index <- function(group){
#     indexes_list <- tapply(
#         X = 1:length(group),
#         INDEX = group,
#         FUN = function(x) sample(x, replace= TRUE, size = length(x))
#     )
#     indexes <- unlist(indexes_list)
#     names(indexes) <- NULL
#     return(indexes)
# }
#
#
# ### Function to collapse multiple strata into a single grouping vector
# strata <- collapse_strata( vector_1(), vector_2(), ...)
#
