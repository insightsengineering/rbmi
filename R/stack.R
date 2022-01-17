#' R6 Class for a FIFO stack
#'
#' @description
#'
#' This is a simple stack object offering add / pop functionality
#'
#' @import R6
#' @export
Stack <- R6::R6Class(
    classname = "Stack",
    public = list(
        #' @field stack A list containing the current stack
        stack = list(),

        #' @description
        #' Adds content to the end of the stack (must be a list)
        #' @param x content to add to the stack
        add = function(x) {
            self$stack <- append(self$stack, x)
        },

        #' @description
        #' Retrieve content from the stack
        #' @param i the number of items to retrieve from the stack. If there are less than `i`
        #' items left on the stack it will just return everything that is left.
        pop = function(i) {
            assert_that(
                length(self$stack) > 0,
                msg = "Stack doesn't have any items to return"
            )
            i2 <- min(i, length(self$stack))
            index <- seq_len(i2)
            res <- self$stack[index]
            self$stack <- self$stack[-index]
            return(res)
        }
    )
)


#' Creates a stack object populated with bootstrapped samples
#'
#' Function creates a [Stack()] object and populated the stack with bootstrap
#' samples based upon `method$n_samples`
#'
#' @param longdata A [longDataConstructor()] object
#' @param method A `method` object
#' @param stack A [Stack()] object (this is only exposed for unit testing purposes)
get_bootstrap_stack <- function(longdata, method, stack = Stack$new()) {
    max_samples <- ceiling(method$threshold * method$n_samples) + method$n_samples
    stack$add(replicate(
        n = max_samples,
        expr = {
            longdata$sample_ids()
        },
        simplify = FALSE
    ))
    return(stack)
}


#' Creates a stack object populated with jackknife samples
#'
#' Function creates a [Stack()] object and populated the stack with jackknife
#' samples based upon
#'
#' @param longdata A [longDataConstructor()] object
#' @param method A `method` object
#' @param stack A [Stack()] object (this is only exposed for unit testing purposes)
get_jackknife_stack <- function(longdata, method, stack = Stack$new()) {
    ids <- longdata$ids
    stack$add(
        lapply(seq_along(ids), function(i) ids[-i])
    )
    return(stack)
}
