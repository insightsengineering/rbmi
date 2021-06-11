
# impute.stochastic <- function(draws, reference){
#     Map(
#         impute_data,
#         draws@samples,
#         data = draws@data,
#         vars = draws@vars ,
#         formula = draws@formula,
#         references = references,
#         strategy = draws@strategy
#     )
# }
#
# impute.deterministic <- function(draws, reference){
#
#     fun <- function(draw, data, ...){
#         index <- draw@index
#         dat <- data[index,]
#         impute_data(
#             draw = draw,
#             data = dat,
#             ...
#         )
#     }
#
#     Map(
#         fun,
#         draws@samples,
#         data = draws@data,
#         vars = draws@vars ,
#         formula = draws@formula,
#         references = references,
#         strategy = draws@strategy
#     )
# }
#
#
#
#
# impute_data <- function(sample, data, vars, formula, references, strategy){
#     validate_references(data[,vars$group], references)
#     pts <- unique(data[,vars$subjid])
#
#     fun <- function(pt){
#         index <- dat[,vars$subjid == pt]
#         dat_pt <- dat[, index]
#         impute_data_individual(
#             dat_pt,
#             vars,
#             formula,
#             references,
#             strategy,
#             sample$beta,
#             sample$sigma
#         )
#     }
#     Map(fun, pts)
# }
#
# impute_data_individual <- function(
#     dat_pt,
#     vars,
#     formula,
#     references,
#     strategy,
#     beta,
#     sigma,
#     type
# ){
#     index_missing <- is.na(dat_pt[, vars$outcome])
#
#     if(!any(index_missing)){
#         return(c())
#     }
#
#
#     group_pt <- unique(dat_pt[,vars$group])
#     group_ref <- reference[group_pt]
#
#     dat_ref <- dat_pt
#     dat_ref[,vars$group] <- group_ref
#
#     parameters_group <- list(
#         mu = get_mu( dat_pt, formula, beta),
#         sigma = sigma[group_pt]
#     )
#
#     parameters_reference <- list(
#         mu = get_mu( dat_ref, formula, beta),
#         sigma = sigma[group_ref]
#     )
#
#     pars <- merge_parameters(
#         parameters_group,
#         parameters_reference,
#         strategy = strategy,
#         index = index_mar
#     )
#
#     conditional_parameters <- get_conditional_parameters(pars, index_missing)
#     imputed_outcome <- impute_outcome(conditional_parameters)
#     dat_pr[index_missing,vars$outcome] <- imputed_outcome
#
#     return(dat_pr)
# }
#
#
# impute_outcome <- function(conditional_parameters){
#     # TODO
# }
#
# get_conditional_parameters <- function(pars, index_missing){
#     # TODO
# }
#
# merge_parameters <- function(pars_group, pars_ref, strat = strat, mar_index = mar_index){
#     # TODO
# }
#
# get_mu <- function(dat_pr, frm, beta){
#     # TODO
# }
#
# validate_references <- function(group, references){
#     # TODO
# }
#
# strategy.MAR <- function(parameters_group, parameters_reference){
#     # TODO
# }
#
# strategy.JR <- function(parameters_group, parameters_reference){
#     # TODO
# }
#
# strategy.CR <- function(parameters_group, parameters_reference){
#     # TODO
# }
#
# strategy.CIR <- function(parameters_group, parameters_reference){
#     # TODO
# }
#
# strategy.LMCF <- function(parameters_group, parameters_reference){
#     # TODO
# }
#
#
#
#
#
#
