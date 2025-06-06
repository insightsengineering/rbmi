% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/controls.R
\name{control}
\alias{control}
\alias{control_bayes}
\title{Control the computational details of the imputation methods}
\usage{
control_bayes(
  warmup = 200,
  thin = 50,
  chains = 1,
  init = ifelse(chains > 1, "random", "mmrm"),
  seed = sample.int(.Machine$integer.max, 1),
  ...
)
}
\arguments{
\item{warmup}{a numeric, the number of warmup iterations for the MCMC sampler.}

\item{thin}{a numeric, the thinning rate of the MCMC sampler.}

\item{chains}{a numeric, the number of chains to run in parallel.}

\item{init}{a character string, the method used to initialise the MCMC sampler, see the details.}

\item{seed}{a numeric, the seed used to initialise the MCMC sampler.}

\item{...}{additional arguments to be passed to \code{\link[rstan:stanmodel-method-sampling]{rstan::sampling()}}.}
}
\description{
These functions control lower level computational details of the imputation methods.
}
\details{
Currently only the Bayesian imputation via \code{\link[=method_bayes]{method_bayes()}} uses a control function:
\itemize{
\item The \code{init} argument can be set to \code{"random"} to randomly initialise the sampler with \code{rstan}
default values or to \code{"mmrm"} to initialise the sampler with the maximum likelihood estimate
values of the MMRM.
\item The \code{seed} argument is used to set the seed for the MCMC sampler. By default, a random seed
is generated, such that outside invocation of the \code{set.seed()} call can effectively set the
seed.
\item The samples are split across the chains, such that each chain produces \code{n_samples / chains}
(rounded up) samples. The total number of samples that will be returned across all chains is \code{n_samples}
as specified in \code{\link[=method_bayes]{method_bayes()}}.
\item Therefore, the additional parameters passed to \code{\link[rstan:stanmodel-method-sampling]{rstan::sampling()}} must not contain
\code{n_samples} or \code{iter}. Instead, the number of samples must only be provided directly via the
\code{n_samples} argument of \code{\link[=method_bayes]{method_bayes()}}. Similarly, the \code{refresh} argument is also not allowed
here, instead use the \code{quiet} argument directly in \code{\link[=draws]{draws()}}.
}
}
\note{
For full reproducibility of the imputation results, it is required to use a \code{set.seed()} call
before defining the \code{control} list, and calling the \code{draws()} function. It is not sufficient to
merely set the \code{seed} argument in the \code{control} list.
}
