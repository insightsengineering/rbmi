#' Antidepressant trial data.
#'
#' A dataset containing data from a publicly available antidepressant clinical trial of an active drug versus placebo.
#' The dataset is available [here](https://www.lshtm.ac.uk/research/centres-projects-groups/missing-data#dia-missing-data).
#' The relevant endpoint is the Hamilton 17-item rating scale for depression (HAMD17) which was assessed at baseline and weeks 1, 2, 4, and 6.
#' Study drug discontinuation occurred in 24% subjects from the active drug and 26% from placebo.
#' All data after study drug discontinuation are missing and there is a single additional intermittent missing observation.
#'
#' @format A data frame with 608 rows and 11 variables:
#'   - `PATIENT`: patients IDs.
#'   - `HAMATOTL`: total score Hamilton Anxiety Rating Scale.
#'   - `PGIIMP`: patient’s Global Impression of Improvement Rating Scale.
#'   - `RELDAYS`: number of days between visit and baseline.
#'   - `VISIT`: post-baseline visit. Has levels 4,5,6,7.
#'   - `THERAPY`: the treatment group variable. It is equal to `PLACEBO` for observations
#'   from the placebo arm, or `DRUG` for observations from the active arm.
#'   - `GENDER`: patient's sex.
#'   - `POOLINV`: pooled investigator.
#'   - `BASVAL`: baseline outcome value.
#'   - `HAMDTL17`: Hamilton 17-item rating scale value.
#'   - `CHANGE`: change from baseline in the Hamilton 17-item rating scale.
#'
"antidepressant_data"
