#' Antidepressant trial data
#'
#' A dataset containing data from a publicly available example data set from an antidepressant
#' clinical trial.
#' The dataset is available on the website of the
#' [Drug Information Association Scientific Working Group on Estimands and Missing Data](https://www.lshtm.ac.uk/research/centres-projects-groups/missing-data#dia-missing-data).
#' As per that website, the original data are from an antidepressant clinical trial with four
#' treatments; two doses of an experimental medication,
#' a positive control, and placebo and was published in Goldstein et al (2004). To mask the real
#' data, week 8 observations were removed and two arms were created:
#' the original placebo arm and a "drug arm" created by randomly selecting patients from the
#' three non-placebo arms.
#'
#' The relevant endpoint is the Hamilton 17-item rating scale for depression (HAMD17) for
#' which baseline and weeks 1, 2, 4, and 6 assessments are included.
#' Study drug discontinuation occurred in 24% subjects from the active drug and 26% from
#' placebo.
#' All data after study drug discontinuation are missing and there is a single additional
#' intermittent missing observation.
#'
#' @format A `data.frame` with 608 rows and 11 variables:
#'   - `PATIENT`: patients IDs.
#'   - `HAMATOTL`: total score Hamilton Anxiety Rating Scale.
#'   - `PGIIMP`: patient's Global Impression of Improvement Rating Scale.
#'   - `RELDAYS`: number of days between visit and baseline.
#'   - `VISIT`: post-baseline visit. Has levels 4,5,6,7.
#'   - `THERAPY`: the treatment group variable. It is equal to `PLACEBO` for observations from
#' the placebo arm, or `DRUG` for observations from the active arm.
#'   - `GENDER`: patient's gender.
#'   - `POOLINV`: pooled investigator.
#'   - `BASVAL`: baseline outcome value.
#'   - `HAMDTL17`: Hamilton 17-item rating scale value.
#'   - `CHANGE`: change from baseline in the Hamilton 17-item rating scale.
#'
#' @references
#' Goldstein, Lu, Detke, Wiltse, Mallinckrodt, Demitrack. Duloxetine in the treatment of
#' depression: a double-blind placebo-controlled comparison with paroxetine.
#' J Clin Psychopharmacol 2004;24: 389-399.
#'
"antidepressant_data"
