devtools::load_all()
data <- haven::read_sas("~/Downloads/chapter15_example.sas7bdat")
summary(data)
apply(data,2,is.numeric)
apply(data,2,is.factor)

data$PATIENT <- as.factor(data$PATIENT)
data$VISIT <- as.factor(data$VISIT)
data$change <- as.numeric(data$change)
data$basval <- as.numeric(data$basval)
data$THERAPY <- as.factor(data$THERAPY)

data[is.na(data)] = 1

vars <- ivars(
    outcome = "change",
    subjid = "PATIENT",
    visit = "VISIT",
    group = "THERAPY",
    covariates = "basval"
)

method <- method_bootstrap(n_samples = 5, covariance = "us" ) 

levels_visit = levels(data$VISIT)
J = nlevels(data$VISIT)

expand_patient <- function(subject, levels_visit) {

    n_visits <- length(levels_visit)

    if(!is.data.frame(subject)) {
        subject <- as.data.frame(subject)
    }

    obs_visits <- subject$VISIT
    if(length(obs_visits) < n_visits) {

        miss_visits <- setdiff(levels_visit, obs_visits)

        for(i in 1:length(miss_visits)) {
            miss_vis_index <- which(levels_visit == miss_visits[i])
            last_visit <- levels_visit[miss_vis_index-1]
            last <- subject[subject$VISIT == last_visit,]
            last$VISIT <- miss_visits[i]
            last$change = NA
            subject_list <- split(subject, as.character(subject$VISIT))
            subject_list <- append(subject_list, list(last), after = miss_vis_index - 1)
            subject <- do.call(rbind, subject_list)
        }

    }

    return(subject)
}

data_exp <- do.call(rbind,
                    lapply(
                        split(data, data$PATIENT),
                        function(x) expand_patient(x, levels_visit)
                    ))

data_ice <- data_exp %>%
    group_by(PATIENT) %>%
    summarise(
        "strategy" = "CR",
        "visit" = ifelse(any(is.na(change)), VISIT[is.na(change)][1], J+1)
    )

draws_boot <- draws(
    data_exp,
    data_ice = data_ice,
    vars,
    method
)
