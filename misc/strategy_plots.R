library(dplyr)
library(ggplot2)
library(scales)


get_plot <- function(d1, d2, title) {
    dat <- bind_rows(d1, d2) %>%
        mutate(type = factor(type, labels = c("Observed", "Missing")))

    ggplot(dat, aes(x = x, y = y, linetype = type, col = group)) +
        geom_point() +
        geom_path() +
        theme_bw() +
        xlab("") +
        ylab("") +
        theme(legend.position = "bottom") +
        scale_color_discrete(name = "") +
        scale_linetype(name = "") +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        scale_x_continuous(breaks = pretty_breaks(10)) +
        ggtitle(title) +
        theme(
            axis.ticks = element_blank(),
            axis.text = element_blank()
        )
}


d2 <- tibble(
    x = 1:9,
    y = x * 1,
    type = "A",
    group = "Control"
)

d1 <- tibble(
    x = c(1, 2, 3, 4, 5, 6, 6, 7, 8, 9),
    type = c("A", "A", "A", "A", "A", "A", "B", "B", "B", "B"),
    group = "Treatment"
)


d1$y <- c(2, 4, 6, 8, 10, 12, 12, 14, 16, 18)
get_plot(d1, d2, "Strategy: MAR")


d1$y <- c(3, 6, 9, 12, 15, 18, 18, 19, 20, 21)
get_plot(d1, d2, "Strategy: CIR")


d1$y <- c(2, 4, 6, 8, 10, 12, 12, 7, 8, 9) + 0.08
get_plot(d1, d2, "Strategy: JR")


d1$y <- c(2, 4, 6, 8, 10, 12, 12, 11, 10.5, 10) + 0.08
get_plot(d1, d2, "Strategy: CR")
