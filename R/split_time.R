#' Split count-data rows into time intervals
#'
#' `split_time()` divides each count-data row into fixed-width or calendar
#' intervals. Event-time columns are used to allocate an observed aggregate
#' count to the resulting cells. Missing aggregate counts remain missing in
#' every resulting positive-duration cell.
#'
#' @param data A data frame with one row per original count period.
#' @param start Name of the row start variable. It must be numeric or a `Date`.
#' @param duration Name of the non-negative duration variable, measured in days
#'   for `Date` starts and in the same units as a numeric `start`.
#' @param outcome Name of the aggregate count variable.
#' @param event_times Optional character vector naming columns that contain
#'   one-based event-time offsets from `start`. These are required to allocate
#'   a positive observed count when a row intersects more than one interval.
#' @param interval A positive whole number giving a fixed interval width, or
#'   one of `"day"`, `"week"`, `"month"`, `"quarter"`, or `"year"`.
#'   Calendar months, quarters, and years require `Date` starts.
#' @param origin Optional interval origin. For fixed intervals this is the
#'   boundary immediately before interval zero. For calendar intervals it must
#'   be the first day of a month. Defaults to `0` for numeric starts and
#'   `1970-01-01` for `Date` starts. (Note that `interval = "week"`` with the default
#'   numeric origin 0 / `1970-01-01`` (a Thursday) produces Thursday-based weeks.)
#' @param period Optional name of the original period variable. When supplied,
#'   `split_period` combines this value with `split_interval`, which allows two
#'   original periods intersecting the same interval to remain distinct.
#'
#' @return A data frame with the original columns and six additional columns:
#'   `split_start`, `split_end`, `split_interval`, `split_period`,
#'   `split_duration`, and `split_outcome`. `split_period` is an ordered factor
#'   suitable for `set_vars(period = "split_period")`.
#'
#' @examples
#' dat <- data.frame(
#'     id = 1,
#'     period = "observed",
#'     start = 10,
#'     duration = 10,
#'     count = 2,
#'     event1 = 2,
#'     event2 = 9
#' )
#' split_time(
#'     dat,
#'     start = "start",
#'     duration = "duration",
#'     outcome = "count",
#'     event_times = c("event1", "event2"),
#'     interval = 7,
#'     origin = 0,
#'     period = "period"
#' )
#'
#' @export
split_time <- function(
    data,
    start,
    duration,
    outcome,
    event_times = NULL,
    interval,
    origin = NULL,
    period = NULL
) {
    data <- as_dataframe(data)
    assert_that(nrow(data) > 0, msg = "`data` must contain at least one row")
    if (length(event_times) == 0) {
        event_times <- NULL
    }
    input_names <- c(start, duration, outcome, event_times, period)
    output_names <- c(
        "split_start",
        "split_end",
        "split_interval",
        "split_period",
        "split_duration",
        "split_outcome"
    )
    assert_that(
        is_char_one(start),
        is_char_one(duration),
        is_char_one(outcome),
        is.null(period) || is_char_one(period),
        is.null(event_times) || is.character(event_times),
        all(input_names %in% names(data)),
        !any(output_names %in% names(data)),
        msg = "Invalid column specification for `split_time()`"
    )

    start_value <- data[[start]]
    duration_value <- data[[duration]]
    outcome_value <- data[[outcome]]
    is_date <- inherits(start_value, "Date")
    assert_that(
        is_date || is.numeric(start_value),
        !anyNA(start_value),
        is.numeric(duration_value),
        !anyNA(duration_value),
        all(is.finite(duration_value)),
        all(duration_value >= 0),
        all(duration_value == trunc(duration_value)),
        is.numeric(outcome_value),
        all(
            is.na(outcome_value) |
                (outcome_value >= 0 & outcome_value == trunc(outcome_value))
        ),
        msg = paste(
            "`start`, `duration`, and `outcome` must contain valid times,",
            "whole-number durations, and non-negative integer counts"
        )
    )

    interval_info <- count_interval_info(interval, origin, is_date)
    split_rows <- lapply(seq_len(nrow(data)), function(row) {
        split_time_row(
            source_row = data[row, , drop = FALSE],
            start = start,
            duration = duration,
            outcome = outcome,
            event_times = event_times,
            period = period,
            interval_info = interval_info,
            is_date = is_date
        )
    })
    result <- do.call(rbind, split_rows)
    rownames(result) <- NULL

    original_period <- if (is.null(period)) {
        rep("", nrow(result))
    } else {
        as.character(result[[period]])
    }
    original_levels <- unique(original_period)
    period_keys <- unique(data.frame(
        key = as.character(result$split_period),
        interval = result$split_interval,
        original = match(original_period, original_levels),
        stringsAsFactors = FALSE
    ))
    period_keys <- period_keys[
        order(
            period_keys$interval,
            period_keys$original
        ),
    ]
    result$split_period <- factor(
        result$split_period,
        levels = period_keys$key,
        ordered = TRUE
    )
    as_dataframe(result)
}


#' Normalize a count-data time interval specification
#'
#' @param interval A fixed interval width or supported calendar interval name,
#'   as accepted by [split_time()].
#' @param origin The user-supplied interval origin, or `NULL`.
#' @param is_date Logical indicating whether the row start variable inherits
#'   from `Date`.
#'
#' @return A list containing `origin` and either the fixed integer `width` or
#'   the number of `calendar_months` in the requested interval.
#'
#' @keywords internal
count_interval_info <- function(interval, origin, is_date) {
    calendar_intervals <- c(
        month = 1L,
        quarter = 3L,
        year = 12L
    )
    if (is.character(interval)) {
        assert_that(length(interval) == 1, !is.na(interval))
        interval <- sub("s$", "", tolower(interval))
        if (interval %in% c("day", "week")) {
            interval <- ife(interval == "day", 1, 7)
        } else {
            assert_that(
                interval %in% names(calendar_intervals),
                is_date,
                msg = paste(
                    "Calendar month, quarter, and year intervals require",
                    "`Date` start values"
                )
            )
            origin <- if (is.null(origin)) as.Date("1970-01-01") else origin
            assert_that(
                inherits(origin, "Date"),
                length(origin) == 1,
                !is.na(origin),
                format(origin, "%d") == "01",
                msg = "Calendar interval `origin` must be the first day of a month"
            )
            return(list(
                calendar_months = unname(calendar_intervals[[interval]]),
                origin = origin
            ))
        }
    }

    assert_that(
        is.numeric(interval),
        length(interval) == 1,
        is.finite(interval),
        interval > 0,
        interval == trunc(interval),
        msg = "Fixed `interval` must be one positive whole number"
    )
    if (is.null(origin)) {
        origin <- if (is_date) as.Date("1970-01-01") else 0
    }
    assert_that(
        length(origin) == 1,
        !is.na(origin),
        (is_date && inherits(origin, "Date")) ||
            (!is_date && is.numeric(origin) && is.finite(origin)),
        msg = "`origin` must have the same time scale as `start`"
    )
    list(width = as.integer(interval), origin = origin)
}


#' Split one count-data row into time intervals
#'
#' @param source_row A one-row data frame from the original count data.
#' @param start Name of the row start variable.
#' @param duration Name of the row duration variable.
#' @param outcome Name of the aggregate count variable.
#' @param event_times Optional names of the event-time offset columns.
#' @param period Optional name of the original period variable.
#' @param interval_info A normalized interval specification returned by
#'   [count_interval_info()].
#' @param is_date Logical indicating whether the row start variable inherits
#'   from `Date`.
#'
#' @return A data frame containing one row per interval intersected by
#'   `source_row`, with split timing, duration, period, and outcome columns.
#'
#' @keywords internal
split_time_row <- function(
    source_row,
    start,
    duration,
    outcome,
    event_times,
    period,
    interval_info,
    is_date
) {
    row_start <- source_row[[start]]
    row_duration <- source_row[[duration]]
    row_outcome <- source_row[[outcome]]

    if (row_duration == 0) {
        interval_index <- count_interval_index(
            row_start,
            interval_info,
            is_date
        )
        cell_start <- row_start
        cell_end <- row_start
        cell_duration <- 0
    } else if (!is.null(interval_info$calendar_months)) {
        row_end <- row_start + row_duration - 1
        first_index <- count_interval_index(row_start, interval_info, is_date)
        last_index <- count_interval_index(row_end, interval_info, is_date)
        interval_index <- seq.int(first_index, last_index)
        boundaries <- count_month_boundaries(interval_index, interval_info)
        next_boundaries <- count_month_boundaries(
            interval_index + 1L,
            interval_info
        )
        cell_start <- pmax(row_start, boundaries)
        cell_end <- pmin(row_end, next_boundaries - 1)
        cell_duration <- as.numeric(cell_end - cell_start) + 1
    } else {
        numeric_start <- as.numeric(row_start)
        numeric_origin <- as.numeric(interval_info$origin)
        row_end <- numeric_start + row_duration - 1
        first_index <- floor(
            (numeric_start - numeric_origin) / interval_info$width
        )
        last_index <- floor(
            (row_end - numeric_origin) / interval_info$width
        )
        interval_index <- seq.int(first_index, last_index)
        cell_start <- pmax(
            numeric_start,
            numeric_origin + interval_index * interval_info$width
        )
        cell_end <- pmin(
            row_end,
            numeric_origin + (interval_index + 1) * interval_info$width - 1
        )
        cell_duration <- cell_end - cell_start + 1
        if (is_date) {
            cell_start <- as.Date(cell_start, origin = "1970-01-01")
            cell_end <- as.Date(cell_end, origin = "1970-01-01")
        }
    }

    number_cells <- length(interval_index)
    result <- source_row[rep(1, number_cells), , drop = FALSE]
    result$split_start <- cell_start
    result$split_end <- cell_end
    result$split_interval <- as.integer(interval_index)
    original_period <- if (is.null(period)) {
        NULL
    } else {
        as.character(source_row[[period]])
    }
    result$split_period <- if (is.null(original_period)) {
        as.character(interval_index)
    } else {
        paste(original_period, interval_index, sep = ":")
    }
    result$split_duration <- as.numeric(cell_duration)

    if (is.na(row_outcome)) {
        result$split_outcome <- NA_integer_
        return(result)
    }
    if (row_duration == 0) {
        assert_that(
            row_outcome == 0,
            msg = "A zero-duration row cannot have a positive count"
        )
        result$split_outcome <- 0L
        return(result)
    }

    event_offsets <- if (is.null(event_times)) {
        numeric()
    } else {
        as.numeric(source_row[1, event_times, drop = TRUE])
    }
    event_offsets <- event_offsets[!is.na(event_offsets)]
    assert_that(
        all(is.finite(event_offsets)),
        all(event_offsets >= 1 & event_offsets <= row_duration),
        msg = "Event-time offsets must fall within their original row"
    )
    if (number_cells > 1 && row_outcome > 0 && is.null(event_times)) {
        stop(
            "`event_times` is required to split a positive count across intervals",
            call. = FALSE
        )
    }
    if (!is.null(event_times)) {
        assert_that(
            length(event_offsets) == row_outcome,
            msg = paste(
                "The number of non-missing event times must equal the observed",
                "aggregate count"
            )
        )
    }

    if (length(event_offsets) == 0) {
        result$split_outcome <- c(
            as.integer(row_outcome),
            rep(0L, number_cells - 1)
        )
    } else {
        event_day <- as.numeric(row_start) + event_offsets - 1
        result$split_outcome <- vapply(
            seq_len(number_cells),
            function(cell) {
                sum(
                    event_day >= as.numeric(cell_start[cell]) &
                        event_day <= as.numeric(cell_end[cell])
                )
            },
            integer(1)
        )
    }
    result
}


#' Find the interval index for a time value
#'
#' @param value A numeric or `Date` time value.
#' @param interval_info A normalized interval specification returned by
#'   [count_interval_info()].
#' @param is_date Logical indicating whether `value` inherits from `Date`.
#'
#' @return A single integer interval index relative to the normalized origin.
#'
#' @keywords internal
count_interval_index <- function(value, interval_info, is_date) {
    if (is.null(interval_info$calendar_months)) {
        return(as.integer(floor(
            (as.numeric(value) - as.numeric(interval_info$origin)) /
                interval_info$width
        )))
    }
    value_parts <- as.POSIXlt(value)
    origin_parts <- as.POSIXlt(interval_info$origin)
    month_difference <-
        (value_parts$year - origin_parts$year) *
        12L +
        value_parts$mon -
        origin_parts$mon
    as.integer(floor(month_difference / interval_info$calendar_months))
}


#' Calculate calendar interval boundaries
#'
#' @param index An integer vector of interval indices.
#' @param interval_info A normalized calendar interval specification returned
#'   by [count_interval_info()].
#'
#' @return A `Date` vector containing the first day of each requested calendar
#'   interval.
#'
#' @keywords internal
count_month_boundaries <- function(index, interval_info) {
    origin_parts <- as.POSIXlt(interval_info$origin)
    origin_month <- (origin_parts$year + 1900L) * 12L + origin_parts$mon
    month_number <- origin_month + index * interval_info$calendar_months
    year <- month_number %/% 12L
    month <- month_number %% 12L + 1L
    as.Date(sprintf("%04d-%02d-01", year, month))
}
