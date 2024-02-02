fill_date_gaps <- function (df) {

    if (!methods::is(df$date, 'Date')) {
        df$date <- as.Date(df$date)
    }
    date_sequence <- data.frame(
        date = seq(min(df$date),
                   max(df$date),
                   by = "days"))
    df_with_all_dates <- merge(df, date_sequence,
                               by = 'date',
                               all.x = FALSE, all.y = TRUE)
    df_with_all_dates
}
