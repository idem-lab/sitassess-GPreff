# Sit assessment specific script
# Read in latest linelist file and set target dates for modelling

linelist_file <- "../sitassess-data/processed_linelist_20231102.RDS"
linelist_all_dates <- readRDS(linelist_file)

# make target dates
target_dates <- as.character(
    seq.Date(as.Date("2021-06-01"),
             as.Date("2021-12-31"),
             by = "day"))

# sitassess specific clean PCR data
# fill in target_dates and gaps in dates/jurisdictions
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


notif_dat <- linelist_all_dates |>
    dplyr::filter(test_type == 'PCR',
                  date_confirmation %in% target_dates,
                  import_status == 'local') |>
    dplyr::select(c(date_confirmation, state)) |>
    dplyr::rename(date = date_confirmation,
                  jurisdiction = state) |>
    dplyr::group_by(date, jurisdiction) |>
    dplyr::summarise(count = dplyr::n()) |>
    tidyr::pivot_wider(id_cols = date,
                       names_from = jurisdiction,
                       values_from = count,
                       values_fn = sum,
                       values_fill = 0) |>
    fill_date_gaps() |>
    tidyr::pivot_longer(!date,
                        names_to = 'jurisdiction',
                        values_to = 'count')


#
#
#
# # sitassess specific clean RAT data
# # including remove invalid dates as noted in deprecated 'make_rat_validity_matrix'
# linelist_clean_rat <- linelist_all_dates |>
#     dplyr::filter(test_type == 'RAT',
#                   date_confirmation %in% target_dates,
#                   import_status == 'local',
#                   !(date_confirmation == as.Date('2023-06-30') & state == 'VIC'),
#                   !(date_confirmation > as.Date('2023-08-31') & state == 'QLD'),
#                   !(date_confirmation > as.Date('2023-09-30') & state == 'NSW'),
#                   !(date_confirmation > as.Date('2023-10-08') & state == 'WA')) |>
#     dplyr::select(c(date_confirmation, state))
#
#
#
# # ====
# # create proportion matrices
# linelist_clean_pcr$test_type <- 'PCR'
# linelist_clean_rat$test_type <- 'RAT'
# test_type_proportions <- rbind(linelist_clean_pcr, linelist_clean_rat) |>
#     dplyr::group_by(date_confirmation, state, test_type) |>
#     dplyr::summarise(n_cases = dplyr::n()) |>
#     tidyr::pivot_wider(id_cols = c(date_confirmation, state),
#                        names_from = test_type,
#                        values_from = n_cases,
#                        values_fill = NA) |>
#     dplyr::mutate(total = sum(PCR, RAT, na.rm = TRUE)) |>
#     dplyr::mutate(prop_PCR = PCR / total) |>
#     dplyr::mutate(prop_RAT = 1 - prop_PCR)
#
# pcr_prop <- test_type_proportions |>
#     tidyr::pivot_wider(
#         id_cols = date_confirmation,
#         names_from = state,
#         values_from = prop_PCR,
#         values_fn = mean,
#         values_fill = NA) |>
#     fill_date_gaps() |>
#     tibble::column_to_rownames(var = 'date_confirmation') |>
#     data.frame()
#
# rat_prop <- test_type_proportions |>
#     tidyr::pivot_wider(
#         id_cols = date_confirmation,
#         names_from = state,
#         values_from = prop_RAT,
#         values_fn = mean,
#         values_fill = NA) |>
#     fill_date_gaps() |>
#     tibble::column_to_rownames(var = 'date_confirmation') |>
#     data.frame()
#
#
# # delay_input <- estimate_delays_from_data(
# #     delay_data, jurisdictions)
# # linelist[linelist$test_type == 'PCR',]
#
#
# # subset data that goes in to valid data.
#
