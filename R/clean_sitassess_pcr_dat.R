clean_sitassess_pcr_dat <- function (raw_dat, study_seq) {

    notif_dat <- raw_dat |>
        dplyr::filter(test_type == 'PCR',
                      date_confirmation %in% study_seq,
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
    notif_dat
}
