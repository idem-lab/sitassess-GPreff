sitassess_car_from_surveys <- function(dates,
                                       jurisdictions,
                                       latest_survey_data,
                                       test_type = c("PCR", "RAT")) {

    CAR_time_point <- latest_survey_data |>
        readr::read_csv() |>
        dplyr::select(state, date, fitted_point4) |>
        dplyr::mutate(test_prob_given_symptom = fitted_point4 * 0.01) #changed to a proportion at this point because rat reporting is a proportion

    # hack in 0.75 and 0.00 for Delta period for PCR and RAT
    if (test_type == 'RAT') {

        rat_reporting <- "outputs/report_positive_rat_state_aggregate4weeks_2023-09-28.csv"
        RAT_report_time_point <- rat_reporting |>
            readr::read_csv() |>
            dplyr::select(state, date, proportion)

        RAT_CAR_smooth <- dplyr::left_join(
            CAR_time_point,
            RAT_report_time_point)

        CAR_time_estimate <-  RAT_CAR_smooth |>
            dplyr::mutate(test_prob_given_infection =
                              zoo::na.locf(rat_reporting,
                                           na.rm = FALSE,
                                           fromLast = FALSE)) |>
            dplyr::mutate(test_prob_given_infection =
                              test_prob_given_symptom * proportion * 0.75)

        pre_delta_ascertainment_date <- lubridate::as_date('2021_11_01')
        pre_delta_ascertainment_rate <- 0.00

    }

    if (test_type == 'PCR') {

        CAR_time_estimate <- CAR_time_point |>
            dplyr::mutate(test_prob_given_infection =
                              test_prob_given_symptom * 0.75)

        pre_delta_ascertainment_date <- lubridate::as_date('2021_12_07')
        pre_delta_ascertainment_rate <- 0.75

    }

    # join in the estimates
    # remove conditional on symptom column since it's linearly scaled from infection
    # add quick drop off in Dec 2021
    CAR_smooth <- tibble::tibble(
        date = rep(dates, length(jurisdictions)),
        state = rep(jurisdictions, each = length(dates))) |>
        dplyr::left_join(CAR_time_estimate) |>
        dplyr::select(-test_prob_given_symptom, -fitted_point4) |>
        dplyr::mutate(test_prob_given_infection =
                          dplyr::case_when(
                              date <= pre_delta_ascertainment_date ~ pre_delta_ascertainment_rate,
                              TRUE ~ test_prob_given_infection)) |>
        dplyr::mutate(test_prob_given_infection =
                          dplyr::case_when(
                              date == as.Date('2021-12-14') ~ 0.33,
                              TRUE ~ test_prob_given_infection))

    if (test_type == "RAT") {
        CAR_smooth <- CAR_smooth |>
            dplyr::mutate(test_prob_given_infection =
                              dplyr::case_when(
                                  state == "VIC" & date > as.Date('2023-06-30') ~ 0.00,
                                  state == "QLD" & date > as.Date('2023-08-31') ~ 0.00,
                                  state == "NSW" & date > as.Date('2023-09-30') ~ 0.00,
                                  state == "WA" & date > as.Date("2023-10-08") ~ 0.00,
                                  TRUE ~ test_prob_given_infection))
    }

    # interpolate NAs and repeat forward last survey result
    CAR_smooth <- CAR_smooth |>
        dplyr::group_by(state) |>
        dplyr::mutate(test_prob_given_infection =
                          zoo::na.approx(test_prob_given_infection,
                                         na.rm = FALSE),
                      test_prob_given_infection =
                          zoo::na.locf(test_prob_given_infection,
                                       na.rm = FALSE,
                                       fromLast = FALSE)) |>
        dplyr::rename(CAR = test_prob_given_infection)

    # get matrix form
    CAR_smooth_mat <- CAR_smooth |>
        dplyr::select(date, state, CAR) |>
        tidyr::pivot_wider(values_from = CAR,
                           names_from = state) |>
        dplyr::select(-date) |>
        as.matrix()

    CAR_smooth_mat

}
