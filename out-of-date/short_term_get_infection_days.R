#' Title
#'
#' @param jurisdictions
#' @param case_dat
#' @param case_delay_dat
#' @param hosp_delay_dat
#'
#' @return
#' @export
#'
#' @examples
short_term_get_infection_days <- function (jurisdictions,
                                           case_dat,
                                           case_delay_dat) {

    dates <- unique(case_dat$date)

    case_delay_mat <- prepare_delay_input(
        dates, jurisdictions,
        constant_delay = list(case_delay_dat))
    case_inf_days <- calculate_days_infection(case_delay_mat)

    case_inf_days
}
#' Title
#'
#' @param delay_matrix
#'
#' @return
#' @export
#'
#' @examples
calculate_days_infection <- function (delay_matrix) {

    # extra_left is defined by the max notification delay at the start
    extra_left <- max(which(delay_matrix[[1]](0:28) != 0))

    # extra_right defined by the max notification delay at the end
    extra_right <- max(which(delay_matrix[[nrow(delay_matrix)]](0:28) != 0))

    earliest_date <- as.Date(min(rownames(delay_matrix)))

    # outputs vector of probabilities of infection being delayed between 0 and 28 days
    latest_date <- as.Date(max(rownames(delay_matrix)))

    infection_seq <- seq(
        from = earliest_date - extra_left,
        to = latest_date + extra_right,
        by = 'day')

    return(infection_seq)
}

