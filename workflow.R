## header
# library(lowerGPreff)
# library(GPreff)

linelist_file <- "../sitassess-data/processed_linelist_20231102.RDS"
linelist_all_dates <- readRDS(linelist_file)

study_seq <- seq(from = as.Date('2021-06-01'), to = as.Date('2021-12-31'), 'days')

notif_dat <- linelist_all_dates |>
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

# jurisdictions
jurisdictions <- unique(notif_dat$jurisdiction)
n_jurisdictions <- length(jurisdictions)

## delay data
notif_delay_file <- 'data/ECDF_delay_constant_PCR.rds'
notif_delay_ecdf <- readRDS(notif_delay_file)

## days of infection timeseries
infection_days <- seq(from = as.Date('2021-05-03'),
                      to = as.Date('2022-01-29'), 'days')

n_days_infection <- length(infection_days)

## proportions
car <- GPreff::expand_constant_value(
    dates = infection_days,
    jurisdictions = jurisdictions,
    constant_val = 0.75,
    col_name = 'prop')

## incubation period
incubation_period_distribution <- lowerGPreff::make_cdf(
    option = "Delta")

## formatted delay distribution
notif_delay_dist <- GPreff::expand_constant_value(
    dates = infection_days,
    jurisdictions = jurisdictions,
    constant_val = list(notif_delay_ecdf),
    col_name = 'delay_fxn')
notif_full_delay_dist <- lowerGPreff::extend_delay_data(
    notif_delay_dist,
    incubation_period_distribution)

## generation interval distribution
gi_distribution_data <- readr::read_csv(
    file = "data/nishiura_samples.csv",
    col_types = readr::cols(param1 = readr::col_double(),
                            param2 = readr::col_double()))

generation_interval_distribution <- lowerGPreff::make_generation_interval_density(
    gi_distribution_data)

## optional day-of-week effect model
dow_model <- lowerGPreff::create_dow_priors(
    n_jurisdictions)

## infection timeseries model
infection_model_objects <- lowerGPreff::create_infection_timeseries(
    n_days_infection,
    n_jurisdictions,
    effect_type = 'growth_rate')

## observation models
notif_observation_model_objects <- lowerGPreff::create_observation_model(
    infection_timeseries = infection_model_objects$infection_timeseries,
    delay_distribution = notif_full_delay_dist,
    proportion_observed = car,
    count_data = notif_dat,
    dow_model = dow_model,
    data_id = 'notif')

## Reff
reff_model_objects <- lowerGPreff::estimate_reff(
    infection_timeseries = infection_model_objects$infection_timeseries,
    generation_interval_mass_fxns = generation_interval_distribution)

## greta model fit
m <- greta::model(infection_model_objects$infection_timeseries,
                  reff_model_objects$reff)

plot(m)

fit <- GPreff::fit_model(
    model = m,
    n_chains = 4,
    max_convergence_tries = 1,
    warmup = 1000,
    n_samples = 1000,
    n_extra_samples = 1000)

# long is classic output, then map to infection traj.
infections_out <- GPreff::generate_long_estimates(
    infection_model_objects$infection_timeseries,
    fit,
    infection_days,
    jurisdictions)

reff_out <- GPreff::generate_long_estimates(
    reff_model_objects$reff,
    fit,
    infection_days,
    jurisdictions)

infection_traj <- GPreff::build_trajectories(
    param = infection_model_objects$infection_timeseries,
    infection_days,
    fit,
    nsim = 1000,
    jurisdictions)

GPreff::plot_reff_interval_curves(
    '../sitassess-data/outputs/reff2.png',
    reff_out,
    dates = infection_days,
    start_date = min(study_seq),
    end_date = max(study_seq),
    jurisdictions = jurisdictions)

infection_sims <- greta::calculate(infection_model_objects$infection_timeseries,
                                   values = fit,
                                   nsim = 1000)

GPreff::plot_timeseries_sims(
    '../sitassess-data/outputs/infection_timeseries2.png',
    infection_sims[[1]],
    type = "infection",
    dates = infection_days,
    start_date = study_seq[1],
    end_date = study_seq[length(study_seq)],
    states = jurisdictions,
    dim_sim = "2",
    infection_nowcast = FALSE)

