
# Load targets associated packages required to define the pipeline
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(

    # packages that your targets need to run
    packages = c('lowerGPreff', 'GPreff', 'greta',
                 'dplyr', 'readr', 'tidyselect'),

    workspace_on_error = TRUE,

    # default storage format
    format = "rds"
)

# Run the R scripts in the R/ folder with custom functions:
tar_source()

# main workflow
targets <- list(

    tar_target(linelist_file,
               '../sitassess-data/processed_linelist_20231102.RDS',
               format = 'file'),

    tar_target(linelist_all_dates,
               readRDS(linelist_file)),

    tar_target(study_seq,
               seq(from = as.Date('2021-06-01'),
                   to = as.Date('2021-12-31'), 'days')),

    tar_target(notif_dat,
               clean_sitassess_pcr_dat(linelist_all_dates,
                                       study_seq)),

    # jurisdictions
    tar_target(jurisdictions, unique(notif_dat$jurisdiction)),
    tar_target(n_jurisdictions, length(jurisdictions)),

    ## delay data
    tar_target(notif_delay_file,
               'data/ECDF_delay_constant_PCR.rds',
               format = 'file'),
    tar_target(notif_delay_ecdf,
               readRDS(notif_delay_file)),

    ## days of infection timeseries
    tar_target(infection_days,
               seq(from = as.Date('2021-05-03'),
                   to = as.Date('2022-01-28'), 'days')),

    tar_target(n_days_infection,
               length(infection_days)),

    ## proportions
    tar_target(car,
               expand_constant_value(
                   dates = infection_days,
                   jurisdictions = jurisdictions,
                   constant_val = 0.75,
                   col_name = 'prop')),

    ## incubation period
    tar_target(incubation_period_distribution,
               make_cdf(
                   option = "Delta")),

    ## formatted delay distribution
    tar_target(notif_delay_dist,
               expand_constant_value(
                   dates = infection_days,
                   jurisdictions = jurisdictions,
                   constant_val = list(notif_delay_ecdf),
                   col_name = 'delay_fxn')),
    tar_target(notif_full_delay_dist,
               extend_delay_data(
                   notif_delay_dist,
                   incubation_period_distribution)),

    ## generation interval distribution
    tar_target(gi_distribution_data,
               read_csv(
                   file = "data/nishiura_samples.csv",
                   col_types = cols(param1 = col_double(),
                                    param2 = col_double()))),

    tar_target(generation_interval_distribution,
               make_generation_interval_density(
                   gi_distribution_data)),

    ## optional day-of-week effect model
    tar_target(dow_model,
               create_dow_priors(
                   n_jurisdictions)),

    ## infection timeseries model
    tar_target(infection_model_objects,
               create_infection_timeseries(
                   n_days_infection,
                   n_jurisdictions,
                   effect_type = 'growth_rate')),

    ## observation models
    tar_target(notif_observation_model_objects,
               create_observation_model(
                   infection_model_objects$infection_timeseries,
                   delay_distribution = notif_full_delay_dist,
                   proportion_observed = car,
                   count_data = notif_dat,
                   dow_model = dow_model,
                   data_id = 'notif')),

    ## Reff
    tar_target(reff_model_objects,
               estimate_reff(
                   infection_model_objects$infection_timeseries,
                   generation_interval_distribution)),

    ## greta model fit
    tar_target(infection_model,
               model(infection_model_objects$infection_timeseries,
                     reff_model_objects$reff)),

    tar_target(fit,
               fit_model(
                   model = infection_model,
                   n_chains = 4,
                   max_convergence_tries = 1,
                   warmup = 1000,
                   n_samples = 1000,
                   n_extra_samples = 10)),

    ## full estimates long table outputs
    tar_target(infections_out,
               generate_long_estimates(
                   infection_model_objects$infection_timeseries,
                   fit,
                   infection_days,
                   jurisdictions)),

    tar_target(reff_out,
               generate_long_estimates(
                   reff_model_objects$reff,
                   fit,
                   infection_days,
                   jurisdictions)),

    ## reff interval curves plot
    tar_target(reff_plot,
               plot_reff_interval_curves(
                   '../sitassess-data/outputs/reff_interval_plot.pdf',
                   reff_out,
                   dates = infection_days,
                   start_date = '2021-11-01',
                   end_date = max(study_seq),
                   jurisdictions = jurisdictions)),

    ## trajectories table for integration with decision support engine
    tar_target(infection_traj,
               build_trajectories(
                   param = infection_model_objects$infection_timeseries,
                   infection_days,
                   fit,
                   nsim = 1000,
                   jurisdictions)),

    ## infection timeseries plot
    tar_target(infection_sims,
               calculate(infection_model_objects$infection_timeseries,
                         values = fit,
                         nsim = 1000)),
    tar_target(infection_timeseries_plot_Delta,
               plot_timeseries_sims('../sitassess-data/outputs/infection_timeseries_Delta.pdf',
                                    infection_sims[[1]],
                                    type = "infection",
                                    dates = infection_days,
                                    start_date = study_seq[1],
                                    end_date = as.Date('2021-11-30'),
                                    states = jurisdictions,
                                    dim_sim = "2",
                                    infection_nowcast = FALSE)),
    tar_target(infection_timeseries_plot_Omicron,
               plot_timeseries_sims('../sitassess-data/outputs/infection_timeseries_Omicron.pdf',
                                    infection_sims[[1]],
                                    type = "infection",
                                    dates = infection_days,
                                    start_date = as.Date('2021-12-01'),
                                    end_date = study_seq[length(study_seq)],
                                    states = jurisdictions,
                                    dim_sim = "2",
                                    infection_nowcast = FALSE))
)

## specify need to re-initialise the greta model internals into the environment,
## before running a suite of downstream targets
tar_hook_before(
    targets = targets,
    hook = awaken_greta_model(infection_model),
    names = any_of(c('fit', 'infections_out', 'reff_out',
                     'infection_traj', 'infection_sims'))
)
