#' Collect all data from store, prepare data, train random forest
#'
#' @seealso [rmweather::rmw_do_all()]
#'
#' @param config in list form from [yaml::read_yaml()]
#' @param store containing all the data
#' @param description of the model. Will be saved in statistics.yml
#' @param n_trees Priority order is: argument `n_trees` > `config$n_trees` > default `500`
#' @param n_samples Priority order is: argument `n_samples` > `config$n_samples` > default `50`
#' @param prepare_data_only only prepare the data and then return
#' @param output_dir if not NULL all data will be saved under `output_dir\config$site\xxhash64` (xxhash64 is calculated
#'   from the return value from [rmweather::rmw_do_all()] with [digest::digest()])
#'
#' @return list with prepared data or model
#' @export
#'
#' @examples
#' # TODO
do_rf <- function(
  config,
  store,
  description,
  n_trees = NULL,
  n_samples = NULL,
  prepare_data_only = TRUE,
  output_dir = NULL
) {

  res <- list()

  n_trees <- purrr::detect(list(n_trees, config$n_trees, 500), rlang::is_integerish, finite = TRUE)
  n_samples <- purrr::detect(list(n_samples, config$n_samples, 50), rlang::is_integerish, finite = TRUE)


  explanatory_vars <- parse_explanatory_vars(config)

  input <- prepare_observations_rf(config, store)
  input$observations <-  rmweather::rmw_prepare_data(input$observations)

  if (isTRUE(prepare_data_only)) {
    return(input)
  }


  # in the future call rmw_train_model and make the normalise call after
  # and configurable?
  rf <- rmweather::rmw_do_all(
      df = input$observations,
      variables = explanatory_vars$all$variable,
      n_trees = n_trees,  # todo move into config, call
      n_samples = n_samples, # todo move into config, call
      verbose = TRUE  # todo move into config, call
  )

  statistics <- rmweather::rmw_model_statistics(rf$model)

  res$rf <- rf
  res$r2 <- statistics$r_squared
  res$xxhash64 <- digest::digest(rf, algo = "xxhash64")  # ensure a unique id
  res$run_started <- lubridate::as_datetime(rf$elapsed_times$date_start)

  statistics <- dplyr::mutate(statistics,
    description = description,
    site = config$site,
    parameter = config$parameter,
    run_started = format.POSIXct(res$run_started, "%Y-%m-%dT%H:%M:%SZ", tz = "GMT"),
    xxhash64 = res$xxhash64,
    n_samples = n_samples,
  )

  statistics <- dplyr::select(statistics, "site", "description", "r_squared", "parameter", "run_started", "xxhash64", "n_trees",
                              "n_samples", dplyr::everything())

  res$statistics <- statistics

  # make function for this
  res$timeseries <- tibble::tibble(
    date = rf$normalised$date,
    site = config$site,
    parameter = config$parameter,
    xxhash64 = res$xxhash64,
    value_observed = rf$observations$value,
    value_normalised = rf$normalised$value_predict,
    value_predicted = rmweather::rmw_predict(rf$model, rf$observations, verbose = FALSE)
  )



  if (!is.null(output_dir)) {
    output_dir <- fs::path(output_dir, config$site, res$xxhash64)
    fs::dir_create(output_dir)

    yaml::write_yaml(config, fs::path(output_dir, "config.yml"))
    yaml::write_yaml(res$statistics, fs::path(output_dir, "statistics.yml"))


    # build save logic
    saveRDS(res, fs::path(output_dir, "model.rds"))
    saveRDS(res$timeseries, fs::path(output_dir, "timeseries.rds"))
    readr::write_tsv(input$n_observations, fs::path(output_dir, "n_observations.tsv"))
    importance <- rmweather::rmw_model_importance(rf$model)
    plt <- rmweather::rmw_plot_importance(importance)
    ggplot2::ggsave(fs::path(output_dir, "importance.png"), width = 600/300, height = 600/300, scale = 3)
  }
  res
}


#' Runs a list of configs
#'
#' @seealso [rmweather::rmw_do_all()], [do_rf()]
#'
#' @param list_of_configs a list of configs to run
#' @param store a local store containing all the data needed for the run
#' @param description of the models. Will be saved in statistics.yml and a run file with the stats from all models
#'   with sanitized desription as file name is created in `output_dir`.
#' @inheritDotParams do_rf
#'
#' @return list of results from do_rf
#' @export
do_rf_list <- function(list_of_configs, store, description, ...) {
  dots <- rlang::dots_list(...)

  # run all models
  results <- purrr::map(list_of_configs, do_rf, store = store, description = description, ...)


  if (!is.null(dots$output_dir)) {
    stats_fn <- fs::path(dots$output_dir, fs::path_sanitize(description, replacement = "_"), ext = "csv")
    stats <- purrr::map(results, purrr::chuck, "statistics")
    stats <- dplyr::bind_rows(stats)
    readr::write_excel_csv(stats, stats_fn, delim = ";")
  }

  results
}


