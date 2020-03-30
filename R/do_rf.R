#' Collect all data from store, prepare data, train random forest
#'
#' @seealso
#'   * [rmweather::rmw_do_all()]
#'
#' @param config in list form from [yaml::read_yaml()]
#' @param store containing all the data
#' @param n_trees Priority order is: argument `n_trees` > `config$n_trees` > default `500`
#' @param n_samples Priority order is: argument `n_samples` > `config$n_samples` > default `50`
#' @param prepare_data_only only prepare the data and then return
#'
#' @return list with prepared data or model
#' @export
#'
#' @examples
#' # TODO
do_rf <- function(config, store, n_trees = NULL, n_samples = NULL, prepare_data_only = TRUE) {
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

  res$rf <- rf
  res$statistics <- rmweather::rmw_model_statistics(rf$model)
  res$r2 <- res$statistics$r_squared

  #res$run_started <- lubridate::as_datetime(rf$model$elapsed_times$date_start)
  res
}

