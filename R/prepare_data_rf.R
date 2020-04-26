#' Get data from store and rearange to wide format
#'
#' @param config in list form from [yaml::read_yaml()]
#' @param store containing all the data
#'
#' @return list with items observations and n_observations as summary
#' @export
#'
#' @examples
#' config <- get_example_config()
#' store <- rOstluft::store_aqmet()
#' prepare_observations_rf(config, store)
prepare_observations_rf <- function(config, store) {

  explanatory_vars <- parse_explanatory_vars(config)

  # append depent variable als value
  obs_explanatory_vars <- dplyr::bind_rows(
    explanatory_vars$observations,
    list(variable = "value", site = config$site, parameter = config$parameter)
  )

  sites <- unique(obs_explanatory_vars$site)

  observations <- store$get(site = sites, interval = config$interval, year = config$years)

  create_filter_quo <- function(site, parameter) {
    rlang::quo(( .data$site == {{site}} & .data$parameter == {{parameter}} ))
  }

  reduce_filter_or <- function(acc, nxt) {
    rlang::quo(!!acc | !!nxt)
  }

  filter_list_quo <- purrr::map2(obs_explanatory_vars$site, obs_explanatory_vars$parameter, create_filter_quo)
  filter_arg <- purrr::reduce(filter_list_quo, reduce_filter_or)

  observations <- dplyr::filter(observations, !!filter_arg)

  # check for number of observations per year
  n_observations <- dplyr::count(observations, .data$site, .data$parameter, year = lubridate::year(.data$starttime)) %>%
    dplyr::mutate_if(is.factor, as.character)

  df <- tidyr::expand_grid(obs_explanatory_vars, year = config$years)
  n_observations <- dplyr::left_join(df, n_observations, by=c("site", "parameter", "year")) %>%
    dplyr::arrange(.data$variable, .data$year)

  # rearange observations for rmw
  observations <- dplyr::mutate_if(observations, is.factor, as.character)
  observations <- dplyr::left_join(observations, obs_explanatory_vars, by = c("site", "parameter"))
  observations <- dplyr::select(observations, date = "starttime", "variable", "value")
  observations <- tidyr::spread(observations, "variable", "value")

  list(
    observations = observations,
    n_observations = n_observations
  )
}
