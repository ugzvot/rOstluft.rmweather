#' Parse list of explanatory variables into
#'
#' @param config in list form from [yaml::read_yaml()]
#'
#' @return list with items all, default, observations and expressions
#' @export
#'
#' @examples
#' # TODO proper location for files
#' config <- get_example_config()
#' parse_explanatory_vars(config)
parse_explanatory_vars <- function(config) {
  explanatory_vars <- tibble::tibble(
    variable = purrr::map_chr(config$explanatory_variables, ~purrr::chuck(.x, "variable")),
    site = purrr::map_chr(config$explanatory_variables, ~purrr::pluck(.x, "site", .default = NA_character_)),
    parameter = purrr::map_chr(config$explanatory_variables, ~purrr::pluck(.x, "parameter", .default = NA_character_)),
    exp = purrr::map_chr(config$explanatory_variables, ~purrr::pluck(.x, "exp", .default = NA_character_)),
  )

  # default variables calculated by rmw_prepare_data
  default_vars <- c("date_unix", "day_julian", "weekday", "hour")

  default_explanatory_vars <- dplyr::intersect(default_vars, explanatory_vars$variable)

  other_explanatory_vars <- dplyr::filter(explanatory_vars, !(.data$variable %in% default_vars))

  exp_explanatory_vars <- dplyr::filter(other_explanatory_vars, !is.na(.data$exp))

  obs_explanatory_vars <- dplyr::filter(other_explanatory_vars, is.na(.data$exp)) %>%
    dplyr::select(-"exp")



  obs_explanatory_vars <- dplyr::mutate(obs_explanatory_vars,
    site = dplyr::if_else(is.na(.data$site), config$site, .data$site),
    parameter = dplyr::if_else(is.na(.data$parameter), .data$variable, .data$parameter)
  )

  list(
    all = explanatory_vars,
    default = default_explanatory_vars,
    observations = obs_explanatory_vars,
    expressions = exp_explanatory_vars
  )
}
