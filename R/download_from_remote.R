#' Download the necessary data from a remote store in a local
#'
#' @param local local rOstluft store. Probably a [rOstluft::storage_local_rds()]
#' @param remote remote rOstluft store. Probably a [rOstluft::storage_s3_rds()]
#' @param config config in list form from [yaml::read_yaml()]
#'
#' @return
#' @export
#'
#' @examples
#' config <- get_example_config()
#' remote <- rOstluft::store_aqmet()
#' local <- rOstluft::storage_local_rds(
#'   "rf_example",
#'   format = rOstluft::format_rolf(),
#'   read.only = FALSE
#' )
#'
#' download_from_remote(local, remote, config)
#'
#' # tidy up
#' local$destroy("DELETE")
download_from_remote <- function(local, remote, config) {
 explanatory_vars <- parse_explanatory_vars(config)

  # append depent variable als value
  obs_explanatory_vars <- dplyr::bind_rows(
    explanatory_vars$observations,
    list(variable = "value", site = config$site, parameter = config$parameter)
  )

  sites <- unique(obs_explanatory_vars$site)

  observations <- remote$get(site = sites, interval = config$interval, year = config$years)
  local$put(observations)
}
