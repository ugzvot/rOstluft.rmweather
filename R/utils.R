#' Reads example yaml from disk
#'
#' @return output from[yaml::read_yaml()]
#' @export
#'
#' @examples
#' # config file content
#' fn <- system.file("extdata", "rf_config.yml", package = "rOstluft.rmweather", mustWork = TRUE)
#' cat(readr::read_file(fn))
#'
#'
#' # get the parsed yaml file
#' get_example_config()

get_example_config <- function() {
  yaml::read_yaml(system.file("extdata", "rf_config.yml", package = "rOstluft.rmweather", mustWork = TRUE))
}
