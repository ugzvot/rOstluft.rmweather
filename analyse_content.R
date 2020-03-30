# just some helpers to find avaible time series

library(rOstluft)
library(magrittr)

store <- rOstluft::store_aqmet()

content <- store$get_content()

years <- 2015:2020

# content <- aqmet_public$get_content()


# find the right place to save this function
filter_content <- function(
  content,
  intervals,
  parameters = NULL,
  sites = NULL,
  years = NULL,
  pivot = "year"
) {
  if (length(pivot) > 1) stop("can only pivot over one variable")


  if (!is.null(parameters)) {
    content <- rOstluft::pluck_parameter(content, !!!parameters)
  }

  if (!is.null(sites)) {
    content <- rOstluft::pluck_site(content, !!!sites)
  }

  if (!is.null(intervals)) {
    content <- rOstluft::pluck_interval(content, !!!intervals)
  }

  if (!is.null(years)) {
    content <- dplyr::filter(content, .data$year %in% years)
  }

  content <- dplyr::arrange(content, !!rlang::sym(pivot))

  id_cols <- dplyr::setdiff(c("site", "parameter", "interval", "year"), pivot)

  content <- tidyr::pivot_wider(
    content,
    id_cols = tidyselect::all_of(id_cols),
    names_from = "year",
    values_from = c("n")
  )
}

wind <- filter_content(
  content,
  interval = "h1",
  parameters = "WVs",
  sites = NULL,
  years = years,
  pivot = "year"
) %>% tidyr::drop_na() %>% dplyr::mutate_if(is.factor, as.character)


# for some reasons
purrr::map(wind$site, ~filter_content(
    content = content,
    interval = "h1",
    sites = .x,
    parameters = NULL,
    years = years,
    pivot = "year"
))


