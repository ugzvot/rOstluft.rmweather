% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{parse_explanatory_vars}
\alias{parse_explanatory_vars}
\title{Parse list of explanatory variables into}
\usage{
parse_explanatory_vars(config)
}
\arguments{
\item{config}{in list form from \code{\link[yaml:read_yaml]{yaml::read_yaml()}}}
}
\value{
list with items all, default, observations and expressions
}
\description{
Parse list of explanatory variables into
}
\examples{
# TODO proper location for files
config <- get_example_config()
parse_explanatory_vars(config)
}
