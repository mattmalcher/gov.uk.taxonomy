% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_api_funs.r
\name{get_gov_orgs}
\alias{get_gov_orgs}
\title{Get Government Organisations}
\usage{
get_gov_orgs(limit = 10000)
}
\arguments{
\item{limit}{limit on number of organisations to fetch - default 10k will get all orgs, which as of Jan 2020 number 1099.}
}
\value{

}
\description{
A function to get government organisations as a nice tibble
}
\examples{

get_gov_orgs()
}
