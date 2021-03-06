% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{validate_dataset_id}
\alias{validate_dataset_id}
\title{Validates a provided Socrata data set id}
\usage{
validate_dataset_id(dataset_id)
}
\arguments{
\item{dataset_id}{A Socrata data set identifier}
}
\value{
Boolean. True if the data set identifier meets the format criteria.
False if the provided id does not match the expected format.
}
\description{
Socrata data set identifiers are assigned when a data set is created and take
the form of two sets of four alphanumeric characters separated by a dash.
This function tests the format of the identifier, but not whether or not that
identifier actually exists on Socrata.
}
