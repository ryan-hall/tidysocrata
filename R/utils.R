#' Validates a provided Socrata data set id
#'
#' Socrata data set identifiers are assigned when a data set is created and take
#' the form of two sets of four alphanumeric characters separated by a dash.
#' @param dataset_id A Socrata data set identifier
#'
#' @return Boolean. True if the data set identifier meets the format criteria.
#' @export
#'
#' @examples
validate_dataset_id <- function(dataset_id) {
  if (!is.character(dataset_id)) {
    stop("The provided data set id is not a character vector.")
  }

  id <- casefold(dataset_id)

  valid <- grepl("^[a-z0-9]{4}-[a-z0-9]{4}$", id)

  return(valid)
}
