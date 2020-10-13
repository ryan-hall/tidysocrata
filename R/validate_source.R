# Validation step to ensure a named source is either a valid filepath or dataframe
validate_source <- function(source_for_socrata) {

  if(!is.data.frame(source_for_socrata) & !is.character(source_for_socrata)) {
    return(FALSE)
  }

  if (is.data.frame(source_for_socrata)) {
    return(TRUE)
  }

  if (is.character(source_for_socrata)) {
    if(grepl("\\.csv$", source_for_socrata)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
