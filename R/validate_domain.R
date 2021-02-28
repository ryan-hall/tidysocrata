#' Validates the format of a provided Socrata domain
#'
#' @param domain A Socrata hostname.
#'
#' @return A Socrata hostname.
#' @export
#'
validate_domain <- function(domain) {

  domain <- casefold(domain)
  domain_parse <- httr::parse_url(domain)

  if(!is.null(domain_parse$hostname)) {
    domain_valid <- domain_parse$hostname
  } else if(is.null(domain_parse$scheme) & is.null(domain_parse$host_name) &
            !is.null(domain_parse$path)) {
    domain_parse_edit <- httr::parse_url(paste0("https://", domain_parse$path))
    domain_valid <- domain_parse_edit$hostname
  } else if(is.null(domain_parse$scheme) & is.null(domain_parse$host_name) &
            is.null(domain_parse$path)) {
    stop(domain, " does not appear to be a valid domain name")
  }

  return(domain_valid)
}
