## Validates and handles a domain argument
validate_domain <- function(domain) {
  domain <- casefold(domain)
  domain_parse <- httr::parse_url(domain)

  if(!is.null(domain_parse$hostname)) {
    domain_valid <- domain_parse$hostname
  } else if(is.null(domain_parse$scheme) & is.null(domain_parse$host_name) &
            !is.null(domain_parse$path)) {
    domain_valid <- domain_parse$path
  } else if(is.null(domain_parse$scheme) & is.null(domain_parse$host_name) &
            is.null(domain_parse$path)) {
    stop(domain, " does not appear to be a valid domain name")
  }

  remove_path <- httr::parse_url(paste0("https://", domain_valid))
  domain_valid <- remove_path$hostname

  return(domain_valid)
}
