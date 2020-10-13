apply_revision <- function(revision_response_object, domain, email, password) {

  apply_revision_url <- paste0(domain, revision_response_object$links$apply)
  apply_revision_json <- paste0('{"resource": {"id":',
                                revision_response_object$resource$revision_seq,
                                '}}')

  apply_revision_response <- httr::PUT(apply_revision_url,
                                       body = apply_revision_json,
                                       httr::add_headers("Content-Type" = "application/json"),
                                       httr::authenticate(email, password, type = "basic"),
                                       httr::user_agent(fetch_user_agent())
  )

  if (apply_revision_response$status_code == "200") {
    message("Revision applied. Socrata is processing the update.")
  } else {
    message("Revision failed to apply. Check the apply_revision_response for details.")
    apply_revision_response <- jsonlite::fromJSON(httr::content(apply_revision_response,as = "text",type = "application/json", encoding = "utf-8"))
    return(apply_revision_response)
  }
}
