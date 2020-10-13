## Open a revision on an existing dataset
open_revision <- function(dataset_id, action_type, domain, email, password) {
  ## Validate inputs. If anything doesn't look right, don't open a revision.
  dataset_id <- casefold(as.character(dataset_id))

  if(!isFourByFour(dataset_id))
    stop(dataset_id, " does not appear to be of valid Socrata dataset identifier format.")

  action_type <- casefold(action_type)
  if(!any(c(action_type == "update", action_type == "replace", action_type == "delete")))
    stop(action_type, " is not one of 'update', 'replace', or 'delete'.")

  domain <- validate_domain(domain)

  open_revision_endpoint <- paste0('https://', domain, '/api/publishing/v1/revision/', dataset_id)


  open_revision_json <- paste0('{"action": {"type":"', action_type,'"}}')

  open_revision_response <- POST(open_revision_endpoint,
                                 body = open_revision_json,
                                 httr::add_headers("Content-Type" = "application/json"),
                                 httr::authenticate(email, password, type = "basic"),
                                 httr::user_agent(fetch_user_agent())
  )

  if(open_revision_response$status_code == '201') {
    message(dataset_id, " - Opened new revision on dataset ", dataset_id)
    open_revision_response_body <- jsonlite::fromJSON(httr::content(open_revision_response, as = "text", type = "application/json", encoding = "utf-8"))
  } else {
    message(dataset_id, " - Failed to open revision with status code ", open_revision_response$status_code)
    stop_for_status(open_revision_response$status_code)
  }
}
