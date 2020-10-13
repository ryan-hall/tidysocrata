create_source <- function(revision_response_object,
                          source_type = "upload", source_parse = "true",
                          domain, email, password) {

  source_json_type <- paste0('{"type":"',source_type,'", "filename":"socrata_upload_temp.csv"}')
  source_json_parse <- paste0('{"parse_source":"',source_parse,'"}')
  source_json <- paste0('{"source_type":',source_json_type,', "parse_options":',
                        source_json_parse,'}')

  create_source_url <- paste0(domain, revision_response_object$links$create_source)

  create_source_response <- httr::POST(create_source_url,
                                       body = source_json,
                                       httr::add_headers("Content-Type" = "application/json"),
                                       httr::authenticate(email, password, type = "basic"),
                                       httr::user_agent(fetch_user_agent())
  )

  if (create_source_response$status_code == "201") {
    message("Created source for update to ", revision_response_object$resource$fourfour)
    create_source_response <- jsonlite::fromJSON(httr::content(create_source_response,
                                                               as = "text",
                                                               type = "application/json",
                                                               encoding = "utf-8"))
  } else {
    message("Failed to create source with status code ",create_source_response$status_code)
    stop_for_status(create_source_response$status_code)
  }
}
