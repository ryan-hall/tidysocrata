apply_revision <- function(upload_to_source_response,
                           username, password) {

  apply_revision_url <- paste0(upload_to_source_response$revision_url, '/apply')

  apply_revision_json <- jsonlite::toJSON(
    list(
      resource = list(
        id = upload_to_source_response$revision_id)
    ))

  apply_revision_response <- httr::PUT(apply_revision_url,
                                       body = apply_revision_json,
                                       httr::add_headers("Content-Type" = "application/json"),
                                       httr::authenticate(username, password, type = "basic"))

  apply_revision_response_body <- jsonlite::fromJSON(
    httr::content(apply_revision_response,
                  as = "text",
                  type = "application/json",
                  encoding = "utf-8"))

  if (apply_revision_response$status_code == "200") {
    message("Revision ", upload_to_source_response$revision_id,
            " applied on dataset ", upload_to_source_response$asset_id,
            ". Socrata is processing the update.")

    return(list(asset_id = upload_to_source_response$asset_id,
                revision_id = upload_to_source_response$revision_id,
                revision_url = upload_to_source_response$revision_url,
                status_code = apply_revision_response$status_code,
                response_body = apply_revision_response_body))

  } else {
    message("Revision ", upload_to_source_response$revision_id,
            " failed to apply on dataset ", upload_to_source_response$asset_id,
            ". Check the response_body for details.")

    return(list(asset_id = upload_to_source_response$asset_id,
                revision_id = upload_to_source_response$revision_id,
                revision_url = upload_to_source_response$revision_url,
                status_code = apply_revision_response$status_code,
                response_body = apply_revision_response_body))
  }
}
