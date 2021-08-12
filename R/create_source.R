#' Define a source for a Socrata dataset revision
#'
#' @description Defines a source for a Socrata dataset revision, including that the new data
#' will be of `source_type` "upload" and whether the new data is parseable or not by Socrata
#' with `source_parse`. Defining a source is a prerequite to actually sending new data to the
#' revision in `upload_to_source`.
#'
#' @param revision_response The named list returned by `open_revision`.
#' @param username A Socrata username or API Key ID
#' @param password A Socrata password or API Key Secret
#' @param source_type a character string indicating the type of source: 'upload' for files uploaded
#' to Socrata is the only supported option at this time.
#' @param source_parse logical, indicating if the source is a parseable filetype.
#'
#' Filetypes parseable by Socrata include Comma-separated values (CSV), Tab-separated values (TSV),
#' Microsoft Excel (XLS), Microsoft Excel (OpenXML), ZIP archive (shapefile), JSON format (GeoJSON),
#' GeoJSON format, Keyhole Markup Language (KML), Zipped Keyhole Markup Language (KMZ)
#'
#' @return A named list with the asset id, the revision sequence id,
#' the url of the revision, the status code returned while
#' creating the source, and the full response body.
#' @export
#'
#' @examples
create_source <- function(revision_response,
                          username, password,
                          source_type = "upload", source_parse = TRUE) {

  source_json <- jsonlite::toJSON(
    list(
      source_type = list(
        type = jsonlite::unbox(source_type),
        filename = jsonlite::unbox("socrata_upload_temp.csv")
      ),
      parse_options = list(
        parse_source = jsonlite::unbox(source_parse)
      )
    ),
    pretty = T)

  create_source_url <- paste0(revision_response$revision_url, "/source")

  create_source_response <- httr::POST(create_source_url,
                                       body = source_json,
                                       httr::add_headers("Content-Type" = "application/json"),
                                       httr::authenticate(username, password, type = "basic"))

  create_source_response_body <- jsonlite::fromJSON(
    httr::content(create_source_response, as = "text",
                  type = "application/json",
                  encoding = "utf-8")
  )

  if (create_source_response$status_code == "201") {

    message("Created source for revision ",
              revision_response$revision_id  ," on dataset ",
              revision_response$asset_id)

    return(list(asset_id = revision_response$asset_id,
                revision_id = revision_response$revision_id,
                revision_url = revision_response$revision_url,
                status_code = create_source_response$status_code,
                response_body = create_source_response_body))

  } else {

    message("Failed to create source for revision ",
            revision_response$revision_id  ," on dataset ",
            revision_response$asset_id, " with status code ",
            create_source_response$status_code)

    return(list(asset_id = revision_response$asset_id,
                revision_id = revision_response$revision_id,
                revision_url = revision_response$revision_url,
                status_code = create_source_response$status_code,
                response_body = create_source_response_body))

    stop_for_status(create_source_response$status_code)
  }
}
