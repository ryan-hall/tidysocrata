upload_to_source <- function(create_source_response,
                             username, password,
                             data_source,
                             timeout = 100, delay = 1) {

  revision_url_parsed <- httr::parse_url(create_source_response$revision_url)

  upload_data_url <- paste0(revision_url_parsed$scheme,
                            '://', revision_url_parsed$hostname,
                            '/', create_source_response$response_body$links$bytes)

  if(!is.data.frame(data_source) & !is.character(data_source)) {
    stop(data_source, " does not appear to be a data.frame or filepath.")
  }

  if (is.data.frame(data_source)) {
    temp_file <- tempfile("socrata_upload_temp.csv")
    data.table::fwrite(data_source, temp_file)
    data_for_upload <- httr::upload_file(temp_file)
  }

  if (is.character(data_source)) {
    data_for_upload <- httr::upload_file(data_source)
  }

  upload_data_response <- httr::POST(upload_data_url,
                                     body = data_for_upload,
                                     httr::add_headers("Content-Type" = "text/csv"),
                                     httr::authenticate(username, password,
                                                        type = "basic"))

  upload_data_response_body <- jsonlite::fromJSON(
    httr::content(upload_data_response,
                  as = "text",
                  type = "application/json",
                  encoding = "utf-8"))

  if (upload_data_response$status_code == "200") {

    message("Uploading data source to revision ",
            create_source_response$revision_id  ," on dataset ",
            create_source_response$asset_id, ".")

  } else {
    message("Failed to upload data to revision ",
            create_source_response$revision_id,
            " on dataset ",
            create_source_response$asset_id,
            " with status code ",
            upload_data_response$status_code, ".")

    stop_for_status(upload_data_response$status_code)
  }

  poll_for_status <- 0
  repeat {

    poll_for_status <- poll_for_status + 1

    if (!is.null(upload_data_response_body$resource$failed_at)) {
      stop("Upload failed. Check upload response body for more detail.")

      return(list(asset_id = create_source_response$asset_id,
                  revision_id = create_source_response$revision_id,
                  revision_url = create_source_response$revision_url,
                  status_code = upload_data_response$status_code,
                  response_body = upload_data_response_body))

    } else if (!is.null(upload_data_response_body$resource$finished_at)){
      message("Upload finished.")

      return(list(asset_id = create_source_response$asset_id,
                  revision_id = create_source_response$revision_id,
                  revision_url = create_source_response$revision_url,
                  status_code = upload_data_response$status_code,
                  response_body = upload_data_response_body))

      break

    } else if (poll_for_status == timeout) {

      return(list(asset_id = create_source_response$asset_id,
                  revision_id = create_source_response$revision_id,
                  revision_url = create_source_response$revision_url,
                  status_code = upload_data_response$status_code,
                  response_body = upload_data_response_body))

      stop("Timeout reached while polling for upload to finish. Review the response_body
           and consider increasing the timeout.")

    } else {
      message("Upload in progress...")
      upload_status_url <- paste0(revision_url_parsed$scheme,
                                '://', revision_url_parsed$hostname,
                                '/', create_source_response$response_body$links$show)

      upload_data_response <- httr::GET(upload_status_url,
                                        httr::authenticate(username, password,
                                                           type = "basic"))
      httr::stop_for_status(upload_data_response)

      upload_data_response <- jsonlite::fromJSON(httr::content(upload_data_response,
                                                               as = "text",
                                                               type = "application/json",
                                                               encoding = "utf-8"))

      Sys.sleep(delay)

    }
  }
}
