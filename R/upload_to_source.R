upload_to_source <- function(create_source_response_object, filepath_to_data,
                             domain, email, password, status_checks = 100) {

  upload_data_url <- paste0('https://', domain, create_source_response_object$links$bytes)

  if(!is.data.frame(filepath_to_data) & !is.character(filepath_to_data)) {
    stop(filepath_to_data, " does not appear to be a data.frame or filepath")
  }

  if (is.data.frame(filepath_to_data)) {
    temp_file <- tempfile("socrata_upload_temp.csv")
    data.table::fwrite(filepath_to_data, temp_file)
    data_for_upload <- httr::upload_file(temp_file)
  }

  if (is.character(filepath_to_data)) {
    data_for_upload <- httr::upload_file(filepath_to_data)
  }

  ## the body of the post is the new data
  upload_data_response <- httr::POST(upload_data_url,
                                     body = data_for_upload,
                                     httr::add_headers("Content-Type" = "text/csv"),
                                     httr::authenticate(email, password,
                                                        type = "basic"),
                                     httr::user_agent(fetch_user_agent())
  )

  if (upload_data_response$status_code == "200") {
    message("Uploading data to draft.")
    upload_data_response <- jsonlite::fromJSON(httr::content(upload_data_response,as = "text",
                                                             type = "application/json",
                                                             encoding = "utf-8"))
  } else {
    message("Failed to upload data to source with status code ",
            upload_data_response$status_code)
    stop_for_status(upload_data_response$status_code)
  }

  poll_for_status <- 0
  repeat {

    poll_for_status <- poll_for_status + 1

    if (!is.null(upload_data_response$resource$failed_at)) {
      upload_data_response
      stop("Upload failed. Check upload response.")
    } else if (!is.null(upload_data_response$resource$finished_at)){
      message("Upload finished.")
      break
    } else if (poll_for_status == status_checks) {
      stop("Polling for upload status verification has timed out. Check upload
           response and/or increase poll limit.")
    } else {
      message("Polling for upload and data validation status. Stay tuned.")
      upload_data_response <- httr::GET(paste0(domain,
                                               upload_data_response$links$show),
                                        httr::authenticate(email, password,
                                                           type = "basic"))

      httr::stop_for_status(upload_data_response)

      upload_data_response <- jsonlite::fromJSON(httr::content(upload_data_response,
                                                               as = "text",
                                                               type = "application/json",
                                                               encoding = "utf-8"))

      Sys.sleep(1)

    }
  }
}
