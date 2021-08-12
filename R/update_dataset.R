update_dataset <- function(data,
                           domain, dataset_id,
                           username, password,
                           action_type,
                           source_type = "upload",
                           source_parse = "true",
                           timeout = 60,
                           delay = 1) {

  if(validate_source(data)) {
    open_revision_socrata <- open_revision(domain, dataset_id, username, password, action_type)
    create_source_socrata <- create_source(open_revision_socrata, username, password, source_type, source_parse)
    upload_to_source_socrata <- upload_to_source(create_source_socrata, username, password, data, timeout, delay)
    apply_revision_socrata <- apply_revision(upload_to_source_socrata, username, password)
    return(apply_revision_socrata)
  } else {
    stop("The data source does not appear to be a data.frame or filepath to a csv. Exiting before opening a new draft.")
  }
}
