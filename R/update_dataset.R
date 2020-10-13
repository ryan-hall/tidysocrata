update_dataset <- function(dataset_id,
                           filepath_to_data,
                           action_type,
                           source_type = "upload",
                           source_parse = "true",
                           domain,
                           email,
                           password) {

  if(validate_source(filepath_to_data)) {

    open_revision_socrata <- open_revision(dataset_id, action_type, domain, email, password)
    create_source_socrata <- create_source(open_revision_socrata, source_type, source_parse, domain, email, password)
    upload_to_source_socrata <- upload_to_source(create_source_socrata, filepath_to_data, domain, email, password)
    apply_revision_socrata <- apply_revision(open_revision_socrata, domain, email, password)
    return(apply_revision_socrata)
  } else {
    stop("The data source does not appear to be a data.frame or filepath to a csv")
  }
}
