#' Delete a Socrata revision
#'
#' Deletes a Socrata dataset revision, identified by `dataset_id` and
#' `revision_id`, the revision sequence number. Revisions are numbered
#' sequentially from 0, the very first revision, and increment by 1 for each
#' new revision.
#'
#' A revision id can be identified by looking at the end of the URL of a draft,
#' i.e. 4 in "/dataset/dataset-name/abcd-1234/revisions/4", or by making a GET
#' request to "https://yourdomain.gov/api/publishing/v1/revision/abcd-1234",
#' with your dataset id, finding the most recent or relevant revision, and
#' locating the `revision_seq` number.
#'
#'
#' @param domain A Socrata domain name.
#' @param dataset_id A Socrata dataset identifier, or four-by-four.
#' @param revision_id An integer identifying the revision you wish to delete.
#' @param email A Socrata username or API Key ID
#' @param password A Socrata password or API Secret ID
#'
#' @return A HTTP status code for the delete request.
#' @export
#'
delete_revision <- function(domain, dataset_id, revision_id, email, password) {
  revision_url <- paste0("https://", domain, "/api/publishing/v1/revision/",
                         dataset_id, "/", revision_id)
  delete_revision_response <- httr::DELETE(revision_url,
                                           httr::add_headers(
                                             "Content-Type" = "application/json"
                                             ),
                                           httr::authenticate(email,
                                                              password,
                                                              type = "basic"))

  if (delete_revision_response$status_code == 202) {
    message("Successfully deleted revision ", revision_id,
            " of dataset ", dataset_id)

    return(delete_revision_response$status_code)
  } else {
    message("Failed to delete revision ", revision_id,
            " of dataset ", dataset_id)

    return(delete_revision_response$status_code)
  }
}
