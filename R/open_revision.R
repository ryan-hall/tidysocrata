#' Open a revision on a Socrata dataset
#'
#' @description Opens a revision (draft) on a Socrata dataset. When opening a
#' revision on an existing dataset, provide the dataset id as well as an action
#' type. The action type determines the type of revision: an "update", "replace",
#' or "delete" revision. An "update" revision will utilize a row identifier, if
#' one is set on the Socrata dataset, to update existing rows and append new
#' rows. A "replace" will perform a complete replace of the existing dataset
#' with the file you are sending with the revision. A "delete" action type
#' requires a row identifier to be have been set on the Socrata dataset, as
#' the revision will delete rows in the published dataset that are
#' provided in the file in the revision.
#'
#' This function simply opens a revision and sets the action type of that
#' revision.
#'
#' @param domain The domain name of the Socrata site, without "https://"
#' @param dataset_id The dataset id, or four-by-four, of the Socrata dataset
#' @param action_type The type of revision you want to create: "update",
#' "replace", or "delete"
#' @param username Your Socrata username or API Key ID
#' @param password Your Socrata password or API Key Secret
#'
#' @return A named list with the asset id, the revision sequence id,
#' the url of the new revision, and the status code returned while
#' opening the revision
#' @export
#'
open_revision <- function(domain, dataset_id, action_type, username, password) {
  ## Validate inputs. If anything doesn't look right, don't open a revision.
  dataset_id <- casefold(as.character(dataset_id))

  if(!grepl("^[A-Za-z0-9]{4}[-]{1}[A-Za-z0-9]{4}$", dataset_id)) {
    stop(dataset_id, " does not appear to be a valid Socrata dataset ",
        "identifier format.")
  }

  action_type <- casefold(action_type)
  if(!any(c(action_type == "update", action_type == "replace",
            action_type == "delete")))
    stop("Invalid action_type: '",
         action_type,
         "' should be one of 'update', 'replace', or 'delete' " )

  domain <- validate_domain(domain)

  open_revision_endpoint <- paste0('https://',
                                   domain,
                                   '/api/publishing/v1/revision/',
                                   dataset_id)

  open_revision_json <- paste0('{"action": {"type":"', action_type,'"}}')

  open_revision_response <- httr::POST(open_revision_endpoint,
                                 body = open_revision_json,
                                 httr::add_headers(
                                   "Content-Type" = "application/json"),
                                 httr::authenticate(username,
                                                    password,
                                                    type = "basic")

  )

  if(open_revision_response$status_code == '201') {
    message("Opened new revision on dataset ", dataset_id)

    open_revision_response_body <- jsonlite::fromJSON(
      httr::content(open_revision_response, as = "text",
                    type = "application/json",
                    encoding = "utf-8")
      )

    return(list(asset_id = dataset_id,
                revision_id = open_revision_response_body$resource$revision_seq,
                revision_url = paste0('https://',
                                      domain,
                                      open_revision_response_body$links$show),
                status_code = open_revision_response$status_code))

  } else {
    httr::stop_for_status(open_revision_response$status_code)
  }
}
