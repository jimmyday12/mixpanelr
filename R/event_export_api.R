#' Export Mixpanel Events
#'
#' Use the Mixpanel Export Data API to return raw events ffrom specific date range. Documentation on the API itself can be found at the [API Documentation](https://developer.mixpanel.com/reference/raw-event-export).

#' @param project_id required, an integer representing the id of the project
#' @param from_date required string, The date in yyyy-mm-dd format to query to. This date is inclusive.
#' @param to_date required string, The date in yyyy-mm-dd format from which to begin querying from. This date is inclusive.
#' @param limit optional integer, The max number of events to be returned.
#' @param event optional vector, The event or events that you wish to get data for
#' @param where optional string, An expression to filter events by. More info on expression sequence structure can be found [here](https://developer.mixpanel.com/reference-link/segmentation-expressions)
#' @param username required, service account username see https://developer.mixpanel.com/reference/service-accounts
#' @param secret required, service account secret see https://developer.mixpanel.com/reference/service-accounts
#' @param eu_region optional, set to TRUE if you are enrolled in the EU server residency
#'
#' @return returns a tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  mixpanel_export_events(id, "2021-11-01", "2021-12-01")
#'  }
mixpanel_export_events <- function(project_id,
                                   from_date,
                                   to_date,
                                   limit = NULL,
                                   event = NULL,
                                   where = NULL,
                                   username,
                                   secret,
                                   eu_region = FALSE) {

  event <- jsonlite::toJSON(event)

  resp <- mixpanel_api(method = "export",
                       project_id = project_id,
                       from_date = from_date,
                       to_date = to_date,
                       limit = limit,
                       event = event,
                       where = where,
                       username = username,
                       secret = secret,
                       eu_region = eu_region)

  json_file <- resp %>%
    httr2::resp_body_string()

  json_file <- gsub('\n', ',', trimws(json_file), fixed = TRUE)
  json_file <- paste0('[', json_file, ']')

  jsonlite::fromJSON(json_file, flatten = TRUE) %>%
    tibble::as_tibble()

}
