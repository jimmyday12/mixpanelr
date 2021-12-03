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
