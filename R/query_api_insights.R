#' Query saved insights report
#'
#' Return data from a saved insights report in Mixpanel
#'
#' @param project_id required, an integer representing the id of the project
#' @param bookmark_id required, bookmark_id of your Insights report can be found from the url: https://mixpanel.com/report/1/insights#report/<YOUR_BOOKMARK_ID>/example-report
#' @param workspace_id optional, the id of the workspace if applicable.
#' @param username required, service account username https://developer.mixpanel.com/reference/service-accounts
#' @param secret required, service account secret https://developer.mixpanel.com/reference/service-accounts
#' @param eu_region optional, set to TRUE if you are enrolled in the EU server residency
#'
#' @return returns a tibble with the data from the insights report
#' @export
#'
#' @examples
mixpanel_insights <- function(project_id, bookmark_id, workspace_id = NULL,
                              username, secret, eu_region = FALSE) {

  resp <- mixpanel_api(method = "insights",
                       project_id = project_id,
                       bookmark_id = bookmark_id,
                       workspace_id = workspace_id,
                       username = username,
                       secret = secret,
                       eu_region = eu_region)


  json <- resp %>%
    httr2::resp_body_json(flatten = TRUE)

  series <- json %>%
    purrr::pluck("series") %>%
    purrr::map(purrr::flatten_dfr) %>%
    purrr::map_dfr(tidyr::pivot_longer, tidyr::everything(), names_to = "date",
            .id = "series") %>%
    dplyr::mutate(date = lubridate::ymd_hms(date))

  series %>%
    tibble::as_tibble()
}
