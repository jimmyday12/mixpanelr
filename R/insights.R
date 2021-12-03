
#' Title
#'
#' @param project_id
#' @param bookmark_id
#' @param workspace_id
#' @param username
#' @param secret
#' @param eu_region
#'
#' @return
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
                       secret = secret)


  series <- resp %>%
    purrr::pluck("series") %>%
    purrr::map(purrr::flatten_dfr) %>%
    purrr::map_dfr(tidyr::pivot_longer, tidyr::everything(), names_to = "date",
            .id = "series") %>%
    dplyr::mutate(date = lubridate::ymd_hms(date))

  series %>%
    tibble::as_tibble()
}
