#' Title
#'
#' @param method
#' @param project_id
#' @param ...
#' @param username
#' @param secret
#' @param eu_region
#'
#' @return
#' @export
#'
#' @examples
mixpanel_api <- function(method, project_id,  ..., username, secret, eu_region = FALSE) {

  if (method == "export") {
    base_url <- "mixpanel.com/api/2.0"
    if (eu_region) prefix <- "https://data-eu." else prefix <- "https://data."
  } else {
    base_url <- "mixpanel.com/api/2.0"
    if (eu_region) prefix <- "https://eu." else prefix <- "https://"
  }

  api_url <- paste0(prefix, base_url)

  params <- list(
    project_id = project_id,
    ...
  )

  req <- httr2::request(api_url) %>%
    httr2::req_url_path_append(method) %>%
    httr2::req_url_query(!!!params) %>%
    httr2::req_auth_basic(username, secret)

  req %>%
    httr2::req_perform()

}
