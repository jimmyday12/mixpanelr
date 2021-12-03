#' Title


#' @param bookmark_id required, bookmark_id of your Insights report can be found from the url: https://mixpanel.com/report/1/insights#report/<YOUR_BOOKMARK_ID>/example-report
#' @param method method to use, read the API documentation [here](https://developer.mixpanel.com/reference/overview)
#' @param project_id required, an integer representing the id of the project
#' @param ... optional parameters passed onto other functions. Read the Mixpanel API documentation [here]("https://developer.mixpanel.com/reference/overview)
#' @param username required, service account username https://developer.mixpanel.com/reference/service-accounts
#' @param secret required, service account secret https://developer.mixpanel.com/reference/service-accounts
#' @param eu_region optional, set to TRUE if you are enrolled in the EU server residency
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
