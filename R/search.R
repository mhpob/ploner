#'
pl_search <- function(plone, ...) {
  search_query <- plone |>
    httr2::request() |>
    httr2::req_url_path_append("++api++") |>
    httr2::req_url_path_append("@search") |>
    httr2::req_url_query(..., .multi = "explode") |>
    httr2::req_auth_bearer_token(Sys.getenv("PLONE_JWT")) |>
    httr2::req_perform()

  search_query |>
    httr2::resp_body_json()
}
#note that ?path.query=  searches the given path
