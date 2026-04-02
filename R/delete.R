pl_delete <- function(plone, path) {
  resp <- plone |>
    httr2::request() |>
    httr2::req_url_path_append("++api++") |>
    httr2::req_url_path_append(path) |>
    httr2::req_auth_bearer_token(Sys.getenv("PLONE_JWT")) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()

  if (httr2::resp_status(resp) == 204) {
    message(path, " successfully deleted.\n")
  }
}

# pl_delete(plone, "/data/repository/nsbs/125")
# pl_delete(plone, "/data/repository/nsbs/sturg_alert2.png")
