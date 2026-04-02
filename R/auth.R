#' Authenticate with Plone
#'
#' Adds a JSON Web Token to the session environment variables
#'
#' @param plone Location of the Plone server
pl_login <- function(plone) {
  username <- askpass::askpass("Username: ")
  password <- askpass::askpass("Password: ")

  login_request <- plone |>
    httr2::request() |>
    httr2::req_url_path_append("++api++/@login") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_body_json(
      list(
        login = username,
        password = password
      )
    )

  withCallingHandlers(
    login_response <- login_request |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    httr2_http_401 = function(cnd) {
      rlang::abort("Login failed. Check the supplied credentials", parent = cnd)
    }
  )

  Sys.setenv(PLONE_JWT = login_response$token)
}

#' Renews the JSON Web Token for a previously-authenticated user
#'
#'
pl_login_renew <- function(plone) {
  renew_request <- plone |>
    httr2::request() |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("++api++/@login-renew") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_auth_bearer_token(Sys.getenv("PLONE_JWT"))

  withCallingHandlers(
    renew_response <- renew_request |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    httr2_http_401 = function(cnd) {
      rlang::abort(
        "Token renewal failed. Check the supplied credentials",
        parent = cnd
      )
    }
  )

  Sys.setenv(PLONE_JWT = renew_response$token)
}

#'
#'
pl_logout <- function(plone) {
  logout_request <- plone |>
    httr2::request() |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("++api++/@logout") |>
    httr2::req_headers(
      Accept = "application/json"
    ) |>
    httr2::req_auth_bearer_token(Sys.getenv("PLONE_JWT")) |>
    httr2::req_perform()

  Sys.unsetenv("PLONE_JWT")
}
