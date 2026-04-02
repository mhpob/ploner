pl_querystring <- function(plone, query, ...) {
  search_query <- plone |>
    httr2::request() |>
    httr2::req_url_path_append("++api++") |>
    httr2::req_url_path_append("@querystring-search") |>
    httr2::req_body_json(
      list(
        query = query,
        ...
      )
    ) |>
    httr2::req_auth_bearer_token(Sys.getenv("PLONE_JWT")) |>
    httr2::req_perform()

  search_query |>
    httr2::resp_body_json()
}

.qs_builder <- function(type, operator) {
  paste("plone.app.querystring.operation", type, operator, sep = ".")
}

# "https://members.devel.oceantrack.org" |>
#   pl_querystring(list(
#     qs_between("modified", "2018-01-01", "2020-01-01"),
#     qs_path("/data/repository/nsbs", recursive = F)
#   ))

#'
#' @param index the index for Plone's index-operator-value syntax
qs_before <- function(index, before) {
  list(
    i = index,
    o = .qs_builder("date", "lessThan"),
    v = before
  )
}

qs_between <- function(index, start, end) {
  list(
    i = index,
    o = "plone.app.querystring.operation.date.between",
    v = c(start, end)
  )
}

qs_contains <- function(index, text) {
  list(
    i = index,
    o = "plone.app.querystring.operation.string.contains",
    v = text
  )
}

qs_file_type <- function(file_type) {
  list(
    i = "portal_type",
    o = "plone.app.querystring.operation.selection.any",
    v = file_type
  )
}

qs_here <- function(path = "0") {
  list(
    i = "path",
    o = "plone.app.querystring.operation.string.relativePath",
    v = "0"
  )
}

qs_path <- function(path = ".", recursive = FALSE) {
  if (path %in% c(".", "0")) {
    list(
      i = "path",
      o = .qs_builder("string", "relativePath"),
      v = "0"
    )
  } else {
    # recursive arg is unused, but just need to insure the path starts with "/" or
    # ends with "::"
    list(
      i = "path",
      o = .qs_builder("string", "path"),
      v = path
    )
  }
}
