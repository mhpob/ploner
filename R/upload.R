pl_upload <- function(plone, type, file, path, metadata = list()) {
  # create title if not provided
  if (!("title" %in% names(metadata))) {
    metadata$title <- basename(file)
  }

  req <- plone |>
    httr2::request() |>
    httr2::req_url_path_append("++api++") |>
    httr2::req_url_path_append(path) |>
    httr2::req_auth_bearer_token(Sys.getenv("PLONE_JWT")) |>
    httr2::req_headers("Accept" = "application/json")

  if (type == "folder") {
    metadata$`@type` <- type

    resp <- req |>
      httr2::req_body_json(
        metadata
      ) |>
      httr2::req_perform()
  } else {
    file_encoded <- readBin(file, "raw", file.info(file)$size) |>
      base64enc::base64encode()

    metadata$`@type` <- title_case(type)

    metadata[[type]] <- list(
      data = file_encoded,
      encoding = "base64",
      filename = basename(file),
      `content-type` = mime::guess_type(file)
    )

    resp <- req |>
      httr2::req_body_json(metadata) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
  }

  if (httr2::resp_status(resp) == 201) {
    location <- httr2::resp_body_json(resp)$`@id`
    message(type, " successfully created at ", location)
  }
}
