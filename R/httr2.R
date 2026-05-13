http_req_throttle <- function(req) {
  httr2::req_throttle(
    req,
    capacity = 120,
    fill_time_s = 60
  )
}

http_req_perform_parallel <- function(
  reqs,
  ...,
  on_error = c("stop", "return", "continue"),
  progress = FALSE,
  max_active = 8
) {
  on_error <- match.arg(on_error)
  reqs <- lapply(reqs, http_req_throttle)

  httr2::req_perform_parallel(
    reqs,
    ...,
    on_error = on_error,
    progress = progress,
    max_active = max_active
  )
}

http_req_perform <- function(
  req,
  ...,
  on_error = c("stop", "return", "continue")
) {
  http_req_perform_parallel(
    list(req),
    ...,
    on_error = on_error
  )[[1]]
}
