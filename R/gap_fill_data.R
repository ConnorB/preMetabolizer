#' Impute missing values in stream metabolism data
#'
#' @param df A dataframe containing the data to be imputed with a Site column
#' @param metadata Dataframe containing site metadata (must include Site, Lat, Long, Elev_m)
#' @param method Imputation method: "kalman" (default), "ma", "linear", or "spline"
#' @param maxgap Maximum size of gap to fill (default: Inf)
#' @param progress Show progress updates (default: TRUE)
#' @return A dataframe with imputed values
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom imputeTS na_kalman
#' @importFrom progressr with_progress
#' @importFrom tibble as_tibble
gap_fill_data <- function(df, metadata,
                          method = "kalman",
                          maxgap = Inf,
                          progress = TRUE) {
  # Validate inputs
  validate_input_data(df, metadata)
  validate_parameters(method, maxgap)

  # Process sites
  site_dfs <- split_by_site(df)
  n_sites <- length(site_dfs)

  # Set up progress reporting
  if (progress) {
    progressr::handlers(progressr::handler_progress(
      format = "[:bar] :percent | Elapsed: :elapsed | ETA: :eta",
      width = 80,
      complete = "=",
      incomplete = "-"
    ))
  }

  # Wrap the processing in progressr
  progressr::with_progress({
    p <- progressr::progressor(steps = n_sites)

    imputed_dfs <- future.apply::future_lapply(
      site_dfs,
      function(site_df) {
        result <- impute_single_site(site_df, metadata, method, maxgap)
        p()
        result
      }
    )

    dplyr::bind_rows(imputed_dfs)
  })
}
