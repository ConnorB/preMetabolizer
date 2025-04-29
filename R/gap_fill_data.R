#' Gap fill missing values in stream metabolism data
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
#' @importFrom tidyr nest unnest
#' @importFrom dplyr bind_rows coalesce
gap_fill_data <- function(df, metadata,
                          method = "kalman",
                          maxgap = Inf,
                          progress = TRUE) {
  # Validate inputs
  validate_input_data(df, metadata)
  validate_parameters(method, maxgap)

  # Nest data by site
  nested_df <- df %>%
    tidyr::nest(data = -.data$Site)

  n_sites <- nrow(nested_df)

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
      seq_len(n_sites),
      function(i) {
        site_df <- nested_df$data[[i]]
        site_name <- nested_df$Site[i]
        site_meta <- get_site_metadata(site_name, metadata)

        message(sprintf("\nSite %s:", site_name))

        # Impute numeric columns
        numeric_cols <- setdiff(names(site_df)[sapply(site_df, is.numeric)], "PAR.obs")
        for (col in numeric_cols) {
          na_count <- sum(is.na(site_df[[col]]))
          if (na_count == 0) {
            message(sprintf("  %s: No missing values (skipping)", col))
            next
          }

          message(sprintf("  %s: %d gaps (filling with %s)", col, na_count, method))

          tryCatch(
            {
              site_df[[col]] <- apply_imputation_method(site_df[[col]], method, maxgap)
            },
            error = function(e) {
              message(sprintf("  Error processing %s: %s", col, e$message))
            }
          )
        }

        # Handle PAR data if present
        if ("PAR.obs" %in% colnames(site_df)) {
          na_count <- sum(is.na(site_df$PAR.obs))
          if (na_count == 0) {
            message("  PAR.obs: No missing values (skipping)")
          } else {
            tryCatch(
              {
                message(sprintf("  PAR.obs: %d gaps (filling with modeled light)", na_count))

                site_df$solar.time <- streamMetabolizer::convert_UTC_to_solartime(site_df$dateTime,
                                                                                  longitude = site_meta$Long,
                                                                                  time.type = "mean solar"
                )

                maxLight <- max(site_df$PAR.obs, na.rm = TRUE)

                site_df$CalcLight <- streamMetabolizer::calc_light(
                  site_df$solar.time,
                  latitude = site_meta$Lat,
                  longitude = site_meta$Long,
                  max.PAR = maxLight
                )

                site_df$PAR.obs[site_df$PAR.obs < 0.01] <- 0
                site_df$PAR.obs <- dplyr::coalesce(site_df$PAR.obs, site_df$CalcLight)
                site_df$CalcLight <- NULL
                site_df$solar.time <- NULL
              },
              error = function(e) {
                message(sprintf("  Error processing PAR.obs: %s", e$message))
              }
            )
          }
        }

        p()
        site_df
      }
    )

    # Combine results
    nested_df$data <- imputed_dfs
    tidyr::unnest(nested_df, cols = .data$data)
  })
}
