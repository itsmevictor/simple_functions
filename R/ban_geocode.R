#' Geocode French Addresses Using the BAN API
#'
#' This function queries the French Base Adresse Nationale (BAN) API to geocode
#' French addresses. It can search for specific types of locations or perform
#' a general search.
#'
#' @param query A character string containing the address or location to geocode.
#' @param timeout_sec Numeric. The timeout in seconds for the HTTP request. Default is 10.
#' @param types Character vector or NULL. Specific types to search for (e.g., "municipality", "locality").
#'   If NULL, performs a general search. Default is NULL.
#'
#' @return A data frame containing geocoding results with columns for location
#'   properties including coordinates, scores, and administrative information.
#'   Returns a data frame with NA values if no results are found or if an error occurs.
#'
#' @details
#' This function uses the French BAN (Base Adresse Nationale) API to geocode addresses.
#' The API returns detailed information including coordinates, administrative codes,
#' and confidence scores. When multiple types are specified, the function returns
#' the highest-scoring result across all types.
#'
#' @examples
#' \dontrun{
#' # Basic geocoding
#' result <- ban_geocode("8 Boulevard du Port 80000 Amiens")
#' 
#' # Search for specific types
#' result <- ban_geocode("Amiens", types = c("municipality", "locality"))
#' }
#'
#' @importFrom httr GET content status_code timeout
#' @importFrom dplyr bind_rows select starts_with arrange desc slice
#' @export
ban_geocode <- function(query, timeout_sec = 10, types = NULL) {
  base_url <- "https://ban.addok.xyz/search/?q="
  limit <- 5

  # Define the structure of the expected result with NA values
  expected_structure <- data.frame(
    type = NA,
    "properties.label" = NA,
    "properties.score" = NA,
    "properties.id" = NA,
    "properties.type" = NA,
    "properties.name" = NA,
    "properties.postcode" = NA,
    "properties.citycode" = NA,
    "properties.x" = NA,
    "properties.y" = NA,
    "properties.population" = NA,
    "properties.city" = NA,
    "properties.context" = NA,
    "properties.importance" = NA,
    "properties.municipality" = NA,
    "properties.locality" = NA,
    "properties.oldcitycode" = NA,
    "properties.oldcity" = NA,
    stringsAsFactors = FALSE
  )

  results_df <- data.frame()

  if (is.null(types)) {
    full_url <- URLencode(paste0(base_url, query, "&limit=", limit, "&autocomplete=0"))

    # Sending the GET request with a timeout and error handling
    get_query <- tryCatch(
      {
        httr::GET(full_url, timeout(timeout_sec))
      },
      error = function(e) {
        message("GET request failed for query '", query, "': ", e$message)
        return(NULL)
      }
    )

    # If the GET request failed, return the expected empty structure
    if (is.null(get_query) || status_code(get_query) != 200) {
      if (!is.null(get_query)) {
        message("The API sent back an error: ", status_code(get_query))
      }
      return(expected_structure)
    }

    # Extract content from the response
    content_data <- content(get_query)

    # Check if the API returns features
    if (length(content_data$features) == 0) {
      return(expected_structure)
    }

    # Transform the results into a data frame
    type_results_df <-
      bind_rows(lapply(content_data$features, as.data.frame)) |>
      select(-starts_with("geometry"))

    results_df <- bind_rows(results_df, type_results_df)

    if (nrow(results_df) == 0) {
      return(expected_structure)
    } else {
      return(results_df)
    }
  } else {
    for (type in types) {
      # Construct the full URL with type
      full_url <- URLencode(paste0(base_url, query, "&type=", type, "&limit=", limit, "&autocomplete=0"))

      # Sending the GET request to the API
      get_query <- tryCatch(
        {
          httr:GET(full_url, timeout(timeout_sec))
        },
        error = function(e) {
          message("GET request failed for query '", query, "': ", e$message)
          return(NULL)
        }
      )

      # Checking if the request was successful
      if (status_code(get_query) == 200) {
        # Extracting content from the response
        content_data <- content(get_query)

        # Checks that the API returns features
        if (length(content_data$features) == 0) {
          # Return the empty data frame if no features are found
          return(expected_structure)
        }

        # Transform the results into a data frame
        type_results_df <-
          bind_rows(lapply(content_data$features, as.data.frame)) |>
          select(-starts_with("geometry")) |>
          arrange(desc(properties.score)) |>
          slice(1)
        # slice_max(order_by = "properties.score", with_ties = FALSE, na_rm = FALSE)

        # Combine with the previous results
        results_df <- bind_rows(results_df, type_results_df)
      } else {
        # If the API returned an error, make them all NA & return an error message without stopping the function
        type_results_df <- expected_structure
        message("The API sent back an error for type '", type, "': ", status_code(get_query))
      }
    }

    if (nrow(results_df) == 0) {
      # Return the expected structure with NA values if results_df is empty
      return(expected_structure)
    } else {
      # Otherwise return the results gathered (both types)
      results_df |>
        arrange(desc(properties.score)) |>
        slice(1)
      # slice_max(order_by = "properties.score", with_ties = FALSE, na_rm = TRUE)
    }
  }
}
