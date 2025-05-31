#' Create a Codebook for a Dataset using an LLM (Single Query Version)
#'
#' This function takes a vector of column names and optionally dataset context,
#' then uses an LLM (defaults to Anthropic's Claude) via the 'ellmer' package
#' to generate descriptions for all columns in a single query. It can also
#' translate non-English column names.
#'
#' @param column_names A character vector of the original column names.
#' @param translate A logical scalar. If TRUE, the function will ask the LLM
#'   to translate non-English column names into English.
#' @param context An optional character string providing context about the
#'   dataset (e.g., "This dataset contains customer purchase history.").
#' @param chat An optional `ellmer` Chat object. If NULL (default), a new
#'   `chat_anthropic()` object will be created. This allows using pre-configured
#'   chats (e.g., with specific models, parameters, or API keys).
#' @param ... Additional arguments passed to `ellmer::chat_anthropic()` if `chat`
#'   is NULL. Useful for specifying the model, API key, etc.
#'   Example: `model = "claude-3-opus-20240229"`.
#'
#' @return A tibble containing the codebook. It will always have the
#'   'original_name' and 'description' columns. If `translate` is TRUE,
#'   it will also include a 'translated_name' column. Returns an empty tibble
#'   with a warning if the LLM call fails or returns unexpected data.
#'
#' @export
#'
#' @details
#' Requires the `ellmer`, `dplyr`, `glue`, and `tibble` packages.
#' You need to have an Anthropic API key set up, typically via the
#' `ANTHROPIC_API_KEY` environment variable, unless you provide a `chat` object
#' that's already configured.
#' This version makes a single call to the LLM API, which can be more efficient
#' and provide better context for translations and descriptions compared to
#' making individual calls per column.
#'
#' @examples
#' \dontrun{
#' # Example: Needs ANTHROPIC_API_KEY environment variable set
#'
#' cols_german <- c("Kundennummer", "Bestelldatum", "Produkt Name", "Menge", "Preis")
#' context_german <- "Datensatz über Kundenbestellungen in einem deutschen Online-Shop."
#'
#' # Generate codebook with translation
#' codebook_german <- create_codebook(cols_german, translate = TRUE, context = context_german)
#' print(codebook_german)
#'
#' # ---
#'
#' cols_norwegian <- c(
#'   "Fylkenummer", "Fylkenavn", "Kommunenummer", "Kommunenavn",
#'   "Antall stemmeberettigede", "Godkjente stemmegivninger - Totalt"
#' )
#' context_norwegian <- "Dataset of Norwegian municipal election results."
#' codebook_norwegian <- create_codebook(cols_norwegian, translate = TRUE, context = context_norwegian)
#' print(codebook_norwegian)
#'
#' # ---
#'
#' cols_english <- c("customer_id", "order_date", "product_name", "quantity", "price")
#' context_english <- "Dataset about customer orders in an online store."
#'
#' # Generate codebook without translation
#' codebook_english <- create_codebook(cols_english, translate = FALSE, context = context_english)
#' print(codebook_english)
#'
#' # ---
#'
#' # Using a specific model via ...
#' codebook_opus <- create_codebook(
#'   cols_english,
#'   translate = FALSE,
#'   context = context_english,
#'   model = "claude-3-opus-20240229"
#' )
#' print(codebook_opus)
#' }
create_codebook <- function(column_names,
                            translate,
                            context = NULL,
                            chat = NULL,
                            ...) {
  # --- Input Validation ---
  if (!is.character(column_names) || length(column_names) == 0) {
    stop("`column_names` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.logical(translate) || length(translate) != 1 || is.na(translate)) {
    stop("`translate` must be a single TRUE or FALSE value.", call. = FALSE)
  }
  if (!is.null(context) && (!is.character(context) || length(context) != 1)) {
    stop("`context` must be NULL or a single character string.", call. = FALSE)
  }
  if (!is.null(chat) && !inherits(chat, "Chat")) {
    stop("`chat` must be NULL or an object of class 'Chat' from the ellmer package.", call. = FALSE)
  }

  # --- Setup ---
  # Initialize chat object if not provided
  active_chat <- chat %||% ellmer::chat_anthropic(...)

  # Define the expected output structure: an object containing an array of entries
  if (translate) {
    entry_type <- ellmer::type_object(
      original_name = ellmer::type_string(
        "The original column name from the input list."
      ),
      translated_name = ellmer::type_string(
        "The English translation of the column name. If already English, return the original name."
      ),
      description = ellmer::type_string(
        "A concise (~1-2 sentences) English description of the column's likely meaning."
      )
    )
  } else {
    entry_type <- ellmer::type_object(
      original_name = ellmer::type_string(
        "The original column name from the input list."
      ),
      description = ellmer::type_string(
        "A concise (~1-2 sentences) English description of the column's likely meaning."
      )
    )
  }

  output_type <- ellmer::type_object(
    codebook_entries = ellmer::type_array(
      description = "An array of codebook entries, one for each column name provided.",
      items = entry_type
    )
  )

  # --- Prompt Generation ---
  context_text <- if (!is.null(context)) context else "No specific context provided."
  column_list_md <- paste(paste0("- '", column_names, "'"), collapse = "\n")

  translate_instruction <- if (translate) {
    "2. 'translated_name': The English translation of the column name. If the original name is already English, return the original name. Ensure translated names are unique where possible for distinct original names. It is important that each string is in snake_case format, e.g., 'election_date'.
    3. 'description': A concise (~1-2 sentences) English description of what this column likely represents based on its name and the dataset context."
  } else {
    "2. 'description': A concise (~1-2 sentences) English description of what this column likely represents based on its name and the dataset context. The column name is expected to be in English already, no translation needed."
  }

  prompt <- glue::glue(
    "You are creating a codebook for a dataset.
  The overall dataset context is: '{context_text}'.
  The column names are:
  {column_list_md}
  Your task is to generate a JSON object containing a single key 'codebook_entries'.
  The value associated with 'codebook_entries' should be an array of JSON objects.
  Each object in the array must correspond to one of the column names provided above and MUST contain the following keys:
  1. 'original_name': The original column name exactly as provided in the input list.
  {translate_instruction}
  Ensure the output is a valid JSON object matching this structure precisely. Make sure every input column name has a corresponding entry in the output array."
  )

  message("Sending 1 request to the LLM to generate codebook entries for ", length(column_names), " columns...")


  llm_results_list <- tryCatch(
    active_chat$extract_data(
      prompt, 
      type = output_type,
      convert = TRUE 
    ),
    error = function(e) {
      warning(paste("LLM API call failed:", e$message), call. = FALSE)
      NULL # Return NULL on error
    }
  )

  # --- Process Results ---
  if (is.null(llm_results_list) || !is.list(llm_results_list) || !"codebook_entries" %in% names(llm_results_list)) {
    warning("LLM did not return the expected 'codebook_entries' structure. Returning empty codebook.", call. = FALSE)
    # Return an empty tibble with the correct structure
    if (translate) {
      return(tibble::tibble(original_name = character(0), translated_name = character(0), description = character(0)))
    } else {
      return(tibble::tibble(original_name = character(0), description = character(0)))
    }
  }

  # Extract the data frame (ellmer >= 0.2.0 returns a data frame directly here if convert=TRUE)
  llm_df <- llm_results_list$codebook_entries

  if (!is.data.frame(llm_df) || !"original_name" %in% names(llm_df)) {
    warning("LLM response's 'codebook_entries' element is not a data frame with 'original_name'. Returning empty codebook.", call. = FALSE)
    if (translate) {
      return(tibble::tibble(original_name = character(0), translated_name = character(0), description = character(0)))
    } else {
      return(tibble::tibble(original_name = character(0), description = character(0)))
    }
  }

  # --- Combine and Finalize Results ---
  original_df <- tibble::tibble(original_name = column_names)

  # Merge LLM results with original column names to ensure order and completeness
  # Use left_join to keep all original column names
  final_df <- dplyr::left_join(original_df, llm_df, by = "original_name")

  # Check for missing results after merge
  missing_count <- sum(is.na(final_df$description)) # Check description as a proxy
  if (missing_count > 0) {
    warning(sprintf("LLM results missing for %d out of %d columns. Missing entries will have NA values.", missing_count, length(column_names)), call. = FALSE)
  }
  # Check for more results than expected (shouldn't happen with left_join, but good practice)
  if (nrow(final_df) > length(column_names)) {
    warning("LLM returned more entries than original columns. Extraneous entries ignored.", call. = FALSE)
    # The left_join should prevent this, but as a safeguard:
    final_df <- final_df |> dplyr::filter(original_name %in% column_names)
  }


  # Select and order columns based on the 'translate' flag
  if (translate) {
    # Ensure translated_name column exists, even if it wasn't returned properly
    if (!"translated_name" %in% names(final_df)) {
      final_df$translated_name <- NA_character_
    }
    # Ensure description column exists
    if (!"description" %in% names(final_df)) {
      final_df$description <- NA_character_
    }
    final_df <- dplyr::select(final_df, original_name, translated_name, description)
  } else {
    # Ensure description column exists
    if (!"description" %in% names(final_df)) {
      final_df$description <- NA_character_
    }
    final_df <- dplyr::select(final_df, original_name, description)
  }

  message("Codebook generation complete.")
  return(tibble::as_tibble(final_df))
}
