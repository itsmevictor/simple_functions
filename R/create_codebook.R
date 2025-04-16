# Ensure the necessary packages are loaded
# install.packages("ellmer") # If not already installed
# install.packages("rlang") # ellmer uses rlang internally for %||%
pacman::p_load(ellmer, rlang, dplyr)

#' Create a Codebook for a Dataset using an LLM
#'
#' This function takes a vector of column names and optionally dataset context,
#' then uses an LLM (defaults to Anthropic's Claude) via the 'ellmer' package
#' to generate descriptions for each column. It can also translate non-English
#' column names.
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
#' @param rpm Maximum requests per minute to send to the API. Passed to
#'   `extract_data_parallel`.
#' @param max_active Maximum number of parallel requests allowed simultaneously.
#'   Passed to `extract_data_parallel`.
#'
#' @return A data frame containing the codebook. It will always have the
#'   'original_name' and 'description' columns. If `translate` is TRUE,
#'   it will also include a 'translated_name' column.
#'
#' @details
#' Requires the `ellmer` and `dplyr` packages.
#' You need to have an Anthropic API key set up, typically via the
#' `ANTHROPIC_API_KEY` environment variable, unless you provide a `chat` object
#' that's already configured.
#' The function uses `extract_data_parallel` for efficiency, which sends
#' multiple requests to the LLM API concurrently. Be mindful of API rate limits
#' and costs.
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
#'
#' # ---
#'
#' # Using a pre-configured chat object
#' my_chat <- chat_anthropic(model = "claude-3-5-sonnet-20240620", params = params(temperature = 0.2))
#' codebook_preconf <- create_codebook(
#'   cols_english,
#'   translate = FALSE,
#'   context = context_english,
#'   chat = my_chat
#'  )
#' print(codebook_preconf)
#'
#' }
create_codebook <- function(column_names,
                            translate,
                            context = NULL,
                            chat = NULL,
                            ...,
                            rpm = 60,
                            max_active = 5) {

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
   if (!is.numeric(rpm) || length(rpm) != 1 || rpm <= 0) {
    stop("`rpm` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(max_active) || length(max_active) != 1 || max_active <= 0) {
    stop("`max_active` must be a single positive number.", call. = FALSE)
  }


  # --- Setup ---
  # Initialize chat object if not provided (defaulting to Anthropic Claude)
  # Pass extra arguments (...) like model name to chat_anthropic
  active_chat <- chat %||% ellmer::chat_anthropic(...)
  print(class(active_chat))
  
  # Define the structure we expect back from the LLM for *each* column
  if (translate) {
    output_type <- type_object(
      translated_name = type_string(
        "The English translation of the column name. If already English, return the original name."
      ),
      description = type_string(
        "A concise (~1-2 sentences) English description of the column's likely meaning or purpose based on its name and the dataset context."
      )
    )
  } else {
    output_type <- type_object(
      description = type_string(
         "A concise (~1-2 sentences) English description of the column's likely meaning or purpose based on its name and the dataset context."
      )
    )
  }

  # --- Prompt Generation ---
  prompts <- list(length(column_names))
  context_text <- if (!is.null(context)) sprintf("The overall dataset context is: '%s'.", context) else ""
  translate_instruction_item <- if (translate) {
      "Translate the column name to English if it's not already English (if it is, just repeat the original name)."
    } else {
      "The column name is expected to be in English already, no translation needed."
    }

  for (i in seq_along(column_names)) {
    col_name <- column_names[i]
    # Construct a specific prompt for each column
    prompts[i] <- sprintf(
      "You are creating a codebook entry for a dataset column.\nColumn Name: '%s'\n%s\nYour task is to:\n1. %s\n2. Provide a concise English description of what this column likely represents.\n\nExtract the requested information as a JSON object with the specified fields.",
      col_name,
      context_text,
      translate_instruction_item
    )
  }

   # --- LLM Interaction ---
  message("Sending ", length(prompts), " requests to the LLM to generate codebook entries...")
  # Use extract_data_parallel for efficiency. It returns a data frame when
  # type is an object and convert=TRUE (default).
  llm_results <- active_chat$extract_data_parallel(
      prompts = prompts,
      type = output_type,
      convert = TRUE, # Automatically converts the list of objects to a data frame
      rpm = rpm,
      max_active = max_active
  )
  message("Received results from the LLM.")

  # --- Combine Results ---

  # Check if the number of results matches the number of columns
  if (nrow(llm_results) != length(column_names)) {
      warning(sprintf("LLM returned %d results, but expected %d. Some columns might be missing descriptions or translations. Check API limits or potential errors.", nrow(llm_results), length(column_names)), call. = FALSE)
      # Attempt a merge, which might introduce NAs
      original_df <- data.frame(original_name = column_names)
      # This assumes the order is maintained for successful results. A more robust
      # approach might involve tracking indices or IDs if the API supported it.
      # For now, we'll pad with NAs if lengths mismatch.
       if (nrow(llm_results) < nrow(original_df)) {
           llm_results[(nrow(llm_results) + 1):nrow(original_df), ] <- NA
       } else if (nrow(llm_results) > nrow(original_df)) {
            llm_results <- llm_results[1:nrow(original_df), ] # Truncate extra results
       }
       final_df <- dplyr::bind_cols(original_df, llm_results)

  } else {
       final_df <- dplyr::bind_cols(data.frame(original_name = column_names), llm_results)
  }


  # Select and order columns based on the 'translate' flag
  if (translate) {
    # Ensure translated_name column exists, even if potentially all NAs from failed merges
    if (!"translated_name" %in% names(final_df)) {
        final_df$translated_name <- NA_character_
    }
    final_df <- dplyr::select(final_df, original_name, translated_name, description)
  } else {
    final_df <- dplyr::select(final_df, original_name, description)
  }

  return(final_df)
}
