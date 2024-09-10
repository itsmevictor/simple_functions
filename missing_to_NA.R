missing_to_NA <- function(df) {
    df <- df |>
        mutate(across(everything(), ~ ifelse(.x == "", NA_character_, .x)))

    return (df)
}
