check_missing <- function(df) {
    df |>
        dplyr::summarise(across(everything(), ~ sum(is.na(.x)))) |>
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
        dplyr::arrange(desc(n_missing))
}
