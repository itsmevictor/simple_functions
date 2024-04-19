check_missing <- function(df) {
    df |>
        summarise(across(everything(), ~ sum(is.na(.x)))) |>
        pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
        arrange(desc(n_missing))
}
