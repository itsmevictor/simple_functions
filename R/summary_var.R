#' @export
summary_var <- function(df, var) {
    df |>
        pull({{ var }}) |>
        summary()
}
