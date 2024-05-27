samp <- function(df, n_rows) {
    df |> slice_sample(n = n_rows)
}
