print_max <- function(df) {
    n <- df |> nrow()

    df |> print(n = n, na.print = "NA")
}