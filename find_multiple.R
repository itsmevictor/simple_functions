find_multiple <- function(df, group_var) {
    df <- df |>
        group_by({{group_var}}) |>
        mutate(n_count = n()) |>
        ungroup()
    
    return(df)
}
