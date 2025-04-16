#' @export
time_to_day <- function(string) {
    output <- as.Date(substr(string, 1, 10))

    return (output)
}
