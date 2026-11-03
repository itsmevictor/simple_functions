#' Get the name of the running script
#'
#' Finds the first script on the call stack loaded with [base::source()], then
#' falls back to the script passed to `Rscript`. For nested `source()` calls,
#' this returns the outermost sourced script.
#'
#' @returns A character string containing the script's filename, without its
#'   directory, or `"(unknown)"` when no script can be identified.
#' @examples
#' this_script()
#' @export
this_script <- function() {
  # When run via source()
  for (i in seq_along(sys.frames())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(basename(ofile))
  }

  # When run via Rscript
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    file_path <- sub("^--file=", "", file_arg[[1]])
    return(basename(gsub("~+~", " ", file_path, fixed = TRUE)))
  }

  "(unknown)"
}
