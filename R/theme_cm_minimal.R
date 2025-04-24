#' @title A Minimal Theme Using CMU Sans Serif Font
#' @description Applies a minimal ggplot2 theme using the 'CMU Sans Serif' font.
#' Requires the 'sysfonts' and 'showtext' packages and the font file to be available.
#' Font loading and registration happens when the package loads via `.onLoad()`.
#' @return A ggplot2 theme object.
#' @import ggplot2
#' @export
theme_cm_minimal <- function() {
    theme_minimal() +
        theme(text = element_text(family = "CMU Sans Serif"))
}
