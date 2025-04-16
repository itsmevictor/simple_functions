#' @export
sysfonts::font_add("CMU Sans Serif", "/Users/victorkreitmann/Library/Fonts/cmunss.ttf") # Ensure the path points to the correct font file
showtext::showtext_auto()

theme_cm_minimal <- function() {
    theme_minimal() +
        theme(text = element_text(family = "CMU Sans Serif"))
}
