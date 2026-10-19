# R/zzz.R
.onLoad <- function(libname, pkgname) {
  # CM Unicode ships each face of CMU Sans Serif as a separate file. All four are registered so that
  # element_text(face = "bold"/"italic"/"bold.italic") renders in the right face; with the regular file
  # alone, showtext silently falls back to the regular face.
  font_dir <- path.expand("~/Library/Fonts")
  font_paths <- file.path(font_dir, c(
    regular = "cmunss.ttf", bold = "cmunsx.ttf", italic = "cmunsi.ttf", bolditalic = "cmunso.ttf"
  ))
  names(font_paths) <- c("regular", "bold", "italic", "bolditalic")

  if (file.exists(font_paths[["regular"]])) {
    # Faces whose file is missing fall back to the regular face rather than failing.
    available_paths <- font_paths
    available_paths[!file.exists(font_paths)] <- font_paths[["regular"]]
    sysfonts::font_add(
      "CMU Sans Serif",
      regular = available_paths[["regular"]],
      bold = available_paths[["bold"]],
      italic = available_paths[["italic"]],
      bolditalic = available_paths[["bolditalic"]]
    )
    showtext::showtext_auto()
  } else {
    packageStartupMessage("Font 'CMU Sans Serif' not found at: ", font_paths[["regular"]])
    packageStartupMessage("theme_cm_minimal() might not work correctly.")
  }
}

# Optional: Clean up when package is unloaded
.onUnload <- function(libpath) {
  showtext::showtext_auto(FALSE)
}
