# R/zzz.R
.onLoad <- function(libname, pkgname) {
  # Use a relative or system-independent way to find fonts if possible,
  # otherwise, ensure the path is correct for the target system.
  font_path <- "/Users/victorkreitmann/Library/Fonts/cmunss.ttf" # Be careful with hardcoded paths
  if (file.exists(font_path)) {
    sysfonts::font_add("CMU Sans Serif", regular = font_path)
    showtext::showtext_auto()
  } else {
    packageStartupMessage("Font 'CMU Sans Serif' not found at: ", font_path)
    packageStartupMessage("theme_cm_minimal() might not work correctly.")
  }
}

# Optional: Clean up when package is unloaded
.onUnload <- function(libpath) {
  showtext::showtext_auto(FALSE)
}
