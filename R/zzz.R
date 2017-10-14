.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("save_after_styling") == "")
    message("Set the environment variable 'save_after_styling' (TRUE/FALSE)",
            "\nto control the behaviour of the style_active_file() add-in.")
}
