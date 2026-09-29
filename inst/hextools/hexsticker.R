library(hexSticker)
library(magick)
library(showtext)
library(sysfonts)

# Loading Google fonts (http://www.google.com/fonts)
google_font_name <- "Gayathri"
font_add_google(google_font_name)

# Automatically use showtext to render text for future devices
showtext_auto()

project_root <- here::here()

# https://www.flaticon.com/free-icon/suit_1355137
image <- image_read(file.path(project_root, "inst", "hextools", "suit.png"))
manual_logo_path <- file.path(project_root, "man", "figures", "logo.png")
color_font <- "#b3c9e5"

sticker(
  # image
  subplot = image,
  s_x = 1.1,
  s_y = 1,
  s_width = 1,
  s_height = 1.2,
  # package name
  package = "styler",
  p_color = color_font,
  p_family = google_font_name,
  p_size = 58,
  p_x = 0.35,
  p_y = 0.95,
  angle = 90,
  # image
  h_color = "black",
  h_fill = "#a18595",
  # package URL
  url = "       https://styler.r-lib.org/",
  u_size = 10,
  u_color = color_font,
  # saving sticker
  filename = manual_logo_path,
  dpi = 600,
)


rmarkdown::render("README.Rmd")
