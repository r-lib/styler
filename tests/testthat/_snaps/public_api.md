# messages (via cat()) of style_file are correct

    Code
      cat(catch_style_file_output(file.path("public-api", "xyzdir-dirty",
        "dirty-sample-with-scope-tokens.R")), sep = "\n")
    Output
      Styling  1  files:
       dirty-sample-with-scope-tokens.R i 
      ----------------------------------------
      Status	Count	Legend 
      v 	0	File unchanged.
      i 	1	File changed.
      x 	0	Styling threw an error.
      ----------------------------------------
      Please review the changes carefully!

---

    Code
      cat(catch_style_file_output(file.path("public-api", "xyzdir-dirty",
        "clean-sample-with-scope-tokens.R")), sep = "\n")
    Output
      Styling  1  files:
       clean-sample-with-scope-tokens.R v 
      ----------------------------------------
      Status	Count	Legend 
      v 	1	File unchanged.
      i 	0	File changed.
      x 	0	Styling threw an error.
      ----------------------------------------

---

    Code
      cat(catch_style_file_output(file.path("public-api", "xyzdir-dirty",
        "dirty-sample-with-scope-spaces.R")), sep = "\n")
    Output
      Styling  1  files:
       dirty-sample-with-scope-spaces.R i 
      ----------------------------------------
      Status	Count	Legend 
      v 	0	File unchanged.
      i 	1	File changed.
      x 	0	Styling threw an error.
      ----------------------------------------
      Please review the changes carefully!

# No sensitive to decimal option

    Code
      style_text("1")
    Output
      1

