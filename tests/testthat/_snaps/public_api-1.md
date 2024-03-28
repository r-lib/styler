# messages (via cat()) of style_file are correct

    Code
      cat(catch_style_file_output(file.path("public-api", "xyzdir-dirty",
        "dirty-sample-with-scope-tokens.R")), sep = "\n")
    Message
      Styling 1 file:
    Output
       'dirty-sample-with-scope-tokens.R' i 
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
    Message
      Styling 1 file:
    Output
       'clean-sample-with-scope-tokens.R' v 
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
    Message
      Styling 1 file:
    Output
       'dirty-sample-with-scope-spaces.R' i 
      ----------------------------------------
      Status	Count	Legend 
      v 	0	File unchanged.
      i 	1	File changed.
      x 	0	Styling threw an error.
      ----------------------------------------
      Please review the changes carefully!
