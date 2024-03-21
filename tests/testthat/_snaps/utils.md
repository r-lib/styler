# files with and without blank EOF line are read correctly

    Code
      read_utf8(test_path("reference-objects/missing-blank-at-EOF.R"))
    Output
      $text
      [1] "x"
      
      $missing_EOF_line_break
      [1] TRUE
      

---

    Code
      read_utf8(test_path("reference-objects/non-missing-blank-at-EOF.R"))
    Output
      $text
      [1] "x"
      
      $missing_EOF_line_break
      [1] FALSE
      

