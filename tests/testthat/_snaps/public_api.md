# messages (via cat()) of style_file are correct

    Code
      catch_style_file_output(file.path("public-api", "xyzdir-dirty",
        "dirty-sample-with-scope-tokens.R"))
    Output
      [1] "Styling  1  files:"                      
      [2] " dirty-sample-with-scope-tokens.R i "    
      [3] "----------------------------------------"
      [4] "Status\tCount\tLegend "                  
      [5] "v \t0\tFile unchanged."                  
      [6] "i \t1\tFile changed."                    
      [7] "x \t0\tStyling threw an error."          
      [8] "----------------------------------------"
      [9] "Please review the changes carefully!"    

---

    Code
      catch_style_file_output(file.path("public-api", "xyzdir-dirty",
        "clean-sample-with-scope-tokens.R"))
    Output
      [1] "Styling  1  files:"                      
      [2] " clean-sample-with-scope-tokens.R v "    
      [3] "----------------------------------------"
      [4] "Status\tCount\tLegend "                  
      [5] "v \t1\tFile unchanged."                  
      [6] "i \t0\tFile changed."                    
      [7] "x \t0\tStyling threw an error."          
      [8] "----------------------------------------"

---

    Code
      catch_style_file_output(file.path("public-api", "xyzdir-dirty",
        "dirty-sample-with-scope-spaces.R"))
    Output
      [1] "Styling  1  files:"                      
      [2] " dirty-sample-with-scope-spaces.R i "    
      [3] "----------------------------------------"
      [4] "Status\tCount\tLegend "                  
      [5] "v \t0\tFile unchanged."                  
      [6] "i \t1\tFile changed."                    
      [7] "x \t0\tStyling threw an error."          
      [8] "----------------------------------------"
      [9] "Please review the changes carefully!"    

