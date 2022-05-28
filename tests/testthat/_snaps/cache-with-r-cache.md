# cached expressions are displayed propperly

    Code
      cache_info[, c("n", "size", "last_modified", "activated")]
    Output
      # A tibble: 1 x 4
            n  size last_modified activated
        <int> <dbl> <dttm>        <lgl>    
      1     0     0 -Inf -Inf     FALSE    

---

    Code
      cache_info[, c("n", "size", "activated")]
    Output
      # A tibble: 1 x 3
            n  size activated
        <int> <dbl> <lgl>    
      1     1     0 TRUE     

---

    Code
      cache_info[, c("n", "size", "activated")]
    Output
      # A tibble: 1 x 3
            n  size activated
        <int> <dbl> <lgl>    
      1     2     0 TRUE     

