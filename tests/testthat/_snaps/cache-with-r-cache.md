# cached expressions are displayed propperly

    Code
      cache_info[, c("n", "size", "last_modified", "activated")]
    Output
        n size last_modified activated
      1 0    0          -Inf     FALSE

---

    Code
      cache_info[, c("n", "size", "activated")]
    Output
        n size activated
      1 1    0      TRUE

---

    Code
      cache_info[, c("n", "size", "activated")]
    Output
        n size activated
      1 2    0      TRUE

