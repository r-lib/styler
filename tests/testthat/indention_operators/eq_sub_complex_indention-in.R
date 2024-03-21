call(a =
       5,
     b)

call(a =
       5,
     b
     )

# multiple nested levels
{
  v <- function(x =
  122,
  y) {
       }
}


{
        v <- function(x = 122,
                      y) {
        }
}

MyClass <- R6::R6Class(
        "MyClass",
        public = list(initialize = function(my_arg,
                                            my_named_arg = 1) {
                return(invisible())
        }
        ),
)
