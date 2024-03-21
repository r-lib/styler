x <-
  2


x <- 3

# FIXME: edge case not working for R < 3.6: Problem: most likely, comment is
# not moved to the right nest with relocate_eq_assign.
x <-
  # the culprit

  3


x = #
  2


x = 3

x =

  # comment
  3



ImportantDataFrame$ImportantColumn1 <-
  ImportantDataFrame$ImportantColumn2 <-
  ComplicatedFunction(ImportantDataFrame$InputColumn)


ImportantDataFrame$ImportantColumn1 <-
  ImportantDataFrame$ImportantColumn2 <- ComplicatedFunction(ImportantDataFrame$InputColumn)



ImportantDataFrame$ImportantColumn1 <-

  ImportantDataFrame$ImportantColumn2 <- ComplicatedFunction(ImportantDataFrame$InputColumn)
