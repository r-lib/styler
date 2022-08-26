# You can modify the PR comment header here. You can use github markdown e.g.
# emojis like :tada:.

glue::glue(
  "The image below represents the test results generated",
    " after running `",
    test_function,
    "` function",
    " on this PR branch.",
    "Check out the test results data set in csv ",
    csv_url,
    "\n",
    "<br/>",
    image_url
)
