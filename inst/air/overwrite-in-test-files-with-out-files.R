# List all input files recursively that end with "-in.R"
input_files <- list.files(
  path = "tests/testthat",
  pattern = "-in\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

# Function to generate output filename by replacing -in.R with -out.R
get_output_filename <- function(input_file) {
  gsub("-in\\.R$", "-out.R", input_file)
}

# Process each file
for (input_file in input_files) {
  output_file <- get_output_filename(input_file)

  # Create directory for output file if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Read input file and write to output file
  file.copy(input_file, output_file, overwrite = TRUE)

  # Print status
  cat(sprintf("Processed: %s -> %s\n", input_file, output_file))
}

cat(sprintf("\nProcessed %d files\n", length(input_files)))
