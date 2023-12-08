base::load("common_functions.Rdata")

day_folder <- "2023/08"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
}



test_input <- input_parser("example")

cat(cli::col_green("Part 1 Test Cases"), "\n")


#test_example(
#   ,
#   test_input,
#   c(15,10),
#   label = "Test 1"
#)
cat(cli::col_green("Part 2 Test Cases"), "\n")


cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
#input <- input_parser("input")


cli::cli_alert_info("The Part 1 answer is ")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")


cli::cli_alert_info("The Part 2 answer is ")
cat("\n")