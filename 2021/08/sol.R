base::load("common_functions.Rdata")

library(collections)
day_folder <- "2021/08"

parser <- function(x) {
    lapply(strsplit(x, split = " "), function(x) list(x[1:10], x[-c(1:11)]))
}

input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(parser))
}
test_input <- input_parser("example")

find_unique_digits <- function(input) {
    uniqs <- collections::dict()
    cnt <- 0
    uniq_map <- collections::dict(c(1, 7, 4, 8), c(2, 3, 4, 7))

    for (s in input) {
        num <- nchar(s)
        if (num %in% c(2, 4, 3, 7)) {
            cnt <- cnt + 1
            lst <- strsplit(s, "")[[1]]
            lst <- sort(lst)
            uniqs[uniq_map$get(num)] = 
        }
    }
}

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