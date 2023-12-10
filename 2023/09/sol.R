base::load("common_functions.Rdata")

day_folder <- "2023/09"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    lapply(strsplit(lines, " "), as.integer)
}

extrapolate <- function(x) {
    d <- diff(x)

    if (all(d == 0)) {
        return(x[length(x)])
    }
    
    return(extrapolate(d) + x[length(x)])
}

extrapolate_rev <- function(x) {
    extrapolate(rev(x))
}

extrapolate_and_sum <- function(input, f = extrapolate) {
    e <- vapply(input, f, FUN.VALUE = 0L)
    sum(e)
}

test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  extrapolate_and_sum,
  test_input,
  114L,
  label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
  \(x) extrapolate_and_sum(x, f = extrapolate_rev),
  test_input,
  2L,
  label = "Test 2 - backward"
)



cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

extra_sum <- extrapolate_and_sum(input)

cli::cli_alert_info("The Part 1 answer is {extra_sum}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

extra_sum_backward <- extrapolate_and_sum(input, f = extrapolate_rev)

cli::cli_alert_info("The Part 2 answer is {extra_sum_backward}")
cat("\n")