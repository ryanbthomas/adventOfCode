source("R/input_utils.R")
source("R/testing_utils.R")
source("R/utils.R")

day_folder <- "2021/03"

load_report <- function(x) {
    num_rows <- length(x)
    num_bits <- nchar(x[1])
    matrix(as.integer(unlist(strsplit(x, ""))), nrow = num_rows, byrow = TRUE)
}
input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(load_report))
}

calc_rates <- function(x) {
    num_rows <- nrow(x)
    on_bit_counts <- colSums(x)
    gamma_rate <- (on_bit_counts >= (num_rows %/% 2)) * 1
    epsilon_rate <- 1 - gamma_rate
    list(gamma_rate = gamma_rate, epsilon_rate = epsilon_rate)
}
bin2dec <- function(x) {
    powers <- 2^seq(from = length(x) - 1, to = 0, by = -1)
    sum(x * powers)
}

find_o2_rating <- function(report, verbose = FALSE) {
    num_rows <- nrow(report)
    if (num_rows == 1) {
        return(report)
    }
    on_bit_counts <- colSums(report)[1]

    bit_val <- as.integer(on_bit_counts >= (num_rows / 2))
    idx <- report[, 1] == bit_val
    c(bit_val, find_o2_rating(report[idx, -1, drop = FALSE]))

}
find_co2_rating <- function(report) {
    num_rows <- nrow(report)
    if (num_rows == 1) {
        return(report)
    }
    on_bit_counts <- colSums(report)[1]
    bit_val <- 1 - as.integer(on_bit_counts >= (num_rows / 2))
    idx <- report[, 1] == bit_val
    c(bit_val, find_co2_rating(report[idx, -1, drop = FALSE]))
    
}
report_part1 <- function(rates) {

    gamma_bin <- pretty_print_vector(rates$gamma_rate)
    epsilon_bin <- pretty_print_vector(rates$epsilon_rate)
    cli::cli_inform("The rates (in binary) are gamma: {gamma_bin}; and epsilon: {epsilon_bin}")

    gamma_dec <- bin2dec(rates$gamma_rate)
    epsilon_dec <- bin2dec(rates$epsilon_rate)
        
    cli::cli_inform("The rates (in decimal) are gamma: {gamma_dec}; and epsilon: {epsilon_dec}")

    cli::cli_inform("Power consumption is {gamma_dec} * {epsilon_dec} = {gamma_dec * epsilon_dec}")

}

test_input <- input_parser("example_input")

cat(cli::col_green("Part 1 Test Cases"), "\n")

rates <- calc_rates(test_input[[1]])
test_example(
   function(x) x$gamma_rate,
   rates,
   c(1, 0, 1, 1, 0),
   label = "Test 1 Gamma Rates in binary"
)
test_example(
   function(x) x$epsilon_rate,
   rates,
   c(0, 1, 0, 0, 1),
   label = "Test 2 Epsilon Rates in binary"
)

test_example(
    bin2dec,
    rates$gamma_rate,
    22,
    label = "Test 3 Gamma Rates in decimal"
)

test_example(
    bin2dec,
    rates$epsilon_rate,
    9,
    label = "Test 4 Epsilon Rates in decimal"
)

report_part1(rates)

cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    function(x) {
        y <- find_o2_rating(x)
        list(y, bin2dec(y))
    },
    test_input[[1]],
    expected = list(c(1, 0, 1, 1, 1), 23),
    label = "Test 5 o2 Rating"
)
test_example(
    function(x) {
        y <- find_co2_rating(x)
        list(y, bin2dec(y))
    },
    test_input[[1]],
    expected = list(c(0, 1, 0, 1, 0), 10),
    label = "Test 6 co2 Rating"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")


cli::cli_alert_info("The Part 1 answer")
report_part1(calc_rates(input[[1]]))

cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

oxygen_rating <- bin2dec(find_o2_rating(input[[1]]))
co2_scrubber_rating <- bin2dec(find_co2_rating(input[[1]]))
life_support_rating <- oxygen_rating * co2_scrubber_rating

cli::cli_alert_info("o2 Rating: {oxygen_rating}")
cli::cli_alert_info("co2 Rating: {co2_scrubber_rating}")
cli::cli_alert_info("\nlife support rating: {life_support_rating}\n")


cli::cli_alert_success("The Part 2 answer is {life_support_rating}")
cat("\n")