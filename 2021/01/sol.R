source("R/input_utils.R")
source("R/testing_utils.R")
find_depth_increases <- function(measurements) {
    sum(diff(measurements) > 0)
}

create_rolling_window <- function(measurements) {
    #measurements <- measurements[-length(measurements)]
    num_measurements <- length(measurements)
    idx1 <- num_measurements - 0:1
    idx2 <- c(1, num_measurements)
    idx3 <- 1:2
    measurements[-idx1] + measurements[-idx2] + measurements[-idx3]
}

cat(cli::col_green("Part 1 Test Cases"), "\n")

test_input <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

test_example(find_depth_increases, test_input, 7L, "Test 1")

cat(cli::col_green("Part 2 Test Cases"), "\n")

rolling_avg <- test_example(
    create_rolling_window, 
    test_input, 
    c(607, 618, 618, 617, 647, 716, 769, 792),
    "Test 2"
)

test_example(find_depth_increases, rolling_avg, 5L, "Test 3")

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")

input <- read_input("2021/01/input", list(as.integer))

depth_changes <- find_depth_increases(input[[1]])
cli::cli_alert_info("The number of depth changes is {depth_changes}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

depth_changes <- find_depth_increases(create_rolling_window(input[[1]]))

cli::cli_alert_info("The number of depth changes is {depth_changes}")
cat("\n")