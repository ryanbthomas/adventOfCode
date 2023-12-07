base::load("common_functions.Rdata")

day_folder <- "2021/07"


input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(comma_separated_line))
}
#test_input <- input_parser("example")

find_cheapest_position <- function(init) {
    init <- sort(init)
    idx <- ifelse(length(init) %% 2 == 0, length(init) %/% 2, length(init) %/% 2 + 1)
    init[idx]
}
fuel_calc1 <- function(init, pos) {
    sum(abs(init- pos))
}

find_cheapest_position2 <- function(init) {
    # big idea is that the cost is an increasing some
    # I analytically solved this problem
    m <- mean(init)
    k <- sum(init <= m)
    n <- length(init)
    round(m - k / n + 0.5)
}
fuel_calc2 <- function(init, pos) {
    x <- abs(init - pos)
    sum(x * (x + 1) * 0.5)
}


# calc_cheapest_fuel <- function(init) {
#     x_bar <- mean(init)
#     x_sq_bar <- mean(init^2)
#     a <- 3
#     b <- -(6 *x_bar + 2)
#     c <- 3 * x_sq_bar + 2 * x_bar
#     descrim <- sqrt(b^2 - 4 * a * c +0i)
#     sol <- c((-b + descrim) / (2*a), (-b - descrim) / (2*a))
#     fuel <- c(fuel_calc(init, sol[1]), fuel_calc(init, sol[2]))

#     if (fuel[1] <= fuel[2]) {
#         return( list(sol[1], fuel[1]))
#     }
#     list(sol[2], fuel[2])
# }

cat(cli::col_green("Part 1 Test Cases"), "\n")

test_input <- as.integer(c(16, 1, 2, 0, 4, 2, 7, 1, 2, 14))


test_example(
  find_cheapest_position,
  test_input,
  2L,
  label = "Test 1"
)

test_example(
    function(x) fuel_calc1(x, 2),
    test_input,
    37,
    is_same = function(x, y) abs(x - y) < 0.000001,
    label = "Test 2"
)


cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    find_cheapest_position2,
    test_input,
    5,
    label = "Test 3"
)

test_example(
    function(x) fuel_calc2(x, 5),
    test_input,
    168,
    label = "Test 4"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

cheap_pos <- find_cheapest_position(input[[1]])
fuel_cost <- fuel_calc1(input[[1]], cheap_pos)

cli::cli_alert_info("The Part 1 answer is {fuel_cost} @ position {cheap_pos}")
cli::cli_alert_info("Let's try it with optimization")

f <- function(x) fuel_calc1(input[[1]], x)
opt <- optim(median(input[[1]]), f, method = "Brent", lower = min(input[[1]]), upper = max(input[[1]]))

cheap_pos <- round(opt$par)
fuel_cost <- fuel_calc1(input[[1]], cheap_pos)

cli::cli_alert_info("The Part 1 (w/ optim) answer is {fuel_cost} @ position {cheap_pos}")

cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

cheap_pos <- find_cheapest_position2(input[[1]])
fuel_cost <- fuel_calc2(input[[1]], cheap_pos)

cli::cli_alert_info("The Part 2 answer is {fuel_cost} @ position {cheap_pos}")

cli::cli_alert_info("Let's try it with optimization")

f <- function(x) fuel_calc2(input[[1]], x)
opt <- optim(mean(input[[1]]), f, method = "Brent", lower = min(input[[1]]), upper = max(input[[1]]))

cheap_pos <- round(opt$par)
fuel_cost <- fuel_calc1(input[[1]], cheap_pos)

cli::cli_alert_info("The Part 2 (w/ optim) answer is {fuel_cost} @ position {cheap_pos}")
cat("\n")