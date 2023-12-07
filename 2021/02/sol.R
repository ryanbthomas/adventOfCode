
source("R/input_utils.R")
source("R/testing_utils.R")
source("R/utils.R")

day_folder <- "2021/02"

extract_sublist <- function(l, id) {
    unlist(lapply(l, function(x) x[[id]]))
}

load_commands <- function(x) {

    tmp <- strsplit(x, split = " ")
    directions <- extract_sublist(tmp, 1L)
    units <- as.integer(extract_sublist(tmp, 2L))       
   
    list(directions = directions, units = units)
}

move_sub_part1 <- function(start, direction, unit) {
    if (direction == "forward") {
        return(c(start[1] + unit, start[2]))
    }
    sign <- ifelse(direction == "up", -1, 1)

    c(start[1], start[2] + sign * unit)
}

move_to_final_pos <- function(move, start_pos, directions, units, verbose = FALSE) {
    for(i in seq_along(directions)) {
        new_pos <- move(start_pos, directions[i], units[i])
        if (verbose) {
            pt0 <- pretty_print_vector(start_pos)
            pt1 <- pretty_print_vector(new_pos)
            cli::cli_alert_info(
                "Move sub from {pt0} {directions[i]} {units[i]} units -> {pt1}"
                )
        }
        start_pos <- new_pos
    }
    new_pos
}

move_sub_part2 <- function(start, direction, unit) {
    if (direction == "forward") {
        return(c(start[1] + unit, start[2] + start[3] * unit, start[3]))
    }
    sign <- ifelse(direction == "up", -1, 1)

    c(start[1], start[2], start[3] + sign * unit)
}

test_input <- read_input(file.path(day_folder, "example"), list(load_commands))[[1]]

cat(cli::col_green("Part 1 Test Cases"), "\n")

test_example(
    function(x) move_to_final_pos(move_sub_part1, c(0, 0), x$directions, x$units), 
    test_input,
    c(15,10), 
    label = "Test 1"
)

cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    function(x) move_to_final_pos(move_sub_part2, c(0, 0, 0), x$directions, x$units),
    test_input,
    c(15, 60, 10),
    label = "Test 2"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")

input <- read_input(file.path(day_folder, "input"), list(load_commands))[[1]]

final_position <- move_to_final_pos(
    move_sub_part1, 
    c(0, 0), 
    directions = input$directions,
    units = input$units
)
pt <- pretty_print_vector(final_position)
cli::cli_alert_info("The final position is {pt}")
cli::cli_alert_info("The Part 1 answer is {final_position[1]} * {final_position[2]} = {prod(final_position)}")

cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")
final_position <- move_to_final_pos(
    move_sub_part2, 
    c(0, 0, 0), 
    directions = input$directions,
    units = input$units
)
pt <- pretty_print_vector(final_position)
cli::cli_alert_info("The final position is {pt}")
cli::cli_alert_info("The Part 1 answer is {final_position[1]} * {final_position[2]} = {prod(final_position[-3])}")

cat("\n")