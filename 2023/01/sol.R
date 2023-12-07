base::load("common_functions.Rdata")

day_folder <- "2023/01"

parser <- function(x) {
    strsplit(x, "")
    
}

parser2 <- function(x) {
    x
}

input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(parser))
}

input_parser2<- function(file) {
    read_input(file.path(day_folder, file), list(parser2))
}

test_input <- input_parser("example")
test_input2 <- input_parser2("example")

find_first_digit <- function(x) {
    for(i in seq_along(x)) {
        if (grepl('[0-9]', x[i])) return(x[i])
    }
}

find_digits <- function(x, pattern = "[0-9]") {
    tmp <- regmatches(x, gregexec(pattern, x, perl = TRUE))
    num_str <- lapply(tmp, function(x) paste0(x[1], x[length(x)]))
    as.integer(unlist(num_str))
}

map_spelling_to_digit <- function(x, direction = "front") {
    if (direction == "back") {
        digit_map <- append(
            digit_map, 
            list(
                "eightwo" = "2",
                "eighthree" = "3",
                "twone" = "1",
                "eightwone" = "1",
                "sevenine" = "9",
                "fiveight" = "8",
                "threeight" = "8",
                "nineight" = "8",
                
                "fiveightwo" = "2",
                "threeightwo" = "2",
                "nineightwo" = "2",
                
                "fiveightwone" = "1",
                "threeightwone" = "1",
                "nineightwone" = "1",
                
                "fiveighthree" = "3",
                "threeighthree" = "3",
                "nineighthree" = "3",

            ))
    }else{
        digit_map <- append(
            digit_map, 
            list(
                "eightwo" = "8",
                "eighthree" = "8",
                "twone" = "2",
                "eightwone" = "8",
                "sevenine" = "7"
            ))

    }
    val <- digit_map[[x]]
    if (is.null(val)) return(x)
    val
}
find_first_digits_with_spelling <- function(x, direction = "forward", debug = FALSE) {
    pattern <- "one|two|three|four|five|six|seven|eight|nine"

    digit_map <- list("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5", "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9")

    if (direction == "backward") {
        pattern <- stringi::stri_reverse(pattern)
        x <- stringi::stri_reverse(x)
        digit_map <- setNames(digit_map, stringi::stri_reverse(names(digit_map)))
    }

    tmp <- regmatches(x, gregexpr(paste0(pattern, "|[1-9]"), x))
    if (debug) {
        return(tmp)
    }
    num_str <- lapply(
        tmp, 
        function(x) {
            y <- digit_map[[x[1]]]
            if (is.null(y)) return(x[[1]])
            y
        }
    )
    
    unlist(num_str)
}
find_digits_with_spelling <- function(x) {
    first_digit <- find_first_digits_with_spelling(x)
    last_digit <- find_first_digits_with_spelling(x, direction = "backward")
    as.integer(paste0(first_digit, last_digit))   
}

find_digits_position <- function(x, pattern = "[1-9]") {
    tmp <- gregexpr(pattern = pattern, x)
    
}
make_num_from_first_and_last <- function(x) {
    as.integer(paste0(find_first_digit(x), find_first_digit(rev(x))))
}

cat(cli::col_green("Part 1 Test Cases"), "\n")



test_example(
   function(x) unlist(lapply(x, make_num_from_first_and_last)),
   test_input[[1]],
   c(12L, 38L, 15L, 77L),
   label = "Test Example Method 1"
)

test_example(
   find_digits,
   test_input2[[1]],
   c(12L, 38L, 15L, 77L),
   label = "Test Example Method 2"
)

cat(cli::col_green("Part 2 Test Cases"), "\n")

test_input_example2 <- input_parser2("example02")

test_example(
   find_digits_with_spelling,
   test_input_example2[[1]],
   c(29L, 83L, 13L, 24L, 42L, 14L, 76L),
   label  = "Test Example with spelled out numbers"
)

# Additional solution from Adam Austin via mastodon
# https://github.com/ataustin/advent-of-code/blob/main/y2023/d01/solution.R

decode <- function(input) {
    nums <- gsub("[a-z]", "", input)
    digs <- paste0(substr(nums, 1, 1), substr(nums, nchar(nums), nchar(nums)))
    sum(as.integer(digs))
}

transform_strings <- function(input) {
    digs <- c(twone  = 21, eightwo = 82, oneight = 18, 
        five = 5, nine = 9, eight = 8, two = 2, three = 3, one = 1,
        four = 4, six = 6, seven = 7)

    for (num in names(digs)) {
        input <- gsub(num, digs[num], input)
    }

    input
}

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

cli::cli_alert_info("trying method 1")
start_time <- Sys.time()
nums <- unlist(lapply(input[[1]], make_num_from_first_and_last))
result1 <- sum(nums)
stop_time <- Sys.time()
tictoc <- as.numeric(stop_time - start_time)
cli::cli_alert_success("Elap time: {tictoc} secs")

input2 <- input_parser2("input")
cli::cli_alert_info("trying method 2")
start_time <- Sys.time()
nums <- find_digits(input2[[1]])
result2 <- sum(nums)
stop_time <- Sys.time()
tictoc <- as.numeric(stop_time - start_time)
cli::cli_alert_success("Elap time: {tictoc} secs")

cli::cli_alert_info("trying method 3")
start_time <- Sys.time()
result3 <- decode(input2[[1]])
stop_time <- Sys.time()
tictoc <- as.numeric(stop_time - start_time)
cli::cli_alert_success("Elap time: {tictoc} secs")

cli::cli_alert_info("The Part 1 answer is {result1}")
if (result1 == result2) cli::cli_alert_info("Method 2 and Method 1 match")
if (result2 == result3) cli::cli_alert_info("Method 3 and Method 1 match")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")


cli::cli_alert_info("trying method 1")
start_time <- Sys.time()
nums <- find_digits_with_spelling(input2[[1]])
result <- sum(nums)

stop_time <- Sys.time()
tictoc <- as.numeric(stop_time - start_time)
cli::cli_alert_success("Elap time: {tictoc} secs")

cli::cli_alert_info("trying method 2")

start_time <- Sys.time()
transformed_input <- transform_strings(input2[[1]])
result2 <- decode(transformed_input)

stop_time <- Sys.time()
tictoc <- as.numeric(stop_time - start_time)
cli::cli_alert_success("Elap time: {tictoc} secs")

cli::cli_alert_info("The Part 2 answer is {result}")
if(result == result2) cli::cli_alert_info("Method 1 and Method 2 match")
cat("\n")