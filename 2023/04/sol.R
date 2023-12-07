base::load("common_functions.Rdata")

day_folder <- "2023/04"

parser <- function(x) {
    tmp <- strsplit(x, ":|\\|")
    lapply(tmp, function(x){
        list(
            winners = as.integer(regexprmatch(x[2], "[0-9]+")[[1]]),
            nums = as.integer(regexprmatch(x[3], "[0-9]+")[[1]]) 
        )
    })
}

input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(parser))
}

calc_num_winners <- function(winners, nums) {
    sum(nums %in% winners)
}
score_card <- function(winners, nums) {
    num_winners <- calc_num_winners(winners, nums)
    if (num_winners == 0) return (0)
    2^(num_winners - 1)
}

score <- function(input, debug = FALSE) {
    scores <- sapply(input, function(x) do.call(score_card, args = x))
    if (debug) return(scores)
    sum(scores)
}

num_cards <- function(input, debug = FALSE) {
    cards <- rep(1, length(input))

    for (i in seq_along(cards)) {
        num_winners <- do.call(calc_num_winners, args = input[[i]])
        if (num_winners > 0) {
            cards[i + seq_len(num_winners)] <- cards[i + seq_len(num_winners)] + cards[i]
        }
    }

    if (debug) return(cards)

    sum(cards)
}
test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  score,
  test_input[[1]],
  13,
  label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    num_cards,
    test_input[[1]],
    30,
    label = "Test 2"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

result <- score(input[[1]])

cli::cli_alert_info("The Part 1 answer is {result}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

result <- num_cards(input[[1]])

cli::cli_alert_info("The Part 2 answer is {result}")
cat("\n")