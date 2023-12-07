source("R/input_utils.R")
source("R/testing_utils.R")
source("R/utils.R")

day_folder <- "2021/04"

library(R6)
bingocard <- R6Class("bingocard", list(
    board = NA_integer_,
    marked = NA,
    last_mark = NA_integer_,
    is_bingo = FALSE,
    initialize = function(mat) {
        self$board <- mat
        self$marked <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))

    },
    mark = function(num) {
        idx <- self$board[] == num
        if (any(idx)) {
            self$last_mark <- num
            self$marked <- self$marked | idx
            row_bingo <- any(colSums(self$marked) == ncol(self$board)) 
            col_bingo <- any(rowSums(self$marked) == nrow(self$board))
            self$is_bingo <- row_bingo || col_bingo
        }
    },
    score = function() {
        tmp <- self$board
        tmp[self$marked] <- 0
        sum(tmp) * self$last_mark
    },
    print = function(...) {
        cat("BINGOCARD:\n")
        num_dec_places <- max(ceiling(log10(self$board)))
        str_format <- paste0("%", num_dec_places, "d")
        for (i in seq_len(nrow(self$board))) {
            cat(sprintf(str_format, self$board[i, ]), "\n")
        }
        cat("\n")
        invisible(self)

    }
))

# new_bingocard <- function(mat) {
#     structure(
#         list(
#             board = mat,
#             marked = matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat)),
#             last_mark = NA_integer_,
#             is_bingo = FALSE
#         ),
#         class = "bingocard"
#     )
# }

# print.bingocard <- function(x, ...){
#     cat("BINGOCARD:\n")
#     board <- x$mat
#     num_dec_places <- max(ceiling(log10(as.vector(board)))
    
#     str_format <- paste0("%", num_dec_places, "d")
#     for (i in seq_len(nrow(board))) {
#         cat(sprintf(str_format, x[i, ]), "\n")
#     }
#     cat("\n")
#     invisible(x)
# }

# mark <- function(bc, num) {
#     idx <- bc$board == num
#     if (any(idx)) {

#     }
# }

play_game <- function(calls, boards) {
    for (num in calls) {
        for (brd_idx in seq_along(boards)) {
            boards[[brd_idx]]$mark(num)
            if (boards[[brd_idx]]$is_bingo) {
                cli::cli_alert_success("Game Board {brd_idx} is a BINGO!\n")
                score <- boards[[brd_idx]]$score()
                cli::cli_alert_success("Your score is {score}")
                return(invisible(list(brd_idx, score)))
            }
        }
    }
}

play_game_to_lose <- function(calls, boards) {
    remaining_boards <- rep(TRUE, length(boards))

    for (num in calls) {
        for (brd_idx in seq_along(boards)) {
            if (boards[[brd_idx]]$is_bingo) {
                next()
            }
            boards[[brd_idx]]$mark(num)
            if (boards[[brd_idx]]$is_bingo) {
                remaining_boards[brd_idx] <- FALSE
            }
            score <- boards[[brd_idx]]$score()
            if (!any(remaining_boards)) {
                return(invisible(score))
            }
        }
    }
}

load_bingocards <- function(x) {
    bingocard$new(space_separated_matrix(x))
}   

input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(comma_separated_line, load_bingocards))
}
test_input <- input_parser("example_input")

cat(cli::col_green("Part 1 Test Cases"), "\n")

test_example(
   function(x) play_game(x[[1]], x[-1]),
   test_input,
   list(3, 4512),
   label = "Test 1 Playing to Win"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_input <- input_parser("example_input")
test_example(
    function(x) play_game_to_lose(x[[1]], x[-1]),
    test_input,
    1924,
    label = "Test 2 Playing to Lose"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

result <- play_game(input[[1]], input[-1])

cli::cli_alert_info("The Part 1 answer is {result[[2]]}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

input <- input_parser("input")
result <- play_game_to_lose(input[[1]], input[-1])

cli::cli_alert_info("The Part 2 answer is {result}")
cat("\n")
