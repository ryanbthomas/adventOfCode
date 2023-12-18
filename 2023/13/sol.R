base::load("common_functions.Rdata")
library(collections)
day_folder <- "2023/13"

parser <- function(x) {
    tmp <- strsplit(x, "")
    ncol = length(tmp[[1]])
    matrix(unlist(tmp), ncol = ncol, byrow = TRUE)
}

input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(parser))
}

print_note <- function(note) {
    for (i in seq_len(nrow(note))) {
        cat(note[i, ], "\n", sep = "")
    }
    invisible(note)
}

encode <- function(note) {
    enc <- c("#" = 1, "." = 0)
    pows <- 2^(seq(ncol(note) - 1, 0))

    vapply(seq_len(nrow(note)), \(x) sum(enc[note[x,]]*pows), FUN.VALUE= 0)

}

count_vert_reflections <- function(code, debug = FALSE) {
    s <- stack()
    s2 <- stack()
    max_stack_size <- 0
    while(length(code) > 0 && (max_stack_size == 0 || s$size() > 0)) {
        if (s$size() == 0) {
            if (debug) cli::cli_alert_info("stack is empty, adding{code[1]}")
            s$push(code[1])
            max_stack_size <- max_stack_size + 1
            code <- code[-1]
            next
        }

        if (s$peek() != code[1]) {
            if (debug) cli::cli_alert_info("found new item {code[1]}")

            if (s2$size() > 0) {
                # we've been tracking a potentail reflectiong that isn't going to work
                pot_reflection <- unlist(s2$as_list())
                tmp <- c(pot_reflection, rev(pot_reflection))
                for (i in tmp) {
                    s$push(i)
                }
                max_stack_size <- max_stack_size + length(pot_reflection)
                s2$clear()
            }
            #if (max_stack_size > s$size()) {
            #    if (debug) cli::cli_alert_info("We've already started popping stack. Return 0")
            #    return(0)
            #}
            s$push(code[1])
            max_stack_size <- max_stack_size + 1
            code <- code[-1]
            next
        }

        a <- s$pop()
        s2$push(a)
        code <- code[-1]

    }

    if (s$size() == max_stack_size) return(0)

    max_stack_size
}

summarize_reflections <- function(note) {
    code <- encode(note)
    vert_reflections <- count_vert_reflections(code)
    if (vert_reflections > 0) {
        return(100 * vert_reflections)
    }

    # transpose to get vert reflections
    code <- encode(t(note))
    count_vert_reflections(code)
}


test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  summarize_reflections,
  test_input[[1]],
  5,
  label = "Test 1"
)

test_example(
  summarize_reflections,
  test_input[[2]],
  400,
  label = "Test 2"
)

test_example(
    count_vert_reflections,
    c(1, 2, 3, 4, 5),
    0,
    label = "Test 3: no relections"
)
test_example(
    count_vert_reflections,
    c(1, 2, 3, 3, 2),
    3,
    label = "Test 4: extra rows at the front"
)
test_example(
    count_vert_reflections,
    c(1, 2, 2, 1, 3),
    2,
    label = "Test 5: extra rows in the back"
)

test_example(
    count_vert_reflections,
    c(1, 2, 3, 3, 2, 1),
    3,
    label = "Test 7: divide in half"
)
test_example(
    count_vert_reflections,
    c(1, 2, 3, 3, 2, 4),
    0,
    label = "Test 8: Reversal"
)
test_example(
    count_vert_reflections,
    c(1, 2, 3, 4, 5, 6, 6, 5),
    6,
    label = "Test 9: Reflection at the end"
)

test_example(
    count_vert_reflections,
    c(1, 2, 3, 3, 5, 6, 6, 5),
    6,
    label = "Test 10: Fake reversal followed by real reversal"
)

cat(cli::col_green("Part 2 Test Cases"), "\n")


cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

note_summary <- sum(vapply(input, summarize_reflections, FUN.VALUE = 0))
# tried 62076 too HIGH
# 26650 on correct
cli::cli_alert_info("The Part 1 answer is {note_summary}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")


cli::cli_alert_info("The Part 2 answer is ")
cat("\n")