base::load("common_functions.Rdata")

day_folder <- "2023/08"

input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))

    turns <- strsplit(lines[1], "")[[1]]

    raw_network <- regexprmatch(lines[-c(1,2)], "[A-Z]{3}")

    network <- lapply(raw_network, \(x) setNames(x[-1], c("L", "R")))

    keys <- vapply(raw_network, \(x) x[1], FUN.VALUE = "")
    
    names(network) <- keys

    list(turns = turns, network = network)
}

navigate <- function(input, node = "AAA", step_count = 0L, stop_cond = \(x) x == "ZZZ") {
    turns <- input$turns
    network <- input$network

    while(!all(stop_cond(node))) {
        #node <- vapply(node, \(x) network[[x]][turns[step_count %% length(turns) + 1L]], FUN.VALUE = "")
        node <- network[[node]][turns[step_count %% length(turns) + 1L]]
        
        step_count <- step_count + 1L
    }

    list(step_count, node)
}

find_all_start_nodes <- function(keys) {
    idx <- grepl(pattern = "[A-Z]{2}A", keys)
    keys[idx]
}

ghost_stop <- function(x) {
    grepl(pattern = "[A-Z]{2}Z", x)
}

find_network_cycle <- function(input, node) {
    tmp <- navigate(input, node = node, stop_cond = ghost_stop)

    term_nodes <- NULL
    new_term <- NULL
    network <- input$network
    turns <- input$turns
    num_turns <- length(turns)

    stop_cond <- function(term_nodes) {
        if (is.null(term_nodes)) return(FALSE)
        #cli::cli_alert_info("Here is the term_nodes")
        #print(term_nodes)
        nodes <- vapply(term_nodes, \(x) x[[2]], FUN.VALUE = "")
        steps <- vapply(term_nodes, \(x) x[[1]], FUN.VALUE = 0L)

        tbl <- table(nodes)

        if (max(tbl) == 1) return(FALSE) 

        for (check_node in names(tbl)[tbl > 1]) {
            idx <- nodes %in% check_node
            check <- steps[idx] %% num_turns

            if ( max(table(check)) > 0) return(TRUE)
        }

        FALSE 
    }
    step_count <- 0L
    while(!stop_cond(term_nodes)) {
        tmp <- navigate(input, node = node, step_count = step_count, stop_cond = ghost_stop)

        if (is.null(term_nodes)) {
            term_nodes <- list(tmp)
        } else {
            term_nodes <- append(term_nodes, list(tmp))
        }
        cli::cli_alert_info("found exit node {tmp}")
        node <- network[[tmp[[2]]]][turns[tmp[[1]] %% length(turns) + 1L]]
        step_count <- tmp[[1]] + 1L
    }
    #term_nodes
    vapply(term_nodes[-length(term_nodes)], \(x) x[[1]], FUN.VALUE = 0L)
}
prime_factorization <- function(x) {
    p <- unique(c(2L, seq.int(from = 3L, to = floor(sqrt(x)), by = 2L), as.integer(floor(sqrt(x)))))
    
    n <- x
    x_factors <- c()

    while(length(p) > 0 && n >= p[1]) {
        if (n %% p[1] == 0) {
            x_factors <- append(x_factors, p[1])
            n <- n %/% p[1]
        } else {
            p <- p[-1]
        }
        
    }
    append(x_factors, n)

}
test_input01 <- input_parser("example01")
test_input02 <- input_parser("example02")
test_input03 <- input_parser("example03")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  navigate,
  test_input01,
  list(2L, "ZZZ"),
  is_same = \(a, e) isTRUE(all.equal(a, e, check.attributes = FALSE)),
  label = "Test 1 - two steps"
)

test_example(
  navigate,
  test_input02,
  list(6L, "ZZZ"),
  is_same = \(a, e) isTRUE(all.equal(a, e, check.attributes = FALSE)),
  label = "Test 2 - six steps"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

# test_example(
#   \(x) navigate(x, node = find_all_start_nodes(names(x$network)), stop_cond = ghost_stop),
#   test_input03,
#   6L,
#   label = "Test 3 - Ghost Stop"
# )

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

step_count <- navigate(input)

cli::cli_alert_info("The Part 1 answer is {step_count}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

start_nodes <- find_all_start_nodes(names(input$network))

cycles <- vector("list", length(start_nodes))

for (i in seq_along(cycles)) {
    cycles[[i]] <- find_network_cycle(input, start_nodes[i])
}
# all are vectors of length 1
#print(cycles)
cycles <- unlist(cycles)

all_factors <- lapply(cycles, prime_factorization)
gcd <- as.integer(Reduce(intersect, all_factors))

tmp <- vapply(all_factors, \(x) setdiff(as.integer(x), gcd), FUN.VALUE = 0L)
lcm <- prod(bit64::as.integer64(c(tmp, gcd)))

cli::cli_alert_info("The Part 2 answer is {lcm}")
cat("\n")