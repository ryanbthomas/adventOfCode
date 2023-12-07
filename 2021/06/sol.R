base::load("common_functions.Rdata")

day_folder <- "2021/06"


input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(comma_separated_line))
}


run_experiment <- function(school_init, days = 10) {
    nursery <- rep(0, 9)
    school <- rep(0, 7)
    idx <- table(school_init)
    school[as.integer(names(idx))+1] <- idx
    #print(paste0("Nursery: ", pretty_print_vector(nursery)))
    #print(paste0("School: ", pretty_print_vector(school)))
    for (i in seq_len(days)){
        new_fish_from_school <- school[1]
        school <- school[-1]
        school <- append(school, new_fish_from_school)
        new_kids_from_nursery <- nursery[1]
        nursery <- nursery[-1]
        nursery <- append(nursery, new_kids_from_nursery + new_fish_from_school)
        school[length(school)] <- school[length(school)] + new_kids_from_nursery
        #print(paste0("Nursery: ", pretty_print_vector(nursery)))

        #print(paste0("School: ", pretty_print_vector(school)))

    }
    sum(school) + sum(nursery)
}


cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
  function(x) run_experiment(c(3, 4, 3, 1, 2), days = x),
  18,
  26,
  label = "Test 1 Days: 18"
)

test_example(
  function(x) run_experiment(c(3, 4, 3, 1, 2), days = x),
  80,
  5934,
  label = "Test 2 Days: 80"
)

cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
  function(x) run_experiment(c(3, 4, 3, 1, 2), days = x),
  256,
  26984457539,
  label = "Test 3 Days: 256"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

result <- run_experiment(input[[1]], days = 80)

cli::cli_alert_info("The Part 1 answer is {result}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

result <- run_experiment(input[[1]], days = 256)

cli::cli_alert_info("The Part 2 answer is {result}")

cat("\n")