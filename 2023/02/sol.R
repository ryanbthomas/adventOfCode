base::load("common_functions.Rdata")

day_folder <- "2023/02"

create_game_views <- function(gv) {
    game_views <- vector("list", length(gv))
    for (i in seq_along(game_views)) {
        tmp <- c(red = 0, green = 0, blue = 0)
        nums <- regexprmatch(gv[i], pattern = "[0-9]+")[[1]]
        colors <- regexprmatch(gv[i], pattern = "[a-z]+")[[1]]
        tmp[colors] <- as.integer(nums)
        game_views[[i]] <- tmp
    }
    game_views
}
parser <- function(x) {
    tmp <- strsplit(x, split = ":|;")
    games <- vector("list", length(tmp))

    for (i in seq_along(tmp)) {
        
        games[[i]] <- list(
            id = as.integer(regexprmatch(tmp[[i]][1], "[0-9]+")),
            views = create_game_views(tmp[[i]][-1])
        )
               
    }
    games
}

validate_game <- function(game, condition = c(red = 12L, green = 13L, blue = 14L)){

    for (v in game[["views"]]) {
       if (any(v - condition > 0)) return(0L) 
    }

    game[["id"]]
}

validate <- function(games) {
    
    sum(vapply(games, validate_game, 0L))

}

game_power <- function(game) {
    prod(do.call(pmax, args = game[["views"]]))
}


input_parser <- function(file) {
    read_input(file.path(day_folder, file), list(parser))
}
test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
   validate,
   test_input[[1]],
   8L,
   label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    function(x) sum(vapply(x, game_power, 0)),
    test_input[[1]],
    2286,
    label = "Test 2"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

result <- validate(input[[1]])

cli::cli_alert_info("The Part 1 answer is {result}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

result <- sum(vapply(input[[1]], game_power, 0))

cli::cli_alert_info("The Part 2 answer is {result}")
cat("\n")