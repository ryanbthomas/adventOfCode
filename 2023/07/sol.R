base::load("common_functions.Rdata")

day_folder <- "2023/07"


input_parser <- function(file) {
    lines <- readLines(file.path(day_folder, file))
    tmp <- strsplit(lines, " ") |>
    lapply(function(d) data.frame(hand = d[1], bid = as.integer(d[2])))
    Reduce(rbind, tmp)
}

categorize_hand <- function(hand, rule3 = FALSE) {
    cards <- strsplit(hand, "")[[1]]

    tbl <- table(cards)

    if (length(tbl) == 1) {
        # five of a kind
        return(1L)
    }

    if (length(tbl) == 2 && max(tbl) == 4) {
        # four of a kind
        if (rule3 && 'J' %in% names(tbl)) {
            # if either act is a 'J' we can make it 5 of a kind
            return(1L)
        }
        return(2L)
    }

    if (length(tbl) == 2) {
        # full house
        if (rule3 && 'J' %in% names(tbl)) {
            return(1L)
        }
        return(3L)
    }

    if (length(tbl) == 3 && max(tbl) == 3) {
        if (rule3 && 'J' %in% names(tbl)) {
            # becomes 4 of a kind
            return(2L)
        }
        
        # 3 of a kind
        return(4L)
    }

    if (length(tbl) == 3 && max(tbl) == 2) {
        if (rule3 && 'J' %in% names(tbl)) {
            # can make 4 of a kind if there are 2 Js
            # and a full house there is only 1 J
            return(2L + ifelse(tbl['J'] == 1, 1L, 0L))
        }


        # 2 pair
        return(5L)
    }

    if (length(tbl) == 4) {
        if(rule3 && 'J' %in% names(tbl)) {
            # can make 3 of a kind
            return(4L)
        }
        # 1 pair
        return(6L)
    }

    if (rule3 && 'J' %in% names(tbl)) {
        # can make a pair
        return(6L)
    }
    # high card
    7L
    
        
}


score_hands_by_card <- function(hands, rule3 = FALSE) {
    # base 13
    card_values <- c('2' = 0L, '3' = 1L, '4' = 2L, '5' = 3L, '6' = 4L, '7' = 5L, '8' = 6L, '9' = 7L, 'T' = 8L, 'J' = 9L, 'Q' = 10L, 'K' = 11L, 'A' = 12L)
    if (rule3) {
        names(card_values) <- c('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')
    }
    cards <- strsplit(hands, "")

    score <- function(x) {
        sum(card_values[x] * 13L^(4:0))
    }

    vapply(cards, score, FUN.VALUE = 0)

}
order_hands <- function(input, rule3 = FALSE,  debug = FALSE) {
    input$cats <- vapply(input$hand, categorize_hand, rule3 = rule3, FUN.VALUE= 0L)

    input$scores <- score_hands_by_card(input$hand, rule3 = rule3)
    if(debug) return(input)
    #browser()
    input[order(input$cats, input$scores, decreasing = c(TRUE, FALSE)), ]
    

}

calc_total_winnings <- function(input, rule3 = FALSE, debug = FALSE) {
    if (debug) {
        return(cbind(input, order_hands(input$hand, rule3 = rule3, debug = debug)))
    }
    sorted_input <- order_hands(input, rule3 =  rule3)
    #if(debug) return(input)

    sum(sorted_input$bid * seq_len(nrow(input)))
}
test_input <- input_parser("example01")

cat(cli::col_green("Part 1 Test Cases"), "\n")


test_example(
   calc_total_winnings,
   test_input,
   6440L,
   label = "Test 1"
)
cat(cli::col_green("Part 2 Test Cases"), "\n")

test_example(
    function(x) calc_total_winnings(x, rule3 = TRUE),
    test_input,
    5905L,
    label = "Test2: using rule3"
)

cat(cli::col_br_magenta("=============== Part 1 ============= "), "\n")
input <- input_parser("input")

total_winnings <- calc_total_winnings(input)

if (total_winnings == 246424613) {
    cli::cli_alert_success("You got the right answer to part 1")
}

cli::cli_alert_info("The Part 1 answer is {total_winnings}")
cat(cli::col_br_magenta("=============== Part 2 ============= "), "\n")

total_winnings_rule3 <- calc_total_winnings(input, rule3 = TRUE)

cli::cli_alert_info("The Part 2 answer is {total_winnings_rule3}")
cat("\n")