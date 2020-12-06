# Part 1 ----
library(magrittr)

input <- scan(
    file = "input/06.txt", 
    what = "character", 
    sep = " ", 
    blank.lines.skip = FALSE
)

yesCounter <- function(x) {
    strsplit(x, split = "") %>% 
        unlist() %>% 
        unique() %>% 
        sort() %>% 
        length()
}

questions <- split(
    x = input,
    f = with(rle(input == ""), rep(seq_along(values), lengths))
) %>% 
    .[. != ""] %>% 
    lapply(., yesCounter)

answerP1 <- questions %>% unlist() %>% sum()


# Part 2 ----
yesTabler <- function(x) {
    strsplit(x, split = "") %>% 
        unlist() %>% 
        table()
}

questions <- split(
    x = input,
    f = with(rle(input == ""), rep(seq_along(values), lengths))
) %>% 
    .[. != ""]

counts <- lapply(questions, length)

questions <- questions %>% lapply(yesTabler)

answerP2 <- lapply(seq_along(questions), function(i) {
    questions[[i]] %in% counts[[i]] %>% 
        .[.] %>% 
        length()
}) %>% 
    unlist() %>% 
    sum()


