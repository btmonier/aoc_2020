# Part 1 ----
library(magrittr)

input <- scan(
    file = "input/06.txt", 
    what = "character", 
    sep = " ", 
    blank.lines.skip = FALSE
)

questions <- split(
    x = input,
    f = with(rle(input == ""), rep(seq_along(values), lengths))
) %>% .[. != ""]



# Part 2 ----

