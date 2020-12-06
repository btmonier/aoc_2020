# Part 1 ----
library(magrittr)

input <- scan(
    file = "input/05.txt",
    what = "character"
) %>% 
    strsplit(., "")

seatDecoder <- function(x) {
    i <- 1
    rows <- 0:127
    while(i < 8) {
        if (x[i] == "F") rows <- head(rows, length(rows) * 0.5)
        if (x[i] == "B") rows <- tail(rows, length(rows) * 0.5)
        i <- i + 1
    }
    
    cols <- 0:7
    i <- 8
    while (i < 11) {
        if (x[i] == "L") cols <- head(cols, length(cols) * 0.5)
        if (x[i] == "R") cols <- tail(cols, length(cols) * 0.5)
        i <- i + 1
    }
    (rows * 8) + cols
    
    ## Debug
    # paste0("Row: ", rows, " | ", "Col: ", cols, " | ", "Seat ID: ", seatID)
}

seatIDs <- lapply(input, seatDecoder) %>% unlist()

answerP1 <- max(seatIDs)


# Part 2 ----
seatIDs <- seatIDs[order(seatIDs)]
seqFull <- min(seatIDs):max(seatIDs)
mySeat <- seqFull[!seqFull %in% seatIDs]

