# Part 1 ----
input <- scan(
    file = "input/09.txt",
    what = numeric(),
    sep = "\n",
    blank.lines.skip = FALSE
)

w <- 25
i <- 1
while(T) {
    tmp <- input[i:(i + (w - 1))] %>% combn(2, sum) %>% unique()
    if (!(input[i + w] %in% tmp)) {
        answerP1 <- input[i + w]
        break
    }
    i <- i + 1
}



# Part 2 ----
stop <- FALSE
for (w in 2:length(input)) {
    for (i in 1:length(input)) {
        tV <- input[i:(i + (w - 1))]
        if (sum(tV, na.rm = TRUE) == answerP1) {
            p2V <- tV[order(tV)]
            stop <- TRUE
            break
        }
    }
    if (stop) break
}

answerP2 <- min(p2V) + max(p2V)


