# Part 1 ----

input <- scan(
    file = "input/08.txt",
    what = "character",
    sep = "\n",
    blank.lines.skip = FALSE
)

gameParser <- function(x) {
    x %>% strsplit(split = " ") %>%
        unlist() -> df
    df <- data.frame(
        command = df[1],
        value   = df[2],
        dir     = ifelse(grepl("^\\+", df[2]), "+", "-"),
        stringsAsFactors = FALSE
    )
    df[, 2] <- gsub("^\\+|^^\\-", "", df[, 2])
    df[, 2] <- as.numeric(df[, 2])
    return(df)
}

commandDF <- input %>%
    lapply(gameParser) %>%
    do.call(rbind, .)


i <- 1
accVal <- 0
visitedSites <- c()
while(!(i %in% visitedSites)) {
    visitedSites <- c(visitedSites, i)
    tmp <- commandDF[i, ]

    message("Line:    ", i)
    message("accVal:  ", accVal)
    message("command: ", tmp[["command"]])
    message("---")


    if (tmp[["command"]] != "jmp") {
        if (tmp[["command"]] == "acc") {
            if (tmp[["dir"]] == "+") {
                accVal <- accVal + tmp[["value"]]
            } else {
                accVal <- accVal - tmp[["value"]]
            }
        }

        i <- i + 1
    } else {
        if (tmp[["dir"]] == "+") {
            i <- i + tmp[["value"]]
        } else {
            i <- i - tmp[["value"]]
        }
    }
}





