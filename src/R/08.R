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



# Part 2 ----
read_delim(
    file = "input/08.txt",
    delim = " ",
    col_names = c("instr", "value"),
    col_types = "cd"
) %>%
    mutate(
        visits = 0
    ) -> asm

orig_program <- asm

for (run in seq_len(nrow(asm))) {

    asm <- orig_program

    curr <- 1
    accumulator <-  0

    switch(
        asm$instr[run],
        "acc" = "acc",
        "nop" = "jmp",
        "jmp" = "nop"
    ) -> asm$instr[run]

    while (all(asm$visits < 2)) {

        asm$visits[curr] <- asm$visits[curr] + 1

        switch(
            asm$instr[curr],
            "acc" = curr + 1,
            "nop" = curr + 1,
            "jmp" = curr + asm[curr,]$value
        ) -> nxt

        if (nxt > nrow(asm)) break

        if ((asm$visits[curr] < 2) && (asm$instr[curr] == "acc")) {
            accumulator <- accumulator + asm$value[curr]
        }

        curr <- nxt

    }

    if (all(asm$visits < 2)) break

}
