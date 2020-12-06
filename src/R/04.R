# Part 1 ----
library(magrittr)

passports <- scan(
    file = "input/04.txt", 
    what = "character", 
    sep = " ", 
    blank.lines.skip = FALSE
)

passports <- split(
    x = input,
    f = with(rle(input == ""), rep(seq_along(values), lengths))
) %>% .[. != ""]

kv2df <- function(x, delim = ":") {
    tmp <- strsplit(x, delim, fixed = TRUE)
    tmpDF <- as.data.frame(tmp, stringsAsFactors = FALSE)
    colnames(tmpDF) <- tmpDF[1, ]
    tmpDF[-1, ]
}

passportsDF <- Reduce(
    f = function(dtf1, dtf2) merge(dtf1, dtf2, all = T),
    x = lapply(passports, kv2df)
)

answerP1 <- passportsDF[ , !(names(passportsDF) %in% "cid")]
answerP1 <- nrow(answerP1[complete.cases(answerP1), ])



# Part 2 ----
library(dplyr)
library(tidyr)

passportsDF %>% 
    as_tibble() %>% 
    mutate(hgt_val = gsub("[[:digit:]]", "", hgt)) %>% 
    mutate(hgt = gsub("[[:alpha:]]", "", hgt)) %>% 
    mutate(
        iyr = as.numeric(iyr),
        byr = as.numeric(byr),
        eyr = as.numeric(eyr),
        hgt = as.numeric(hgt)
    ) %>% 
    select(-cid)
    filter(
        nchar(byr) == 4 & byr %in% 1920:2002,
        nchar(iyr) == 4 & iyr %in% 2010:2020,
        nchar(eyr) == 4 & eyr %in% 2020:2030,
        hgt_val == "in" | hgt_val == "cm",
        grepl("^#", hcl) & nchar(hcl) == 7,
        nchar(ecl) == 3 & ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
        nchar(pid) == 9 & !grepl("\\D", pid)
    ) -> filtPassports
nCM <- filtPassports %>% filter(hgt_val == "cm" & hgt %in% 150:193) %>% nrow()
nIN <- filtPassports %>% filter(hgt_val == "in" & hgt %in% 59:76) %>% nrow()

answerP2 <- nCM + nIN


