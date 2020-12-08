# Part 1 ----
library(igraph)
library(magrittr)


input <- scan(
    file = "input/test.txt",
    what = "character",
    sep = "\n",
    blank.lines.skip = FALSE
)

parseBags <- function(x) {
    x %>%
        strsplit(., " contain |, ") %>%
        unlist() %>%
        gsub("[[:digit:]]|\\.|", "", .) %>%
        gsub("bags|bag", "", .) %>%
        gsub("no other", "Root", .) %>%
        trimws() -> parseV

    x %>%
        strsplit(., " contain |, ") %>%
        unlist() %>%
        gsub("[[:alpha:]]|[[:punct:]]", "", .) %>%
        as.numeric() -> weights

    df <- data.frame(
        source = parseV[1],
        dest   = parseV[-1],
        weight = weights[-1]
    )
    df[is.na(df)] <- 1
    return(df)
}

g <- input %>%
    lapply(parseBags) %>%
    do.call("rbind", .) %>%
    graph_from_data_frame()

gDF <- input %>%
    lapply(parseBags) %>%
    do.call("rbind", .)

answerP1 <- g %>%
    all_simple_paths("shiny gold", mode = "in") %>%
    unlist() %>%
    unique() %>%
    length() - 1

answerP2 <- g %>%
    all_simple_paths("shiny gold", mode = "out") %>%
    lapply(., function(i) E(g)$weight[i] %>% sum() - 1 ) %>%
    unlist() %>%
    sum(na.rm = TRUE)


# answerP2 <- g %>%
#     all_simple_paths("shiny gold", mode = "out") %>%
#     lapply(., function(i) E(g)$weight[i]) %>%
#     unlist() %>%
#     sum(na.rm = TRUE)


# answerP2 <- g %>%
#     all_simple_paths("shiny gold", mode = "out") %>%
#     lapply(., function(i) E(g)$weight[i] %>% sum()) %>%
#     unlist() %>%
#     sum(na.rm = TRUE) - 1




plot.igraph(g, asp = 0, main = "Bags",
    ## colors =======================================
    vertex.color = rgb(0.8,0.4,0.3,0.8),      ## color
    vertex.frame.color = "white",             ## border color
    ## shapes =======================================
    vertex.shape = "circle",                  ## none, circle, square, csquare,
    ## vrectangle, pie, raster, sphere
    ## rectangle, crectangle
    ## sizes =======================================
    edge.arrow.size = 0.2,
    vertex.size = 6,                         ## size, default = 15
)


# fname <- 'input_p1.txt'
rules <- readLines(con=file.path("input/07.txt"))
rules_divided <- strsplit(rules, 'contain')
main_colors <- unlist(lapply(1:length(rules_divided), function(i) rules_divided[[i]][1]))
main_colors <- gsub(' bags ', '', main_colors)
colors_contained <- unlist(lapply(1:length(rules_divided), function(i) rules_divided[[i]][2]))
colors_within_shinygold <- main_colors[grepl('shiny gold', colors_contained)]

#find bag colors contained with a specific color bag
find_colors <- function(bagcolor) {
    main_colors[grepl(bagcolor, colors_contained)]
}

#part I answer
results_p1 <- as.list(rep(NA, 100))
i <- 1
results_p1[[i]] <- colors_within_shinygold
while(length(results_p1[[i]])>0) {
    results_p1[[i+1]] <- unlist(sapply(results_p1[[i]], find_colors, USE.NAMES = FALSE))
    i <- i + 1
}
results_p1 <- results_p1[!is.na(results_p1)]
paste('Day 7, Part I answer is', length(unique(unlist(results_p1))))

#part 2
#function to extract numbers of bags contained with a specific bag color
extract_bag_numbers <- function(bagcolor) {
    test <- colors_contained[grepl(bagcolor, main_colors)]
    test <- unlist(strsplit(test, ' '))
    test <- test[grepl('[0-9]', test)]
    as.integer(test)
}

#function to extract colors of bags contained with a specific bag color
extract_colors <- function(bagcolor) {
    test <- colors_contained[grepl(bagcolor, main_colors)]
    test <- unlist(strsplit(test, ','))
    test <- gsub(' bags.', '', test)
    test <- gsub(' bags', '', test)
    test <- gsub(' bag.', '', test)
    test <- gsub(' bag', '', test)
    test <- gsub(' [0-9] ', '', test)
    test
}
#initialize the shiny gold bag
shiny_bag <- data.frame(bagcolors='shiny gold', bagnumbers=1, totalbags=1, stringsAsFactors = FALSE)

#function to do the counting as we search for all the other bags contained within shiny gold
bag_counting <- function(bagcolor, containnumber) {
    if(length(extract_bag_numbers(bagcolor))==0) {
        data.frame(bagcolors='none', bagnumbers=0, totalbags=0, stringsAsFactors = FALSE)
    } else {
        result <- data.frame(bagcolors=extract_colors(bagcolor), bagnumbers=extract_bag_numbers(bagcolor), stringsAsFactors = FALSE)
        result$totalbags <- result$bagnumbers * containnumber
        result
    }
}

#part II answer
results_p2 <- as.list(rep(NA, 100))
i <- 1
results_p2[[i]] <- shiny_bag
while(!all(results_p2[[i]]$totalbags == 0)) {
    results_p2[[i+1]] <- do.call(rbind, mapply(FUN=bag_counting, results_p2[[i]]$bagcolors[results_p2[[i]]$bagcolors!='none'], results_p2[[i]]$totalbags[results_p2[[i]]$bagcolors!='none'], SIMPLIFY = FALSE, USE.NAMES = FALSE))
    i <- i + 1
}
results_p2 <- results_p2[!is.na(results_p2)]
results_p2 <- do.call(rbind, lapply(results_p2, function(x) {x}))
results_p2 <- results_p2[results_p2$bagcolors!='none',]
paste("Day 7, Part II answer is", sum(results_p2$totalbags[2:length(results_p2$totalbags)]))






