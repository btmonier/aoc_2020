# Part 1 ----
input <- scan(
    file = "input/test.txt",
    what = numeric(),
    sep = "\n",
    blank.lines.skip = FALSE
) %>%
    data.frame(
        x = .,
        visit = FALSE,
        vGen = 0,
        d1 = 0,
        d2 = 0,
        d3 = 0
    )
joltAdapt <- max(input$x) + 3

tmp <- input[input$x <= 3 & !input$visit, ]
tmp <- tmp[min(tmp$x), ]



# Part 2 ----
