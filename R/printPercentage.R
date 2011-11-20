printPercentage <-
function (i, n, dp = 0, first = 1, last = n, prev = i - 1)
{
    disp = round(100 * i/n, dp)
    if (prev >= first)
        prev.disp = round(100 * prev/n, dp)
    else prev.disp = ""
    if (disp > prev.disp) {
        nc = nchar(prev.disp)
        if (i != first) {
            for (j in 1:(nc + 1)) cat("\b", sep = "")
        }
        cat(disp, sep = "")
        cat("%", sep = "")
    }
    if (i == last) cat("\n")
    
    return(NULL)
}

