.First <-
function () 
{
    cat("\n\n   RAM is cheap and thinking hurts.\n   -- Uwe Ligges (about memory requirements in R)\n      R-help (June 2007)\n\n")
    Sys.setenv(DISPLAY = readLines("~/.display.txt"))
    options(repos = c(CRAN = "https://cran.r-project.org/"))
    options(show.signif.stars = FALSE)
    options(browser = "conkeror")
    library(Biobase)
    library(BiocManager)
}
.Last <-
function () 
{
    if (!any(commandArgs() == "--no-readline") && interactive()) {
        require(utils)
        try(savehistory(Sys.getenv("R_HISTFILE")))
        cat("\n\nMay these stats lead to conclusions...\n\n")
    }
}
