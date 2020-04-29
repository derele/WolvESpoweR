fix.X11 <-
function () 
{
    Sys.setenv(DISPLAY = readLines("~/.display.txt"))
}
