ll <-
function (envir = globalenv(), ...) 
{
    obs <- ls(envir = envir, ...)
    sizes <- sapply(obs, function(x) object.size(get(x)))
    modes <- sapply(obs, function(x) mode(get(x)))
    classes <- sapply(obs, function(x) class(get(x)))
    as.data.frame(cbind(sizes, modes, classes))
}
