library(plotly)
Plotly <- plotly("jcp1016", "sf1m2imavn")
ggplotly <- function(gg, p=Plotly){
        if(!is.ggplot(gg)){
                stop("gg must be a ggplot")
        }
        if(!is.function(p$plotly)){
                stop("p must be a plotly interface object")
        }
        pargs <- gg2list(gg)
        resp <- do.call(p$plotly, pargs)
        browseURL(resp$url)
        invisible(list(data=pargs, response=resp))
}
sendJSON <- function(pargs, p=Plotly){
        stopifnot(is.list(pargs))
        resp <- do.call(p$plotly, pargs)
        browseURL(resp$url)
        invisible(list(data=pargs, response=resp))
}