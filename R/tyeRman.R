# tyeRman misc functions

#' impute
#' This function is a quick and dirty imputation function
#' @param d is a dataframe containg columns with NA to be imputed, func is the function to sue to calc the imputed value, uses median currently.
#' @export
#' @keywords impute
#' @examples
#' impute(d, "median")

impute <- function(d, func) {
  d.imp <- d
  for (i in 1:ncol(d.imp)) {
    for (j in 1:nrow(d.imp)) {
      med <- sapply(d.imp[i], func, na.rm=TRUE)[[1]]
      if (is.na(d.imp[j,i])) {d.imp[j,i] <- med}
    }
  }
  return(d.imp)
}

#' panel.cor
#' This function is a custom add-on for use with pairs command.
#' @param x, y are numeric vectors, digits is number of sig figs to display
#' @export
#' @keywords graphics
#' @examples
#' panel.cor()

panel_cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#' get_summary_stats
#' This function is a custom add-on for use with pairs command.
#' @param x is vector
#' @export
#' @keywords statistics
#' @examples
#' get_summary_stats(x)

get_summary_stats <- function(x) {
  return(c(	m=mean(x, na.rm=TRUE)
            , med=median(x, na.rm=TRUE)
            , sd=sd(x, na.rm=TRUE)
            , cv = (sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE))
            , n = length(x)
  ))
}

