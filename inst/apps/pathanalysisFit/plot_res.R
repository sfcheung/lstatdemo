plot_res <- function(x) {
  varnames <- colnames(x)
  k <- length(varnames)
  tmpdf <- data.frame(rbind(1:k))
  colnames(tmpdf) <- varnames
  print(tmpdf)
  pairs(tmpdf, lower.panel=panel.res, 
               upper.panel=NULL,
               m_in=x)
  }
  
panel.res <- function(x, y, m_in) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  res <- m_in[x, y]
  #print(res)
  text(.5, .5, res, cex=10)
  }

