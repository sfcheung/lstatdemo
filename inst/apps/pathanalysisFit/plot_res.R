# Not yet working.

plot_res <- function(x) {
  if (is.null(colnames(x))) {
    colnames(x) <- rownames(x) <- paste("x", 1:nrow(x))
  }
  varnames <- colnames(x)
  k <- length(varnames)
  rhi <- max(abs(range(x)))
  rlo <- -1*rhi
  parold <- par(mfrow=c(k, k))
  on.exit(par(parold))
  for (i in 1:k) {
    for (j in 1:k) {
      parold2 <- par(mar=c(ifelse(i == k, 2, 0), 
                           ifelse(j == 1, 2, 0),
                           ifelse(i == 1, 2, 0),
                           ifelse(j == k, 2, 0)))
      plot(1, type="n", xlab="", ylab="", 
           xlim=c(rlo, rhi), ylim=c(rlo, rhi), axes=FALSE)
      box("plot")
      res <- x[i, j]
      points(res)
      par(parold2)
    }
    }
  }

plot_res_old <- function(x, rmax=NULL, rmin=NULL) {
  if (is.null(rmax)) rmax=max(x)
  if (is.null(rmin)) rmin=min(x)
  if (is.null(colnames(x))) {
    colnames(x) <- rownames(x) <- paste("x", 1:nrow(x))
  }
  varnames <- colnames(x)
  k <- length(varnames)
  tmpdf <- data.frame(rbind(1:k))
  #tmpdf <- as.data.frame(sapply(1:k, function(x) c(rmin, rmax)))
  colnames(tmpdf) <- varnames
  #print(tmpdf)
  suppressWarnings(
    pairs(tmpdf, lower.panel=panel.res, 
                 upper.panel=NULL,
                 diag.panel=panel.diag,
                 m_in=x, rmax=rmax, rmin=rmin)
    )
  }
  
panel.res <- function(x, y, m_in, rmax, rmin) {
  res <- m_in[x, y]
  rabs <- max(abs(c(rmin, rmax)))
  parold <- par(usr = c(-1*rabs, rabs, -1*rabs, rabs))
  points(res, res, cex=2)
  on.exit(par(parold))
  print(rmax); print(rmin)
  text(0, 0, round(res,2), cex=2)
  }

panel.diag <- function(x, m_in, rmax, rmin) {
  res <- m_in[x, x]
  rabs <- max(abs(c(rmin, rmax)))
  parold <- par(usr = c(-1*rabs, rabs, -1*rabs, rabs))
  points(res, res, cex=4)
  on.exit(par(parold))
  print(rmax); print(rmin)
  text(0, 0, round(res,2), cex=2)
  }