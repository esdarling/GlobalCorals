dirOut  <-  function(outputDir='output', subDir) {
	dir.create(file.path(outputDir, subDir), showWarnings = FALSE, recursive = TRUE)
}

make.transparent <- function(col, opacity=0.5) {
  if (length(opacity) > 1 && any(is.na(opacity))) {
    n <- max(length(col), length(opacity))
    opacity <- rep(opacity, length.out=n)
    col <- rep(col, length.out=n)
    ok <- !is.na(opacity)
    ret <- rep(NA, length(col))
    ret[ok] <- Recall(col[ok], opacity[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
  }
}

label <- function(px, py, lab, adj=c(0, 1), text=TRUE, log=FALSE, ...) {
  usr  <-  par("usr")
  x.p  <-  usr[1] + px*(usr[2] - usr[1])
  y.p  <-  usr[3] + py*(usr[4] - usr[3])
  if(log=="x"){x.p<-10^(x.p)}
  if(log=="y"){y.p<-10^(y.p)}
  if(log=="xy"){x.p<-10^(x.p);y.p<-10^(y.p)}
  if(text){
    text(x.p, y.p, lab, adj=adj, ...)
  } else {
    points(x.p, y.p, ...)
  }
}

to.dev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  dev(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.pdf <- function(expr, filename, ...) {
  to.dev(expr, pdf, filename, ...)
}

summaryGAMM  <-  function(model_output) {
	GAMM  <-  summary(model_output$gam)
	LME   <-  summary(model_output$lme)
	data  <-  data.frame(cbind(GAMM$p.table[, 1:4, drop=FALSE],
							GAMM$s.table[, 1:4, drop=FALSE],
							LME$tTable[1, 1:5, drop=FALSE],
							LME$tTable[2, 1:5, drop=FALSE],
							AIC=LME$AIC,
							BIC=LME$BIC), stringsAsFactors=FALSE)
	row.names(data)  <-  NULL
	data
}

