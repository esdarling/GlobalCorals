plotGAMM  <-  function(model_output, Vars) {
	fig   <-  fittedAndCIs(model_output)
	par(cex=1, cex.axis=1.3, cex.lab=1.5, oma=rep(1,4), mar=c(5.1,5.1,5.1,2.1))
	plot(fig$resp ~ fig$pred, cex=1.5, pch=16, 
		col=make.transparent('tomato', .7), las=1, xlab=Vars$pred, ylab='')
	label(-0.18, 0.5, paste0(Vars$resp, ' +/- 95% CI'), adj=c(0.5, 0.5), srt=90, cex=1.5, xpd=NA)
	fig   <-  capData(fig)
	lines(fig$pred, fig$fit)
	lines(fig$pred, fig$lower, lty='dashed', lwd=1.2)
	lines(fig$pred, fig$upper, lty='dashed', lwd=1.2)
}

plotResidualGAMM  <-  function(model_output, Vars) {
	par(cex=1, cex.axis=1.3, cex.lab=1.5, oma=rep(1,4), mar=c(5.1,5.1,5.1,2.1))
	plot(model_output$gam, col=make.transparent('tomato', .7), las=1, xlab=Vars$pred, ylab='')
	label(-0.18, 0.5, paste0(Vars$resp, ' (residuals)'), adj=c(0.5, 0.5), srt=90, cex=1.5, xpd=NA)
}

plotAllModels  <-  function(outputList, modelVars, fct) {
	for(k in seq_along(outputList)) {
		fct(outputList[[k]], Vars=modelVars[k,,drop=FALSE])
	}
}

fittedAndCIs  <-  function(model_output) {
	pred.gam  <-  data.frame(predict(model_output$gam, type="response", se.fit=TRUE), stringsAsFactors=FALSE)
	usedData  <-  summary(model_output$lme)$data[,c("resp", "pred", "province")]
	usedData  <-  usedData[complete.cases(usedData), ]
	figData   <-  cbind(usedData, pred.gam)
	figData  <-  within(figData, {
	     lower = fit-1.96*se.fit
	     upper = fit+1.96*se.fit
	 })
	figData
}

capData  <-  function(data, capCols) {
	ddply(data, .(pred), function(x)x[1,])
}