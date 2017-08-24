

# Try-Catch example 1 - failure
subPk <- pkData[sort(sample(1:nrow(pkData), 80)), ]
par(mfrow = c(4,4))
yRange <- range(subPk$Conc)

for (i in unique(subPk$Subject)) {
	myTest <- subPk$Subject == i
	plot(subPk$Time[myTest], subPk$Conc[myTest], type="b",
	     main = paste("Subject", i), ylim = yRange, pch = 4)
	lines(loess.smooth(subPk$Time[myTest], subPk$Conc[myTest]))
}

# Try-Catch example 1 - "success"
subPk <- pkData[sort(sample(1:nrow(pkData), 80)), ]
#subPk <- subPk [ do.call("order", subPk[c("Subject", "Time")]), ]
par(mfrow = c(4,4))
yRange <- range(subPk$Conc)

for (i in unique(subPk$Subject)) {
	myTest <- subPk$Subject == i
	plot(subPk$Time[myTest], subPk$Conc[myTest], type="b",
    	main = paste("Subject", i), ylim = yRange, pch = 4)
	try(lines(loess.smooth(subPk$Time[myTest], subPk$Conc[myTest]), col = "red"))
}

# Another similar eg
subPk <- pkData[sample(1:nrow(pkData), 60),]
subPk <- subPk [ do.call("order", subPk[c("Subject", "Time")]), ]
par(mfrow = c(4,4))
yRange <- range(subPk$Conc)

for (i in unique(subPk$Subject)[1]) {
	myTest <- subPk$Subject == i
	plot(subPk$Time[myTest], subPk$Conc[myTest], 
	     main = paste("Subject", i), ylim = yRange, type="b", pch = 4)
	didItWork <- try({
		lines(loess.smooth(subPk$Time[myTest], subPk$Conc[myTest]), col = "red")
	}, silent = TRUE)
	if (class(didItWork) == "try-error") abline(h = mean(subPk$Conc[myTest]), col = "navy")
}

# Try-Catch example 2
eFun <- function(Dose, E0, ED50, Emax) E0 + (Dose * Emax) / (ED50 + Dose)

nlsSample <- function(howMany) {
	eData <- emaxData [ sample(1:nrow(emaxData), howMany), ]
	myNls <- nls( E ~ eFun(Dose, E0, ED50, Emax), data = eData, 
		start = list(E0 = 0, ED50 = 50, Emax = 100))
	coef(myNls)
}

nlsCoefs <- lapply(sample(1:50, 100, T), nlsSample)

