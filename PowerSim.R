library(ggplot2)

computePower <- function(effect, sd, nTotalSubjects, nTreated, 
                  nIterations, alpha=0.05){
  pVec <- rep(NA,nIterations)
  exposure <- c(rep(1,nTreated),rep(0,nTotalSubjects - nTreated))
  for(i in 1:nIterations){
    resid <- rep(rnorm(nTotalSubjects,0,sd))
    y <- effect*exposure + resid
    m <- lm(y~exposure)
    pVec[i] <- summary(m)$coefficients["exposure","Pr(>|t|)"]
  }
  power <- length(pVec[pVec<alpha])/length(pVec)
  return(list(power=power,p=pVec))
}

computePower(effect = 0.15, sd=1, nTotalSubjects = 100, nTreated = 50, nIterations = 100)

# Power curve for sample size
sizes <- seq(100, 3000, by=50)
powerVals <- vector(length=length(sizes))
for (i in 1:length(sizes)) {
  powerVals[i] <- computePower(effect=0.15,sd=1,
                          nTotalSubjects=sizes[i],nTreated=sizes[i]/2,
                          nIterations=100)$power
}
powerCalc <- data.frame(sizes, powerVals)
ggplot(powerCalc) + aes(x=sizes, y=powerVals) + geom_smooth(se=F) + geom_point() + 
  scale_x_continuous("Sample Size") +
  scale_y_continuous("Power")
