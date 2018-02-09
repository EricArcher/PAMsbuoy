# Drift graph simulation testing
simVals <- seq(-5,5, length.out=200)
x <- dnorm(simVals, 0, 1)
y <- dnorm(simVals, 0, 1)

singleSim <- data.frame(x=simVals, val=x/sum(x))
singleSim %>% filter(abs(x)<2) %>% .$val %>% sum()

doubleSim <- data.frame(x=rep(simVals, length(y)),
                        y=rep(simVals, each=length(x))) %>%
  mutate(val=dnorm(x)*dnorm(y), val=val/sum(val),
         dist=sqrt(x^2+y^2)) %>% arrange(dist) %>%
  mutate(CumVal=cumsum(val),
         CI = cut(CumVal, breaks=c(0,.9,.95,.99,1), include.lowest = TRUE),
         DistBins = cut(dist, breaks=seq(0, max(.$dist), .1), include.lowest =TRUE))
binnedSims <- doubleSim %>% group_by(DistBins) %>% summarise(val = sum(val)) %>%
  mutate(CumVal = cumsum(val), CI = cut(CumVal, breaks=c(0,.9,.95,.99,1), include.lowest = TRUE),
         dist = .1*as.numeric(DistBins))
maxCI <- binnedSims %>% group_by(CI) %>% summarise(Max=max(dist))

ggplot(binnedSims, aes(x=dist, y=val, fill=CI)) +
  geom_col(alpha=.5, position=position_nudge(x=-(.1/2))) +
  geom_vline(data=maxCI[1:3,], aes(xintercept=Max)) +
  scale_x_continuous(breaks=maxCI$Max)

doubleSim %>% filter((x^2+y^2)<(2.4^2)) %>% .$val %>% sum()

library(ggplot2)
ggplot(doubleSim, aes(x=x, y=y, fill=val)) + geom_tile() +
  scale_fill_gradientn(colors=viridis(256))
library(plotly)
plot_ly(z=matrix(arrange(doubleSim, x, y)$val, length(x), length(x))) %>%  add_surface()
# Real comparison to sim
myLike <- testit(testDat, start, realRate, realBearing, plot=TRUE, like=TRUE, debug=FALSE, numInit = 12, numGrid=50,
                 angleError=20, angleBias=, modelSd=7)$Like

myLike <- myLike %>% arrange(Distance) %>%
  mutate(CumVal = cumsum(ExpValue), CI = cut(CumVal, breaks=c(0,.9,.95,.99,1), include.lowest=TRUE))

binnedLike <- myLike %>% group_by(DistBreaks) %>% summarise(Value = sum(ExpValue)) %>%
  mutate(Distance=as.numeric(DistBreaks)*.05, CumVal=cumsum(Value),
         CI = cut(CumVal, breaks=c(0,.9,.95,.99,1), include.lowest=TRUE),
         ScaledValue = Value/(2*as.numeric(DistBreaks)-1))
ggplot(binnedLike, aes(x=Distance, y=Value, fill=CI)) +
  geom_col() + xlim(0,1)

ggplot(myLike, aes(x=Angle, y=Rate, fill=ExpValue)) + geom_tile() +
  scale_fill_gradientn(colors=viridis(256))
plot_ly(z=matrix(arrange(myLike, Rate, Angle)$ExpValue, 50, 50)) %>% add_surface()
