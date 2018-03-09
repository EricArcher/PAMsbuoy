# Drift Simulated Testing
library(ggplot2)
library(dplyr)
library(viridisLite)
library(plotly)
library(gridExtra)



center <- c(32, -117); distance <- 1.5; shape=0; angles=seq(from=1, to=360, length.out=360); boatKnots=10

start <- data.frame(Latitude=32, Longitude=-117, UTC=as.POSIXct('2017-08-08 08:00:00'), Buoy=0)

boatCircle <- makeCircle(start=start, center=start, distance=1.5,
                         angles=seq(from=1, to=195, length.out=180), boatKnots=10) %>%
  makeDifar(buoy)

boatLines <- makeLines(start=start, distances=c(1,2), boatKnots=10, angle=0,turn=135,nPoints=20)

buoy <- driftBuoy(start, rate=2, bearing=130, times=boatCircle$UTC)


boatLines <- makeDifar(boatLines, start)


driftDf <- likeDf(nAngles=60,nRates=60, boat=boatCircle, start=start) %>%
  arrange(desc(Value))

ggplot(driftDf, aes(x=Angle, y=Rate, z=Value, fill=Value)) + geom_tile() +
  scale_fill_gradientn(colors=viridis(256, direction = 1)) +
  coord_cartesian(xlim=c(0,360), ylim=c(0,3), expand=FALSE) +
  geom_contour(breaks=mean(driftDf$Value))

driftCalibration(list(position=start, calibration=boatLines))



realRate <- .7; realBearing <- 130
angleBias <- 0; angleError <- 7; modelSd <- 7

testDat <- makeCircle(start=start, center=start, distance=1, angles=seq(from=0, to=100, length.out=10), boatKnots=5)
testit(testDat, start, realRate, realBearing, plot=TRUE, like=TRUE, debug=FALSE, numInit = 12, numGrid=50,
       angleError=angleError, angleBias=angleBias, modelSd=modelSd)

testDat <- makeLines(start=start, distances=c(3,3), boatKnots=10, angle=90, turn=160, nPoints=c(15,15))
set.seed(12345)
testit(testDat, start, realRate, realBearing, plot=TRUE, like=TRUE, debug=FALSE, numInit = 12, numGrid=50,
       angleError=angleError, angleBias=angleBias, modelSd=modelSd)

testit(testDat, start, realRate, realBearing, plot=TRUE, like=TRUE, debug=FALSE, numInit = 10, numGrid=50,
           angleError=angleError, angleBias=angleBias, modelSd=modelSd)

testitbias(testDat, start, realRate, realBearing, plot=TRUE, like=FALSE, debug=FALSE, numInit = 10, numGrid=50,
       angleError=angleError, angleBias=angleBias, modelSd=modelSd)

driftSims <- do.call(rbind, lapply(1:100, function(x) {
  drift <- testit(testDat, start, realRate, realBearing, plot=FALSE, like=TRUE, debug=FALSE, numInit = 30, numGrid=50,
         angleError=angleError, angleBias=angleBias, modelSd=modelSd)
  data.frame(Rate=drift$rate, Bearing = drift$bearing, RateErr=drift$err[1], BearingErr=drift$err[2],
             CI90=drift$CI90, CI95=drift$CI95, CI99=drift$CI99) %>%
    mutate(RateCI2 = (abs(Rate-realRate) < 2*RateErr), BearingCI2 = (abs(Bearing-realBearing) < 2*BearingErr))
}))

driftSims <- gather(driftSims, CI, Value, c('CI90', 'CI95', 'CI99'))
ggplot(driftSims, aes(x=Rate, y=Value*Rate, color=abs(Rate-realRate))) +
# ggplot(driftSims, aes(y=abs(Rate-realRate), x=Value, color=CI)) +
  geom_point(size=3, alpha=.7) +
  geom_vline(xintercept=realRate) + geom_hline(yintercept = 1) +
  scale_color_gradientn(colors=viridis(256)) +
  xlim(0,3)
  # ylim(0,2)

simDiagnostic(start, filter(driftSims, Rate <= 3), realRate, realBearing, 60*60)


ggplot(driftSims) +
  geom_histogram(aes(x=Rate), binwidth=.2) + geom_vline(xintercept=2)
  # geom_histogram(aes(x=Bearing), binwidth=2) + geom_vline(xintercept =130)

logTest <- function(vals=seq(-3,3, length.out=100), n) {
  log(dnorm(vals, 0, 1)^n)
}
vals <- seq(-3,3, length.out=100)
logDat <- data.frame(x=vals) %>%
  mutate(log1 = logTest(x, 1),
         log2 = logTest(x, 2))

ggplot(logDat, aes(x=x)) + geom_point(aes(y=log1), color='red') +
  geom_point(aes(y=log2), color='blue') +
  geom_point(aes(y=log2/2), color='green')


##### Distance error comparison as function of rate and angle error
grf <- function(x, theta) {
  x^2 - 2*cos(theta*pi/180)*x + 1
}

graphMe <- do.call(rbind, lapply(seq(0,90,5), function(angle) {
  xs <- seq(0,2, length.out=40)
  data.frame(RateRatio=xs, DistErrorRatio=grf(xs, angle), AngleError=angle)
}))

ggplot(graphMe, aes(x=RateRatio, y=DistErrorRatio, color=as.factor(AngleError))) + geom_line() + geom_hline(yintercept=1) + ylim(0,3)
##############
# Distance distribution
##############
# tf <- tempfile()
# Rprof(tf)
set.seed(123)
distDist <- distanceDistribution(testDat, start, reps=100, angleError=30, angleBias = 0)
# prof <- lineprof(brbgs <- expectedBearing(boat, start, 1,1), torture=TRUE)
# prof
# brngs <- for(i in 1:1000) {
#   expectedBearing(boat, start, 1, 1)
# }
# Rprof(NULL)

##### Examples
# More time - 24 minutes, ~ 280 meters
# Less time - 9 minutes, ~ 115 metres

#### REFACTORING AND SHIT TESTING
start <- data.frame(Latitude=32, Longitude=-117, UTC=as.POSIXct('2017-08-08 08:00:00'), Buoy=0)
testDat <- makeLines(start=start, distances=c(1.5,1.5), boatKnots=6, angle=90, turn=160, nPoints=c(10,5))
test <- likeDf(boat=testDat %>% makeDifar(start), start=start)


