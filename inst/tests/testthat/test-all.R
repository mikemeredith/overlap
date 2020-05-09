
# test_that code for the overlap package


# library(testthat)
# library(overlap)
# test_file("./overlap/inst/tests/test-all.R")

require(overlap) # otherwise can't find simulatedData

context("Built-in data sets")
test_that("built-in data sets are unchanged",  {
  data(simulatedData)
  expect_that(round(mean(tigerTrue), 6), equals(0.157957))
   expect_that(round(mean(pigTrue), 6), equals(0.157913))
  expect_that(round(mean(tigerObs), 6), equals(3.248677))
  expect_that(round(mean(pigObs), 6), equals(3.328342))

  data(kerinci)
  expect_that(dim(kerinci), equals(c(1098, 3)))
  expect_that(names(kerinci), equals(c("Zone", "Sps", "Time")))
  expect_that(sum(kerinci$Time), equals(540.68))
  expect_that(sum(kerinci$Zone), equals(2950))
  expect_that(nlevels(kerinci$Sps), equals(8))
  expect_that(summary(kerinci$Sps),
    is_equivalent_to(c(28, 86, 104, 273, 200, 25, 181, 201)))
  expect_that(levels(kerinci$Sps),
    equals(c("boar", "clouded", "golden", "macaque", "muntjac",
             "sambar", "tapir", "tiger")))

  data(simCalls)
  expect_that(dim(simCalls), equals(c(100, 2)))
  expect_that(names(simCalls), equals(c("time", "dates")))
  expect_that(round(sum(simCalls$time), 4), equals(210.7662))
  expect_true(is.character(simCalls$dates))
} )

context("Main computation functions")
test_that("overlapTrue gives correct answer", {
  data(simulatedData)
  expect_that(overlapTrue(tigerTrue, pigTrue), equals(0.2910917, tolerance = 1e-6))
  expect_that(overlapTrue(cbind(tigerTrue, pigTrue)), equals(0.2910917, tolerance = 1e-6))
})

test_that("densityFit gives correct answer", {
  data(simulatedData)
  expect_that(densityFit(tigerObs, c(0, pi/2, pi, 3*pi/2, 2*pi), 30),
    equals(c(0.02440435, 0.44522913, 0.02179983, 0.50513539, 0.02440435), tolerance = 1e-7))
  expect_that(densityFit(pigObs, c(0, pi/2, pi, 3*pi/2, 2*pi), 10),
    equals(c(7.877244e-06, 4.522317e-02, 4.622752e-01, 1.216268e-01, 7.877244e-06),
      tolerance = 1e-7))
})

test_that("getBandWidth gives correct answer", {
  data(simulatedData)
  # expect_that(getBandWidth(tigerObs), equals(29.90645, tolerance = 1e-7))
  expect_that(getBandWidth(tigerObs), equals(29.90650, tolerance = 1e-5))
  # expect_that(getBandWidth(pigObs), equals(10.42065, tolerance = 1e-7))
  expect_that(getBandWidth(pigObs), equals(10.42076, tolerance = 1e-5))
})

test_that("overlapEst gives correct answer", {
  data(simulatedData)
  expect_that(round(overlapEst(tigerObs, pigObs), 5),
    is_equivalent_to(c(0.29086, 0.26920, 0.22750)))
  expect_that(
    round(overlapEst(tigerObs, pigObs, adjust=c(1.2, 1.5, 1)), 5),
    is_equivalent_to(c(0.31507, 0.28849, 0.23750)))
  expect_that(
    round(overlapEst(tigerObs, pigObs, adjust=c(NA, 1, NA)), 6),
    is_equivalent_to(c(NA_real_, 0.269201, NA_real_)))
  expect_that(
    round(overlapEst(tigerObs, pigObs, type="Dhat4"), 6),
    is_equivalent_to(0.269201))
})

test_that("sunTime gives correct answer", {
  data(simCalls)
  Dates <- as.POSIXct(simCalls$dates, tz="GMT")
  coords <- matrix(c(-3, 56), nrow=1)
  Coords <- sp::SpatialPoints(coords, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  st <- sunTime(simCalls$time, Dates, Coords)
  expect_that(round(sum(st), 4), equals(207.0542))
})

stopifnot(getRversion() >= '3.6.0')

context("Bootstrap functions")
test_that("bootstrap smooth=TRUE gives correct answer", {
  data(simulatedData)
  set.seed(123)
  boots <- bootstrap(tigerObs, pigObs, nb=99)
  expect_that(round(mean(boots), 6),
    is_equivalent_to(0.345504))
  # set.seed(123) # parallel not reproducible
  # bootpar <- bootstrap(tigerObs, pigObs, nb=99, cores=2)
  # expect_that(round(mean(bootpar), 6),
    # is_equivalent_to(0.304968))
  set.seed(123)
  boots <- bootstrap(tigerObs, pigObs, nb=99, type="Dhat4")
  expect_that(round(mean(boots), 6),
    is_equivalent_to(0.33061))
})

test_that("bootstrap smooth=FALSE gives correct answer", {
  data(simulatedData)
  set.seed(123)
  boots <- bootstrap(tigerObs, pigObs, nb=99, smooth=FALSE)
  expect_that(round(mean(boots), 6),
    is_equivalent_to(0.28488))
  # set.seed(123) # parallel not reproducible
  # bootpar <- bootstrap(tigerObs, pigObs, nb=99, cores=2)
  # expect_that(round(mean(bootpar), 6),
    # is_equivalent_to(0.304968))
  set.seed(123)
  boots <- bootstrap(tigerObs, pigObs, nb=99, smooth=FALSE, type="Dhat4") 
  expect_that(round(mean(boots), 6),
    is_equivalent_to(0.26333))
})

test_that("resample smooth=TRUE gives correct answer", {
  data(simulatedData)
  set.seed(123)
  tigSim <- resample(tigerObs, 5, TRUE)
  expect_that(round(colMeans(tigSim), 6),
    equals(c(3.088229, 3.459810, 3.103107, 3.149954, 3.055276)))
  pigSim <- resample(pigObs, 5, TRUE)
  expect_that(round(colMeans(pigSim), 6),
    equals(c(3.184782, 3.193389, 3.180786, 3.316040, 3.317885)))
  boots <- bootEst(tigSim, pigSim)
  expect_that(round(colMeans(boots), 6),
    is_equivalent_to(c(0.342983, 0.326681, 0.310500)))
  bootpar <- bootEst(tigSim, pigSim, cores=2)
  expect_that(round(colMeans(bootpar), 6),
    is_equivalent_to(c(0.342983, 0.326681, 0.310500)))
  boots <- bootEst(tigSim, pigSim, adjust=c(NA, 1, NA))
  expect_that(round(colMeans(boots), 6),
    is_equivalent_to(c(NA_real_, 0.326681, NA_real_)))
  boots <- bootEst(tigSim, pigSim, type="Dhat4")
  expect_that(round(mean(boots), 6),
    is_equivalent_to(0.326681))
})

test_that("resample smooth=FALSE gives correct answer", {
  data(simulatedData)
  set.seed(123)
  tigSim <- resample(tigerObs, 5, FALSE)
  expect_that(round(colMeans(tigSim), 6),
    equals(c(3.305859, 3.110860, 3.184909, 3.271987, 3.262150)))
  pigSim <- resample(pigObs, 5, FALSE)
  expect_that(round(colMeans(pigSim), 6),
    equals(c(3.347331, 3.524023, 3.279544, 3.265070, 3.374756)))
  boots <- bootEst(tigSim, pigSim)
  expect_that(round(colMeans(boots), 6),
    is_equivalent_to(c(0.281553, 0.260792, 0.207000)))
  bootpar <- bootEst(tigSim, pigSim, cores=2)
  expect_that(round(colMeans(bootpar), 6),
    is_equivalent_to(c(0.281553, 0.260792, 0.207000)))
  boots <- bootEst(tigSim, pigSim, adjust=c(NA, 1, NA))
  expect_that(round(colMeans(boots), 6),
    is_equivalent_to(c(NA_real_, 0.260792, NA_real_)))
  boots <- bootEst(tigSim, pigSim, type="Dhat4")
  expect_that(round(mean(boots), 6),
    is_equivalent_to(0.260792))
})

context("Confidence intervals")
test_that("bootCI gives same results as boot.ci for common CIs", {
  require(boot)
  set.seed(123)
  dat <- runif(20)
  mean.b <- function(d,p,...) mean(d[p])
  bootout <- boot(dat, mean.b, 999)
  t0 <- bootout$t0
  bt <- as.vector(bootout$t)
  expect_that(t0, equals(mean(dat)))

  expect_that(bootCI(t0, bt)['norm',],
    is_equivalent_to(boot.ci(bootout, 0.95, "norm")$norm[2:3]))
  expect_that(bootCI(t0, bt)['basic',],
    is_equivalent_to(boot.ci(bootout, 0.95, "basic")$basic[4:5]))
  expect_that(bootCI(t0, bt)['perc',],
    is_equivalent_to(boot.ci(bootout, 0.95, "perc")$perc[4:5]))

  expect_that(bootCI(t0, bt, 0.8)['norm',],
    is_equivalent_to(boot.ci(bootout, 0.8, "norm")$norm[2:3]))
  expect_that(bootCI(t0, bt, 0.8)['basic',],
    is_equivalent_to(boot.ci(bootout, 0.8, "basic")$basic[4:5]))
  expect_that(bootCI(t0, bt, 0.8)['perc',],
    is_equivalent_to(boot.ci(bootout, 0.8, "perc")$perc[4:5]))

  expect_that(bootCIlogit(t0, bt)['norm',],
    is_equivalent_to(boot.ci(bootout, 0.95, "norm", h=qlogis, hinv=plogis)$norm[2:3]))
  expect_that(bootCIlogit(t0, bt)['basic',],
    is_equivalent_to(boot.ci(bootout, 0.95, "basic", h=qlogis, hinv=plogis)$basic[4:5]))
} )

test_that("bootCI gives correct results", {
  set.seed(123)
  dat <- runif(20)
  t0 <- sd(dat)
  bootmat <- matrix(sample(dat, 20*999, replace=TRUE), 20, 999)
  bt <- apply(bootmat, 2, sd)

  expect_that(round(bootCI(t0, bt)['norm',], 6),
    is_equivalent_to(c(0.257335, 0.389638)))
  expect_that(round(bootCI(t0, bt)['perc',], 6),
    is_equivalent_to(c(0.229293, 0.364734 )))
  expect_that(round(bootCI(t0, bt)['basic',], 6),
    is_equivalent_to(c(0.262208, 0.397649 )))
  expect_that(round(bootCI(t0, bt)['norm0',], 6),
    is_equivalent_to(c(0.247319, 0.379623 )))
  expect_that(round(bootCI(t0, bt)['basic0',], 6),
    is_equivalent_to(c(0.239309, 0.374750)))
} )

test_that("bootCIlogit gives correct results", {
  set.seed(123)
  dat <- runif(20)
  t0 <- sd(dat)
  bootmat <- matrix(sample(dat, 20*999, replace=TRUE), 20, 999)
  bt <- apply(bootmat, 2, sd)

  expect_that(round(bootCIlogit(t0, bt)['norm',], 6),
    is_equivalent_to(c(0.258635, 0.398876)))
  expect_that(round(bootCIlogit(t0, bt)['perc',], 6),
    is_equivalent_to(c(0.229293, 0.364734)))
  expect_that(round(bootCIlogit(t0, bt)['basic',], 6),
    is_equivalent_to(c(0.266392, 0.412031)))
  expect_that(round(bootCIlogit(t0, bt)['norm0',], 6),
    is_equivalent_to(c(0.248729, 0.386398)))
  expect_that(round(bootCIlogit(t0, bt)['basic0',], 6),
    is_equivalent_to(c(0.238671, 0.376942)))
} )


context("Output from plotting functions")
test_that("densityPlot gives correct output", {
  data(simulatedData)
  foo <- densityPlot(pigObs)
  expect_that(class(foo), equals("data.frame"))
  expect_that(names(foo), equals(c("x", "y")))
  expect_that(nrow(foo), equals(128))
  wanted <- foo$x > 0 & foo$x < 24
  expect_that(round(mean(foo$y[wanted]) * 24, 4), equals( 0.9961))

  foo <- densityPlot(tigerObs, xscale = NA, xcenter = "m", n.grid=1024)
  expect_that(class(foo), equals("data.frame"))
  expect_that(names(foo), equals(c("x", "y")))
  expect_that(nrow(foo), equals(1024))
  wanted <- foo$x > -pi & foo$x < pi
  expect_that(round(mean(foo$y[wanted]) * 2 * pi, 4), equals( 1.0004))

  expect_error(densityPlot(factor(LETTERS)), "The times of observations must be in a numeric vector.")
  expect_error(densityPlot(trees), "The times of observations must be in a numeric vector.")
  expect_error(densityPlot(read.csv), "The times of observations must be in a numeric vector.")
  expect_error(densityPlot(numeric(0)), "You have 0 different observations")
  expect_error(densityPlot(2), "You have 1 different observations")
  expect_error(densityPlot(rep(2, 5)), "You have 1 different observations")
  expect_error(densityPlot(c(1,2,3,NA)), "Your data have missing values.")
  expect_error(densityPlot(c(1,2,3,-2)), "You have times")
  expect_error(densityPlot(c(1,2,3,10)), "You have times")


})

test_that("overlapPlot gives correct output", {
  data(simulatedData)
  foo <- overlapPlot(pigObs, tigerObs)
  expect_that(class(foo), equals("data.frame"))
  expect_that(names(foo), equals(c("x", "densityA", "densityB")))
  expect_that(nrow(foo), equals(128))
  wanted <- foo$x > 0 & foo$x < 24
  expect_that(round(mean(foo$densityA[wanted]) * 24, 4), equals( 1.0079))
  expect_that(round(mean(foo$densityB[wanted]) * 24, 4), equals( 1.0067))

  foo <- overlapPlot(pigObs, tigerObs, xscale = NA, xcenter = "m", n.grid=1024)
  expect_that(class(foo), equals("data.frame"))
  expect_that(names(foo), equals(c("x", "densityA", "densityB")))
  expect_that(nrow(foo), equals(1024))
  wanted <- foo$x > -pi & foo$x < pi
  expect_that(round(mean(foo$densityA[wanted]) * 2 * pi, 4), equals(0.9981))
  expect_that(round(mean(foo$densityB[wanted]) * 2 * pi, 4), equals(1.0008))

  expect_error(overlapPlot(pigObs, factor(LETTERS)), "The times of observations must be in a numeric vector.")
  expect_error(overlapPlot(trees, pigObs), "The times of observations must be in a numeric vector.")
  expect_error(overlapPlot(tigerObs, read.csv), "The times of observations must be in a numeric vector.")
  expect_error(overlapPlot(numeric(0), tigerObs), "You have 0 different observations")
  expect_error(overlapPlot(2, tigerObs), "You have 1 different observations")
  expect_error(overlapPlot(rep(2, 5), pigObs), "You have 1 different observations")
  expect_error(overlapPlot(pigObs, c(1,2,3,NA)), "Your data have missing values.")
  expect_error(overlapPlot(c(1,2,3,-2), pigObs), "You have times")
  expect_error(overlapPlot(c(1,2,3,10), tigerObs), "You have times")

  })

graphics.off()


