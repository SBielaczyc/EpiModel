library(EpiModel)
#Basic ICMs wih Epimodel

#Note: Stochastic = Involving or containing a random variable or process 
#SI Model Stochasticity 
#nsims = Simulate model 10 times
#nsteps = 300 time steps
param <- param.icm(inf.prob = 0.2, act.rate = 0.25)
init <- init.icm(s.num = 500, i.num = 1)
control <- control.icm(type = "SI", nsims = 10, nsteps = 300)
mod <- icm(param, init, control)

mod

summary(mod, at = 125)
#Show averages across all simulations 
head(as.data.frame(mod, out = "mean"))

#Show vakues for a specific simulation
tail(as.data.frame(mod, out = "vals", sim = 1))

plot(mod)

#You can toggle certain elements on and off 
plot(mod, y = "i.num", sim.lines = TRUE, mean.smooth = FALSE, qnts.smooth = FALSE)

#SIR DCM/ICM Comparison 

#DCM model
param <- param.dcm(inf.prob = 0.2, act.rate = 0.8, rec.rate = 1/50,
                   a.rate = 1/100, ds.rate = 1/100, di.rate = 1/90, dr.rate = 1/100)
init <- init.dcm(s.num = 900, i.num = 100, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 300)

det <- dcm(param, init, control)
#ICM model
#Note same paramaters 
param <- param.icm(inf.prob = 0.2, act.rate = 0.8, rec.rate = 1/50,
                   a.rate = 1/100, ds.rate = 1/100, di.rate = 1/90,
                   dr.rate = 1/100)
init <- init.icm(s.num = 900, i.num = 100, r.num = 0)
control <- control.icm(type = "SIR", nsteps = 300, nsims = 10)

sim <- icm(param, init, control)

#Birth rate and death rate are not random
#They are calculated by rounding the product of the rate and state size 
control <- control.icm(type = "SIR", nsteps = 300, nsims = 10,
                       a.rand = FALSE, d.rand = FALSE)
sim2 <- icm(param, init, control)
plot(sim)
plot(sim2)
#Comparing Means

#add places a new set of lines on top of an existing plot
plot(det, alpha = 0.75, lwd = 4, main = "DCM and ICM Comparison")
plot(sim, qnts = FALSE, sim.lines = FALSE, add = TRUE, mean.lty = 2, legend = FALSE)
plot(sim2, qnts = FALSE, sim.lines = FALSE, add = TRUE, mean.lty = 3, legend = FALSE)

#Comparing Variance
par(mfrow = c(1,2), mar = c(3,3,2,1), mgp = c(2,1,0))

plot(sim, y = "di.flow", mean.line = FALSE,
     sim.lines = TRUE, sim.alpha = 0.15, ylim = c(0, 20),
     main = "di.flow: Full Stochastic Model")

plot(sim2, y = "di.flow", mean.line = FALSE,
     sim.lines = TRUE, sim.alpha = 0.15, ylim = c(0, 20),
     main = "di.flow: Limited Stochastic Model")

icm.compare <- rbind(round(as.data.frame(sim, out = "sd")[50,], 2),
                     round(as.data.frame(sim2, out = "sd")[50,], 2))
row.names(icm.compare) <- c("full", "lim")
icm.compare

#SIS model with Demography 
#Demography = The study of vital statistics, such as births, deaths, marriages, mortality, health, etc., 
#in populations and subgroups of populations.
param <- param.icm(inf.prob = 0.2, inf.prob.g2 = 0.1, act.rate = 0.5, balance = "g1",
                   rec.rate = 1/25, rec.rate.g2 = 1/50, a.rate = 1/100, a.rate.g2 = NA,
                   ds.rate = 1/100, ds.rate.g2 = 1/100, di.rate = 1/90, di.rate.g2 = 1/90)
init <- init.icm(s.num = 500, i.num = 1, s.num.g2 = 500, i.num.g2 = 1)
control <- control.icm(type = "SIS", nsteps = 500, nsims = 10)

set.seed(1)
sim <- icm(param, init, control)

par(mfrow = c(1,1))
plot(sim)

plot(sim, y = c("i.num", "i.num.g2"), mean.lwd = 3, sim.lines = TRUE, 
     sim.col = c("steelblue", "firebrick"), legend = TRUE,
     main = "Disease Prevalence: Means and Individual Simulations")
