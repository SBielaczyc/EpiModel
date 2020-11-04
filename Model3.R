library(EpiModel)
library(ggplot2)
library(writexl)
library(tidyverse)
library(ggplot2)
library(lattice)
#quarantine.rate <- function(t){
# ifelse(t%%7==0,(1/4),0)
#}
SEIQHFR <- function(t, t0, params){
  with (as.list(c(t0,params)),{
    
    days = t 
    t.prop = ifelse(t%%q.freq==0, t.prop,0)
    
  
    num = s.num + e.num + i.num + q.num + h.num + r.num +f.num
    t.num = s.num + e.num + i.num 
    #ce is the number of people that an infected person infects per unit time 
    ce <- R0/i.dur
    #Lambda corresponds to the proportion of people who become infected per unit time 
    lambda <- ce * (i.num/num)
    #cat(t,"\n")
    #dS = Susceptible Individuals
    dS <- -lambda*s.num
    #dE = exposed (but not infectious) individuals
    dE <- (lambda*s.num) - ((1/e.dur)*e.num)
    #dI = Infected/infectious individuals
    dI <- ((1/e.dur)*e.num) - ((1/i.dur)*i.num) - (h.rate*i.num) - (t.num *t.prop*(i.num/t.num))  #Change qr
    #dQ = quarantined individuals
    #S.num = 80, e.num = 16, and i.num = 4, num = 100
    #tested number = 25, positive test = 1
    #t.num * q.rate * (i.num/t.num) t.num = (s.num + i.num + e.num)
    #dQ <- ((i.num*qr)-((1/q.time)*q.num)) - (h.rate * q.num)
    dQ <- (t.num *t.prop*(i.num/t.num)) - ((1/q.time)*q.num) - (h.rate*q.num)
    #dH = hospitalized individuals
    dH <- ((i.num*h.rate) + (q.num * h.rate)) - ((1/h.dur)*h.num) - (h.num * cfr)
    #dF = Case Fatalities 
    dF <- (h.num * cfr)
    #Recovered Individuals 
    dR <- ((1/i.dur)*i.num) + ((1/q.time)*q.num) + ((1/h.dur*h.num))
    
    list(c(dS,dE,dI,dQ,dH,dF,dR))
  }) 
}


param <- param.dcm(R0 = 1.9, i.dur=14,q.time=14,t.prop=.25,q.freq=7,h.rate=.01,h.dur=14,
                   cfr=.001,e.dur=2)

init <- init.dcm(s.num = 500, e.num=3,i.num=0,q.num=0,h.num=0,f.num=0,
                 r.num=0)
control <- control.dcm(nsteps=1000,dt=1, new.mod = SEIQHFR)
mod <- dcm(param,init,control)
mod.df <- as.data.frame(mod)

plot1 <- ggplot(data=mod.df,aes(x=time))+
  geom_line(aes(y= s.num),color="steelblue",size=1)+
  geom_line(aes(y= i.num),color="gold",size=1)+
  geom_line(aes(y= q.num),color="darkorchid",size=1)+
  geom_line(aes(y= r.num),color="darkred",size=1)+
  geom_line(aes(y= h.num),color="brown2",size=1)+
  geom_line(aes(y= f.num),color="black",size=1)
plot1 + xlab("Time in Days")+
  ylab("Number of Individuals")

R0 <- c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
iDur <- c(6,7,8)
eDur <- c(2,3,4)

grid <- expand.grid(param1 = R0, param2 = iDur, param3 = eDur)

grid.df <- as.data.frame(grid)

#write_xlsx(the dataframe name,"path to store the Excel file\\file name.xlsx")
write_xlsx(grid.df,"C:\\Fall2020\\IndependentStudy\\modelParameters.xlsx")

R0.list <- grid$param1
iDur.list <- grid$param2
eDur.list <- grid$param3

param2 <- param.dcm(R0 = R0.list, i.dur=iDur.list,q.time=14,t.prop=.25,q.freq=7,h.rate=.01,h.dur=14,
                   cfr=.001,e.dur=eDur.list)

init2 <- init.dcm(s.num = 500, e.num=3,i.num=0,q.num=0,h.num=0,f.num=0,
                 r.num=0)
control2 <- control.dcm(nsteps=1000,dt=1, new.mod = SEIQHFR)
mod2 <- dcm(param2,init2,control2)
mod.df.2 <- as.data.frame(mod2)
mod2

param3 <- param.dcm(R0 = c(1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0), i.dur=7,q.time=14,t.prop=.25,q.freq=7,h.rate=.01,h.dur=14,
                    cfr=.001,e.dur=3)

init3 <- init.dcm(s.num = 500, e.num=3,i.num=0,q.num=0,h.num=0,f.num=0,
                  r.num=0)
control3 <- control.dcm(nsteps=1000,dt=1, new.mod = SEIQHFR)
mod3 <- dcm(param3,init3,control3)
mod.df.3 <- as.data.frame(mod3)
head(mod.df.3)

mod3

par(mfrow=c(1,1))
plot(mod3, y="i.num",main="Number Infected",legend="full")

plot <- ggplot(data=mod.df.3,aes(x=time,y=i.num, group = run))+
  geom_line(aes(color=as.factor(run)))
plot + scale_color_discrete(name="R0 Value",labels=c("1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8","1.9","2.0"))


run1 <- mod.df.3 %>% filter(run == 1)
summary(run1$run)
head(run1)

#i <- 1

#s[[i]] <- tail(s[[i]],n=1)
#s[[1]] 
#num.infected <- 0
#num.infected <- num.infected + split.last$i.num + split.last$q.num + split.last$h.num + split.last$f.num + split.last$r.num
#num.infected
#s[[1]]$num.infected <- num.infected
#s[[1]]
#s[[2]]

#Split data frame by run number
#s <- split(mod.df.3,mod.df.3$run)

#loop that pulls the last row and sums all the individuals who have either been infected, 
#are currently infected, have recovered from infection, and those who have died from covid 

RUN <- split(mod.df.3,mod.df.3$run)
RUN
for(i in seq(min(mod.df.3$run),max(mod.df.3$run))){
  RUN[[i]] <- tail(RUN[[i]],n=1)
  num.infected <- RUN[[i]]$i.num + RUN[[i]]$q.num + RUN[[i]]$h.num + RUN[[i]]$f.num + RUN[[i]]$r.num
  RUN[[i]]$num.infected <- num.infected
  num.infected <- 0
}

Run.list = c()
numInfected.list = c()
for(i in seq(min(mod.df.3$run),max(mod.df.3$run))){
  Run.list[i]<-RUN[[i]]$run
  numInfected.list[i]<-RUN[[i]]$num.infected
}
Run.list
numInfected.list

infectedRun <- data.frame(Run.list,numInfected.list)
infectedRun$R0 <- R0
infectedRun
infectedPlot <- ggplot(data=infectedRun,aes(x=R0,y=numInfected.list,fill=as.factor(R0)))+
  geom_bar(stat="identity")+
  ggtitle("Total Infected")+
  xlab("Run Number")+
  ylab("Number of individuals")+
  geom_text(aes(label=round(numInfected.list,digits=2)), vjust=1.6, color="black",
            position = position_dodge(0.9), size=2.5)+
  scale_fill_discrete(name = "R0 Value")
infectedPlot

contourplot(infectedRun$numInfected.list ~ infectedRun$R0,
            
            main = 'y = p1 + k*p2, k = 2 if p1 <5 or 4 otherwise\ninteraction effect (impact of changing p2 depends on p1)',
            
            region = TRUE)
