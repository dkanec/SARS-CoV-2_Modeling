
### Combined aerosol and close contact modules - base model
### Code updated 2.25.2021

#### Setup ####
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)


# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules -----------------
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-0
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-0
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "surgical"
i.index.mask<- mask
i.susceptible.mask<-mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "V", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

# Transportation Module --------------------------------------------------------

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Transport"

#Fine Tune interventions
ifelse(Module == "Transport", mask <- "surgical", mask <- "surgical")
i.index.mask<- mask
i.susceptible.mask<-mask

# Make sure it is in the same working directory as current code

ifelse(Module == "Transport", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Transport.R"),
       ifelse(Module == "Residential", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential.R"),
              ifelse(Module == "Outdoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Outdoor.R"),
                     ifelse(Module == "Bus", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210801_Bus.R"),
                            ifelse(Module == "Break", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Break.R"),
                                ifelse(Module == "ResBreath", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential_Breathing.R"),
                                   ifelse(Module == "Indoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Indoor.R"), source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R"))))))))

#source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R")

# Set controls/interventions

#Master control for ACH
set.seed(12345)
ifelse(Module == "Transport", m.room.exchange <- mcstoc(runif, type="V",min=0.92, max =4.1), 
       ifelse(Module == "Residential", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max =0.35),
              ifelse(Module == "Outdoor", m.room.exchange <- mcstoc(runif, type="V",min=4.568817715, max =4568.817715),
                     ifelse(Module == "Indoor", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max =0.1),
                            ifelse(Module == "Bus", m.room.exchange <- mcstoc(rtriang, type="V",min=8.335238095, mode = 10.94, max = 14.06571429),
                                   ifelse(Module == "Break", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max = 0.1),
                                          mcstoc(runif, type="V",min=0.1, max =0.1)))))))
#Increased ACH 
set.seed(12345)
ifelse(Module == "Transport" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 0.92, max = 72), 
       ifelse(Module == "Residential" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max =5.82),
              ifelse(Module == "Indoor" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 6, max = 6),
                     ifelse(Module == "Bus" & IncACH == "yes", m.room.exchange <- mcstoc(rtriang, type="V",min= 8.13375, mode = 14.46, max = 42.175),
                            ifelse(Module == "Break" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=6, max = 6),
                                   m.room.exchange <- m.room.exchange)))))
m.Humidity<-"high"

ifelse(Module == "Transport", m.Humidity <- "low",
       ifelse(Module == "Residential", m.Humidity <- "high",
              ifelse(Module == "Outdoor", m.Humidity <- "high",
                     ifelse(Module == "Bus", m.Humidity <- "low",
                            ifelse(Module == "Break", m.Humidity <- "low",
                                 ifelse(Module == "Indoor", m.Humidity <- "low", m.Humidity <- "high"))))))

# Aerosol function call

aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))
#View(aero.dose.clean)
#summary(aero.dose.clean$f4h)


# Close contact function call 

dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event, Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50603m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=3.0, Vol.Frac.Dist.Name = "50603m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))


dose5060_2m <- select(dose50602m,
                      starts_with("a"),
                      starts_with("f"))


dose60100_2m <- select(dose601002m,
                       starts_with("a"),
                       starts_with("f"))


dose5060_3m <- select(dose50603m,
                      starts_with("a"),
                      starts_with("f"))


# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)
dose3m <-cbind (dose5060_3m, aero.dose.clean)


dose1m <- mutate( dose1m, 
                  a1m1h = a50601m1h + a601001m1h + a1007501m1h + aero1h,
                  a1m2h = a50601m2h + a601001m2h + a1007501m2h + aero2h,
                  a1m3h = a50601m3h + a601001m3h + a1007501m3h + aero3h,
                  a1m4h = a50601m4h + a601001m4h + a1007501m4h + aero4h,
                  a1m5h = a50601m5h + a601001m5h + a1007501m5h + aero5h,
                  a1m6h = a50601m6h + a601001m6h + a1007501m6h + aero6h,
                  a1m7h = a50601m7h + a601001m7h + a1007501m7h + aero7h,
                  a1m8h = a50601m8h + a601001m8h + a1007501m8h + aero8h,
                  a1m9h = a50601m9h + a601001m9h + a1007501m9h + aero9h,
                  a1m10h = a50601m10h + a601001m10h + a1007501m10h + aero10h,
                  a1m11h = a50601m11h + a601001m11h + a1007501m11h + aero11h,
                  a1m12h = a50601m12h + a601001m12h + a1007501m12h + aero12h,
                  f1m1h = f50601m1h + f601001m1h + f1007501m1h + f1h,
                  f1m2h = f50601m2h + f601001m2h + f1007501m2h + f2h,
                  f1m3h = f50601m3h + f601001m3h + f1007501m3h + f3h,
                  f1m4h = f50601m4h + f601001m4h + f1007501m4h + f4h,
                  f1m5h = f50601m5h + f601001m5h + f1007501m5h + f5h,
                  f1m6h = f50601m6h + f601001m6h + f1007501m6h + f6h,
                  f1m7h = f50601m7h + f601001m7h + f1007501m7h + f7h,
                  f1m8h = f50601m8h + f601001m8h + f1007501m8h + f8h,
                  f1m9h = f50601m9h + f601001m9h + f1007501m9h + f9h,
                  f1m10h = f50601m10h + f601001m10h + f1007501m10h + f10h,
                  f1m11h = f50601m11h + f601001m11h + f1007501m11h + f11h,
                  f1m12h = f50601m12h + f601001m12h + f1007501m12h + f12h,
                  af1m1h = af50601m1h + af601001m1h + af1007501m1h + aerof1h,
                  af1m2h = af50601m2h + af601001m2h + af1007501m2h + aerof2h,
                  af1m3h = af50601m3h + af601001m3h + af1007501m3h + aerof3h,
                  af1m4h = af50601m4h + af601001m4h + af1007501m4h + aerof4h,
                  af1m5h = af50601m5h + af601001m5h + af1007501m5h + aerof5h,
                  af1m6h = af50601m6h + af601001m6h + af1007501m6h + aerof6h,
                  af1m7h = af50601m7h + af601001m7h + af1007501m7h + aerof7h,
                  af1m8h = af50601m8h + af601001m8h + af1007501m8h + aerof8h,
                  af1m9h = af50601m9h + af601001m9h + af1007501m9h + aerof9h,
                  af1m10h = af50601m10h + af601001m10h + af1007501m10h + aerof10h,
                  af1m11h = af50601m11h + af601001m11h + af1007501m11h + aerof11h,
                  af1m12h = af50601m12h + af601001m12h + af1007501m12h + aerof12h)
dose1m_risk <- select(dose1m, a1m1h:af1m12h)

dose2m <- mutate( dose2m, 
                  a2m1h = a50602m1h + a601002m1h + aero1h ,
                  a2m2h = a50602m2h + a601002m2h + aero2h,
                  a2m3h = a50602m3h + a601002m3h + aero3h,
                  a2m4h = a50602m4h + a601002m4h + aero4h,
                  a2m5h = a50602m5h + a601002m5h + aero5h,
                  a2m6h = a50602m6h + a601002m6h + aero6h,
                  a2m7h = a50602m7h + a601002m7h + aero7h,
                  a2m8h = a50602m8h + a601002m8h + aero8h,
                  a2m9h = a50602m9h + a601002m9h + aero9h,
                  a2m10h = a50602m10h + a601002m10h + aero10h,
                  a2m11h = a50602m11h + a601002m11h + aero11h,
                  a2m12h = a50602m12h + a601002m12h + aero12h,
                  f2m1h = f50602m1h + f601002m1h + f1h,
                  f2m2h = f50602m2h + f601002m2h + f2h,
                  f2m3h = f50602m3h + f601002m3h + f3h,
                  f2m4h = f50602m4h + f601002m4h + f4h,
                  f2m5h = f50602m5h + f601002m5h + f5h,
                  f2m6h = f50602m6h + f601002m6h + f6h,
                  f2m7h = f50602m7h + f601002m7h + f7h,
                  f2m8h = f50602m8h + f601002m8h + f8h,
                  f2m9h = f50602m9h + f601002m9h + f9h,
                  f2m10h = f50602m10h + f601002m10h + f10h,
                  f2m11h = f50602m11h + f601002m11h + f11h,
                  f2m12h = f50602m12h + f601002m12h + f12h,
                  af2m1h = af50602m1h + af601002m1h + aerof1h,
                  af2m2h = af50602m2h + af601002m2h + aerof2h,
                  af2m3h = af50602m3h + af601002m3h + aerof3h,
                  af2m4h = af50602m4h + af601002m4h + aerof4h,
                  af2m5h = af50602m5h + af601002m5h + aerof5h,
                  af2m6h = af50602m6h + af601002m6h + aerof6h,
                  af2m7h = af50602m7h + af601002m7h + aerof7h,
                  af2m8h = af50602m8h + af601002m8h + aerof8h, 
                  af2m9h = af50602m9h + af601002m9h + aerof9h, 
                  af2m10h = af50602m10h + af601002m10h + aerof10h, 
                  af2m11h = af50602m11h + af601002m11h + aerof11h, 
                  af2m12h = af50602m12h + af601002m12h + aerof12h)

dose2m_risk <- select(dose2m, a2m1h:af2m12h)

dose3m <- mutate( dose3m, 
                  a3m1h = a50603m1h + aero1h,
                  a3m2h = a50603m2h + aero2h,
                  a3m3h = a50603m3h + aero3h,
                  a3m4h = a50603m4h + aero4h,
                  a3m5h = a50603m5h + aero5h,
                  a3m6h = a50603m6h + aero6h,
                  a3m7h = a50603m7h + aero7h,
                  a3m8h = a50603m8h + aero8h,
                  a3m9h = a50603m9h + aero9h,
                  a3m10h = a50603m10h + aero10h,
                  a3m11h = a50603m11h + aero11h,
                  a3m12h = a50603m12h + aero12h,
                  f3m1h = f50603m1h + f1h,
                  f3m2h = f50603m2h + f2h,
                  f3m3h = f50603m3h + f3h,
                  f3m4h = f50603m4h + f4h,
                  f3m5h = f50603m5h + f5h,
                  f3m6h = f50603m6h + f6h,
                  f3m7h = f50603m7h + f7h,
                  f3m8h = f50603m8h + f8h,
                  f3m9h = f50603m9h + f9h,
                  f3m10h = f50603m10h + f10h,
                  f3m11h = f50603m11h + f11h,
                  f3m12h = f50603m12h + f12h,
                  af3m1h = af50603m1h + aerof1h,
                  af3m2h = af50603m2h + aerof2h,
                  af3m3h = af50603m3h + aerof3h,
                  af3m4h = af50603m4h + aerof4h,
                  af3m5h = af50603m5h + aerof5h,
                  af3m6h = af50603m6h + aerof6h,
                  af3m7h = af50603m7h + aerof7h,
                  af3m8h = af50603m8h + aerof8h,
                  af3m9h = af50603m9h + aerof9h,
                  af3m10h = af50603m10h + aerof10h,
                  af3m11h = af50603m11h + aerof11h,
                  af3m12h = af50603m12h + aerof12h)

dose3m_risk <- select(dose3m, a3m1h:af3m12h)

Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/CarTransport.csv", row.names = FALSE)

# Residential Coughing Module ------------------------
#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Residential"

#Fine Tune interventions
ifelse(Module == "Residential", mask <- "surgical", mask <- "surgical")
i.index.mask<- mask
i.susceptible.mask<-mask

# Make sure it is in the same working directory as current code

ifelse(Module == "Transport", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Transport.R"),
       ifelse(Module == "Residential", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential.R"),
              ifelse(Module == "Outdoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Outdoor.R"),
                     ifelse(Module == "Bus", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210801_Bus.R"),
                            ifelse(Module == "Break", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Break.R"),
                                   ifelse(Module == "ResBreath", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential_Breathing.R"),
                                          ifelse(Module == "Indoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Indoor.R"), source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R"))))))))

#source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R")

# Set controls/interventions

#Master control for ACH
set.seed(12345)
ifelse(Module == "Transport", m.room.exchange <- mcstoc(runif, type="V",min=0.92, max =4.1), 
       ifelse(Module == "Residential", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max =0.35),
              ifelse(Module == "Outdoor", m.room.exchange <- mcstoc(runif, type="V",min=4.568817715, max =4568.817715),
                     ifelse(Module == "Indoor", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max =0.1),
                            ifelse(Module == "Bus", m.room.exchange <- mcstoc(rtriang, type="V",min=8.335238095, mode = 10.94, max = 14.06571429),
                                   ifelse(Module == "Break", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max = 0.1),
                                          mcstoc(runif, type="V",min=0.1, max =0.1)))))))
#Increased ACH 
set.seed(12345)
ifelse(Module == "Transport" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 0.92, max = 72), 
       ifelse(Module == "Residential" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max =5.82),
              ifelse(Module == "Indoor" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 6, max = 6),
                     ifelse(Module == "Bus" & IncACH == "yes", m.room.exchange <- mcstoc(rtriang, type="V",min= 8.13375, mode = 14.46, max = 42.175),
                            ifelse(Module == "Break" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=6, max = 6),
                                   m.room.exchange <- m.room.exchange)))))

m.Humidity<-"high"

ifelse(Module == "Transport", m.Humidity <- "low",
       ifelse(Module == "Residential", m.Humidity <- "high",
              ifelse(Module == "Outdoor", m.Humidity <- "high",
                     ifelse(Module == "Bus", m.Humidity <- "low",
                            ifelse(Module == "Break", m.Humidity <- "low",
                                   ifelse(Module == "Indoor", m.Humidity <- "low", m.Humidity <- "high"))))))

# Aerosol function call

aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))
#View(aero.dose.clean)
#summary(aero.dose.clean$f4h)


# Close contact function call

dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event, Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50603m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=3.0, Vol.Frac.Dist.Name = "50603m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))


dose5060_2m <- select(dose50602m,
                      starts_with("a"),
                      starts_with("f"))


dose60100_2m <- select(dose601002m,
                       starts_with("a"),
                       starts_with("f"))


dose5060_3m <- select(dose50603m,
                      starts_with("a"),
                      starts_with("f"))


# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)
dose3m <-cbind (dose5060_3m, aero.dose.clean)


dose1m <- mutate( dose1m, 
                  a1m1h = a50601m1h + a601001m1h + a1007501m1h + aero1h,
                  a1m2h = a50601m2h + a601001m2h + a1007501m2h + aero2h,
                  a1m3h = a50601m3h + a601001m3h + a1007501m3h + aero3h,
                  a1m4h = a50601m4h + a601001m4h + a1007501m4h + aero4h,
                  a1m5h = a50601m5h + a601001m5h + a1007501m5h + aero5h,
                  a1m6h = a50601m6h + a601001m6h + a1007501m6h + aero6h,
                  a1m7h = a50601m7h + a601001m7h + a1007501m7h + aero7h,
                  a1m8h = a50601m8h + a601001m8h + a1007501m8h + aero8h,
                  a1m9h = a50601m9h + a601001m9h + a1007501m9h + aero9h,
                  a1m10h = a50601m10h + a601001m10h + a1007501m10h + aero10h,
                  a1m11h = a50601m11h + a601001m11h + a1007501m11h + aero11h,
                  a1m12h = a50601m12h + a601001m12h + a1007501m12h + aero12h,
                  f1m1h = f50601m1h + f601001m1h + f1007501m1h + f1h,
                  f1m2h = f50601m2h + f601001m2h + f1007501m2h + f2h,
                  f1m3h = f50601m3h + f601001m3h + f1007501m3h + f3h,
                  f1m4h = f50601m4h + f601001m4h + f1007501m4h + f4h,
                  f1m5h = f50601m5h + f601001m5h + f1007501m5h + f5h,
                  f1m6h = f50601m6h + f601001m6h + f1007501m6h + f6h,
                  f1m7h = f50601m7h + f601001m7h + f1007501m7h + f7h,
                  f1m8h = f50601m8h + f601001m8h + f1007501m8h + f8h,
                  f1m9h = f50601m9h + f601001m9h + f1007501m9h + f9h,
                  f1m10h = f50601m10h + f601001m10h + f1007501m10h + f10h,
                  f1m11h = f50601m11h + f601001m11h + f1007501m11h + f11h,
                  f1m12h = f50601m12h + f601001m12h + f1007501m12h + f12h,
                  af1m1h = af50601m1h + af601001m1h + af1007501m1h + aerof1h,
                  af1m2h = af50601m2h + af601001m2h + af1007501m2h + aerof2h,
                  af1m3h = af50601m3h + af601001m3h + af1007501m3h + aerof3h,
                  af1m4h = af50601m4h + af601001m4h + af1007501m4h + aerof4h,
                  af1m5h = af50601m5h + af601001m5h + af1007501m5h + aerof5h,
                  af1m6h = af50601m6h + af601001m6h + af1007501m6h + aerof6h,
                  af1m7h = af50601m7h + af601001m7h + af1007501m7h + aerof7h,
                  af1m8h = af50601m8h + af601001m8h + af1007501m8h + aerof8h,
                  af1m9h = af50601m9h + af601001m9h + af1007501m9h + aerof9h,
                  af1m10h = af50601m10h + af601001m10h + af1007501m10h + aerof10h,
                  af1m11h = af50601m11h + af601001m11h + af1007501m11h + aerof11h,
                  af1m12h = af50601m12h + af601001m12h + af1007501m12h + aerof12h)
dose1m_risk <- select(dose1m, a1m1h:af1m12h)

dose2m <- mutate( dose2m, 
                  a2m1h = a50602m1h + a601002m1h + aero1h ,
                  a2m2h = a50602m2h + a601002m2h + aero2h,
                  a2m3h = a50602m3h + a601002m3h + aero3h,
                  a2m4h = a50602m4h + a601002m4h + aero4h,
                  a2m5h = a50602m5h + a601002m5h + aero5h,
                  a2m6h = a50602m6h + a601002m6h + aero6h,
                  a2m7h = a50602m7h + a601002m7h + aero7h,
                  a2m8h = a50602m8h + a601002m8h + aero8h,
                  a2m9h = a50602m9h + a601002m9h + aero9h,
                  a2m10h = a50602m10h + a601002m10h + aero10h,
                  a2m11h = a50602m11h + a601002m11h + aero11h,
                  a2m12h = a50602m12h + a601002m12h + aero12h,
                  f2m1h = f50602m1h + f601002m1h + f1h,
                  f2m2h = f50602m2h + f601002m2h + f2h,
                  f2m3h = f50602m3h + f601002m3h + f3h,
                  f2m4h = f50602m4h + f601002m4h + f4h,
                  f2m5h = f50602m5h + f601002m5h + f5h,
                  f2m6h = f50602m6h + f601002m6h + f6h,
                  f2m7h = f50602m7h + f601002m7h + f7h,
                  f2m8h = f50602m8h + f601002m8h + f8h,
                  f2m9h = f50602m9h + f601002m9h + f9h,
                  f2m10h = f50602m10h + f601002m10h + f10h,
                  f2m11h = f50602m11h + f601002m11h + f11h,
                  f2m12h = f50602m12h + f601002m12h + f12h,
                  af2m1h = af50602m1h + af601002m1h + aerof1h,
                  af2m2h = af50602m2h + af601002m2h + aerof2h,
                  af2m3h = af50602m3h + af601002m3h + aerof3h,
                  af2m4h = af50602m4h + af601002m4h + aerof4h,
                  af2m5h = af50602m5h + af601002m5h + aerof5h,
                  af2m6h = af50602m6h + af601002m6h + aerof6h,
                  af2m7h = af50602m7h + af601002m7h + aerof7h,
                  af2m8h = af50602m8h + af601002m8h + aerof8h, 
                  af2m9h = af50602m9h + af601002m9h + aerof9h, 
                  af2m10h = af50602m10h + af601002m10h + aerof10h, 
                  af2m11h = af50602m11h + af601002m11h + aerof11h, 
                  af2m12h = af50602m12h + af601002m12h + aerof12h)

dose2m_risk <- select(dose2m, a2m1h:af2m12h)

dose3m <- mutate( dose3m, 
                  a3m1h = a50603m1h + aero1h,
                  a3m2h = a50603m2h + aero2h,
                  a3m3h = a50603m3h + aero3h,
                  a3m4h = a50603m4h + aero4h,
                  a3m5h = a50603m5h + aero5h,
                  a3m6h = a50603m6h + aero6h,
                  a3m7h = a50603m7h + aero7h,
                  a3m8h = a50603m8h + aero8h,
                  a3m9h = a50603m9h + aero9h,
                  a3m10h = a50603m10h + aero10h,
                  a3m11h = a50603m11h + aero11h,
                  a3m12h = a50603m12h + aero12h,
                  f3m1h = f50603m1h + f1h,
                  f3m2h = f50603m2h + f2h,
                  f3m3h = f50603m3h + f3h,
                  f3m4h = f50603m4h + f4h,
                  f3m5h = f50603m5h + f5h,
                  f3m6h = f50603m6h + f6h,
                  f3m7h = f50603m7h + f7h,
                  f3m8h = f50603m8h + f8h,
                  f3m9h = f50603m9h + f9h,
                  f3m10h = f50603m10h + f10h,
                  f3m11h = f50603m11h + f11h,
                  f3m12h = f50603m12h + f12h,
                  af3m1h = af50603m1h + aerof1h,
                  af3m2h = af50603m2h + aerof2h,
                  af3m3h = af50603m3h + aerof3h,
                  af3m4h = af50603m4h + aerof4h,
                  af3m5h = af50603m5h + aerof5h,
                  af3m6h = af50603m6h + aerof6h,
                  af3m7h = af50603m7h + aerof7h,
                  af3m8h = af50603m8h + aerof8h,
                  af3m9h = af50603m9h + aerof9h,
                  af3m10h = af50603m10h + aerof10h,
                  af3m11h = af50603m11h + aerof11h,
                  af3m12h = af50603m12h + aerof12h)

dose3m_risk <- select(dose3m, a3m1h:af3m12h)

ResCough_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
write.csv(ResCough_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/ResCough.csv", row.names = FALSE)

# Residential Breathing Module ----------------
#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "ResBreath"

#Fine Tune interventions
ifelse(Module == "ResBreath", mask <- "none", mask <- "surgical")
i.index.mask<- mask
i.susceptible.mask<-mask


# Make sure it is in the same working directory as current code

ifelse(Module == "Transport", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Transport.R"),
       ifelse(Module == "Residential", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential.R"),
              ifelse(Module == "Outdoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Outdoor.R"),
                     ifelse(Module == "Bus", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210801_Bus.R"),
                            ifelse(Module == "Break", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Break.R"),
                                   ifelse(Module == "ResBreath", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential_Breathing.R"),
                                          ifelse(Module == "Indoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Indoor.R"), source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R"))))))))

#source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R")

# Set controls/interventions

#Master control for ACH
set.seed(12345)
ifelse(Module == "Transport", m.room.exchange <- mcstoc(runif, type="V",min=0.92, max =4.1), 
       ifelse(Module == "Residential", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max =0.35),
              ifelse(Module == "Outdoor", m.room.exchange <- mcstoc(runif, type="V",min=4.568817715, max =4568.817715),
                     ifelse(Module == "Indoor", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max =0.1),
                            ifelse(Module == "Bus", m.room.exchange <- mcstoc(rtriang, type="V",min=8.335238095, mode = 10.94, max = 14.06571429),
                                   ifelse(Module == "Break", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max = 0.1),
                                          ifelse(Module == "ResBreath", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max = 0.35),
                                                 mcstoc(runif, type="V",min=0.1, max =0.1))))))))
#Increased ACH 
set.seed(12345)
ifelse(Module == "Transport" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 0.92, max = 72), 
       ifelse(Module == "Residential" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max =5.82),
              ifelse(Module == "Indoor" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 6, max = 6),
                     ifelse(Module == "Bus" & IncACH == "yes", m.room.exchange <- mcstoc(rtriang, type="V",min= 8.13375, mode = 14.46, max = 42.175),
                            ifelse(Module == "Break" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=6, max = 6),
                                   ifelse(Module == "ResBreath" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max = 5.82),
                                          m.room.exchange <- m.room.exchange))))))

m.Humidity<-"high"

ifelse(Module == "Transport", m.Humidity <- "low",
       ifelse(Module == "Residential", m.Humidity <- "high",
              ifelse(Module == "Outdoor", m.Humidity <- "high",
                     ifelse(Module == "Bus", m.Humidity <- "low",
                            ifelse(Module == "Break", m.Humidity <- "low",
                                   ifelse(Module == "Indoor", m.Humidity <- "low", 
                                          ifelse(Module == "ResBreath", m.Humidity <- "high", m.Humidity <- "high")))))))
# Aerosol function call

aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))
#View(aero.dose.clean)
#summary(aero.dose.clean$f4h)


# Close contact function call

dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event, Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50603m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=3.0, Vol.Frac.Dist.Name = "50603m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))


dose5060_2m <- select(dose50602m,
                      starts_with("a"),
                      starts_with("f"))


dose60100_2m <- select(dose601002m,
                       starts_with("a"),
                       starts_with("f"))


dose5060_3m <- select(dose50603m,
                      starts_with("a"),
                      starts_with("f"))


# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)
dose3m <-cbind (dose5060_3m, aero.dose.clean)


dose1m <- mutate( dose1m, 
                  a1m1h = a50601m1h + a601001m1h + a1007501m1h + aero1h,
                  a1m2h = a50601m2h + a601001m2h + a1007501m2h + aero2h,
                  a1m3h = a50601m3h + a601001m3h + a1007501m3h + aero3h,
                  a1m4h = a50601m4h + a601001m4h + a1007501m4h + aero4h,
                  a1m5h = a50601m5h + a601001m5h + a1007501m5h + aero5h,
                  a1m6h = a50601m6h + a601001m6h + a1007501m6h + aero6h,
                  a1m7h = a50601m7h + a601001m7h + a1007501m7h + aero7h,
                  a1m8h = a50601m8h + a601001m8h + a1007501m8h + aero8h,
                  a1m9h = a50601m9h + a601001m9h + a1007501m9h + aero9h,
                  a1m10h = a50601m10h + a601001m10h + a1007501m10h + aero10h,
                  a1m11h = a50601m11h + a601001m11h + a1007501m11h + aero11h,
                  a1m12h = a50601m12h + a601001m12h + a1007501m12h + aero12h,
                  f1m1h = f50601m1h + f601001m1h + f1007501m1h + f1h,
                  f1m2h = f50601m2h + f601001m2h + f1007501m2h + f2h,
                  f1m3h = f50601m3h + f601001m3h + f1007501m3h + f3h,
                  f1m4h = f50601m4h + f601001m4h + f1007501m4h + f4h,
                  f1m5h = f50601m5h + f601001m5h + f1007501m5h + f5h,
                  f1m6h = f50601m6h + f601001m6h + f1007501m6h + f6h,
                  f1m7h = f50601m7h + f601001m7h + f1007501m7h + f7h,
                  f1m8h = f50601m8h + f601001m8h + f1007501m8h + f8h,
                  f1m9h = f50601m9h + f601001m9h + f1007501m9h + f9h,
                  f1m10h = f50601m10h + f601001m10h + f1007501m10h + f10h,
                  f1m11h = f50601m11h + f601001m11h + f1007501m11h + f11h,
                  f1m12h = f50601m12h + f601001m12h + f1007501m12h + f12h,
                  af1m1h = af50601m1h + af601001m1h + af1007501m1h + aerof1h,
                  af1m2h = af50601m2h + af601001m2h + af1007501m2h + aerof2h,
                  af1m3h = af50601m3h + af601001m3h + af1007501m3h + aerof3h,
                  af1m4h = af50601m4h + af601001m4h + af1007501m4h + aerof4h,
                  af1m5h = af50601m5h + af601001m5h + af1007501m5h + aerof5h,
                  af1m6h = af50601m6h + af601001m6h + af1007501m6h + aerof6h,
                  af1m7h = af50601m7h + af601001m7h + af1007501m7h + aerof7h,
                  af1m8h = af50601m8h + af601001m8h + af1007501m8h + aerof8h,
                  af1m9h = af50601m9h + af601001m9h + af1007501m9h + aerof9h,
                  af1m10h = af50601m10h + af601001m10h + af1007501m10h + aerof10h,
                  af1m11h = af50601m11h + af601001m11h + af1007501m11h + aerof11h,
                  af1m12h = af50601m12h + af601001m12h + af1007501m12h + aerof12h)
dose1m_risk <- select(dose1m, a1m1h:af1m12h)

dose2m <- mutate( dose2m, 
                  a2m1h = a50602m1h + a601002m1h + aero1h ,
                  a2m2h = a50602m2h + a601002m2h + aero2h,
                  a2m3h = a50602m3h + a601002m3h + aero3h,
                  a2m4h = a50602m4h + a601002m4h + aero4h,
                  a2m5h = a50602m5h + a601002m5h + aero5h,
                  a2m6h = a50602m6h + a601002m6h + aero6h,
                  a2m7h = a50602m7h + a601002m7h + aero7h,
                  a2m8h = a50602m8h + a601002m8h + aero8h,
                  a2m9h = a50602m9h + a601002m9h + aero9h,
                  a2m10h = a50602m10h + a601002m10h + aero10h,
                  a2m11h = a50602m11h + a601002m11h + aero11h,
                  a2m12h = a50602m12h + a601002m12h + aero12h,
                  f2m1h = f50602m1h + f601002m1h + f1h,
                  f2m2h = f50602m2h + f601002m2h + f2h,
                  f2m3h = f50602m3h + f601002m3h + f3h,
                  f2m4h = f50602m4h + f601002m4h + f4h,
                  f2m5h = f50602m5h + f601002m5h + f5h,
                  f2m6h = f50602m6h + f601002m6h + f6h,
                  f2m7h = f50602m7h + f601002m7h + f7h,
                  f2m8h = f50602m8h + f601002m8h + f8h,
                  f2m9h = f50602m9h + f601002m9h + f9h,
                  f2m10h = f50602m10h + f601002m10h + f10h,
                  f2m11h = f50602m11h + f601002m11h + f11h,
                  f2m12h = f50602m12h + f601002m12h + f12h,
                  af2m1h = af50602m1h + af601002m1h + aerof1h,
                  af2m2h = af50602m2h + af601002m2h + aerof2h,
                  af2m3h = af50602m3h + af601002m3h + aerof3h,
                  af2m4h = af50602m4h + af601002m4h + aerof4h,
                  af2m5h = af50602m5h + af601002m5h + aerof5h,
                  af2m6h = af50602m6h + af601002m6h + aerof6h,
                  af2m7h = af50602m7h + af601002m7h + aerof7h,
                  af2m8h = af50602m8h + af601002m8h + aerof8h, 
                  af2m9h = af50602m9h + af601002m9h + aerof9h, 
                  af2m10h = af50602m10h + af601002m10h + aerof10h, 
                  af2m11h = af50602m11h + af601002m11h + aerof11h, 
                  af2m12h = af50602m12h + af601002m12h + aerof12h)

dose2m_risk <- select(dose2m, a2m1h:af2m12h)

dose3m <- mutate( dose3m, 
                  a3m1h = a50603m1h + aero1h,
                  a3m2h = a50603m2h + aero2h,
                  a3m3h = a50603m3h + aero3h,
                  a3m4h = a50603m4h + aero4h,
                  a3m5h = a50603m5h + aero5h,
                  a3m6h = a50603m6h + aero6h,
                  a3m7h = a50603m7h + aero7h,
                  a3m8h = a50603m8h + aero8h,
                  a3m9h = a50603m9h + aero9h,
                  a3m10h = a50603m10h + aero10h,
                  a3m11h = a50603m11h + aero11h,
                  a3m12h = a50603m12h + aero12h,
                  f3m1h = f50603m1h + f1h,
                  f3m2h = f50603m2h + f2h,
                  f3m3h = f50603m3h + f3h,
                  f3m4h = f50603m4h + f4h,
                  f3m5h = f50603m5h + f5h,
                  f3m6h = f50603m6h + f6h,
                  f3m7h = f50603m7h + f7h,
                  f3m8h = f50603m8h + f8h,
                  f3m9h = f50603m9h + f9h,
                  f3m10h = f50603m10h + f10h,
                  f3m11h = f50603m11h + f11h,
                  f3m12h = f50603m12h + f12h,
                  af3m1h = af50603m1h + aerof1h,
                  af3m2h = af50603m2h + aerof2h,
                  af3m3h = af50603m3h + aerof3h,
                  af3m4h = af50603m4h + aerof4h,
                  af3m5h = af50603m5h + aerof5h,
                  af3m6h = af50603m6h + aerof6h,
                  af3m7h = af50603m7h + aerof7h,
                  af3m8h = af50603m8h + aerof8h,
                  af3m9h = af50603m9h + aerof9h,
                  af3m10h = af50603m10h + aerof10h,
                  af3m11h = af50603m11h + aerof11h,
                  af3m12h = af50603m12h + aerof12h)

dose3m_risk <- select(dose3m, a3m1h:af3m12h)

ResBreath_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
write.csv(ResBreath_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/ResBreath.csv", row.names = FALSE)

# Outdoor Module -----------------------------------
#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Outdoor"

#Turning mask switch back on
ifelse(Module == "Outdoor", mask <- "surgical", mask <- "surgical")
i.index.mask<- mask
i.susceptible.mask<-mask


# Make sure it is in the same working directory as current code

ifelse(Module == "Transport", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Transport.R"),
       ifelse(Module == "Residential", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential.R"),
              ifelse(Module == "Outdoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Outdoor.R"),
                     ifelse(Module == "Bus", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210801_Bus.R"),
                            ifelse(Module == "Break", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Break.R"),
                                   ifelse(Module == "ResBreath", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential_Breathing.R"),
                                          ifelse(Module == "Indoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Indoor.R"), source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R"))))))))

#source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R")

# Set controls/interventions

#Master control for ACH
set.seed(12345)
ifelse(Module == "Transport", m.room.exchange <- mcstoc(runif, type="V",min=0.92, max =4.1), 
       ifelse(Module == "Residential", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max =0.35),
              ifelse(Module == "Outdoor", m.room.exchange <- mcstoc(rtriang, type = "V", min = 456.8817715, mode = 1142.204429, max = 1370.645315),
                     ifelse(Module == "Indoor", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max =0.1),
                            ifelse(Module == "Bus", m.room.exchange <- mcstoc(rtriang, type="V",min=8.335238095, mode = 10.94, max = 14.06571429),
                                   ifelse(Module == "Break", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max = 0.1),
                                          mcstoc(runif, type="V",min=0.1, max =0.1)))))))
#Increased ACH 
set.seed(12345)
ifelse(Module == "Transport" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 0.92, max = 72), 
       ifelse(Module == "Residential" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max =5.82),
              ifelse(Module == "Indoor" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 6, max = 6),
                     ifelse(Module == "Bus" & IncACH == "yes", m.room.exchange <- mcstoc(rtriang, type="V",min= 8.13375, mode = 14.46, max = 42.175),
                            ifelse(Module == "Break" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=6, max = 6),
                                   m.room.exchange <- m.room.exchange)))))

m.Humidity<-"high"

ifelse(Module == "Transport", m.Humidity <- "low",
       ifelse(Module == "Residential", m.Humidity <- "high",
              ifelse(Module == "Outdoor", m.Humidity <- "high",
                     ifelse(Module == "Bus", m.Humidity <- "low",
                            ifelse(Module == "Break", m.Humidity <- "low",
                                   ifelse(Module == "Indoor", m.Humidity <- "low", m.Humidity <- "high"))))))

# Aerosol function call

aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))
#View(aero.dose.clean)
#summary(aero.dose.clean$f4h)


# Close contact function call

dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event, Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50603m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=3.0, Vol.Frac.Dist.Name = "50603m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))


dose5060_2m <- select(dose50602m,
                      starts_with("a"),
                      starts_with("f"))


dose60100_2m <- select(dose601002m,
                       starts_with("a"),
                       starts_with("f"))


dose5060_3m <- select(dose50603m,
                      starts_with("a"),
                      starts_with("f"))


# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)
dose3m <-cbind (dose5060_3m, aero.dose.clean)


dose1m <- mutate( dose1m, 
                  a1m1h = a50601m1h + a601001m1h + a1007501m1h + aero1h,
                  a1m2h = a50601m2h + a601001m2h + a1007501m2h + aero2h,
                  a1m3h = a50601m3h + a601001m3h + a1007501m3h + aero3h,
                  a1m4h = a50601m4h + a601001m4h + a1007501m4h + aero4h,
                  a1m5h = a50601m5h + a601001m5h + a1007501m5h + aero5h,
                  a1m6h = a50601m6h + a601001m6h + a1007501m6h + aero6h,
                  a1m7h = a50601m7h + a601001m7h + a1007501m7h + aero7h,
                  a1m8h = a50601m8h + a601001m8h + a1007501m8h + aero8h,
                  a1m9h = a50601m9h + a601001m9h + a1007501m9h + aero9h,
                  a1m10h = a50601m10h + a601001m10h + a1007501m10h + aero10h,
                  a1m11h = a50601m11h + a601001m11h + a1007501m11h + aero11h,
                  a1m12h = a50601m12h + a601001m12h + a1007501m12h + aero12h,
                  f1m1h = f50601m1h + f601001m1h + f1007501m1h + f1h,
                  f1m2h = f50601m2h + f601001m2h + f1007501m2h + f2h,
                  f1m3h = f50601m3h + f601001m3h + f1007501m3h + f3h,
                  f1m4h = f50601m4h + f601001m4h + f1007501m4h + f4h,
                  f1m5h = f50601m5h + f601001m5h + f1007501m5h + f5h,
                  f1m6h = f50601m6h + f601001m6h + f1007501m6h + f6h,
                  f1m7h = f50601m7h + f601001m7h + f1007501m7h + f7h,
                  f1m8h = f50601m8h + f601001m8h + f1007501m8h + f8h,
                  f1m9h = f50601m9h + f601001m9h + f1007501m9h + f9h,
                  f1m10h = f50601m10h + f601001m10h + f1007501m10h + f10h,
                  f1m11h = f50601m11h + f601001m11h + f1007501m11h + f11h,
                  f1m12h = f50601m12h + f601001m12h + f1007501m12h + f12h,
                  af1m1h = af50601m1h + af601001m1h + af1007501m1h + aerof1h,
                  af1m2h = af50601m2h + af601001m2h + af1007501m2h + aerof2h,
                  af1m3h = af50601m3h + af601001m3h + af1007501m3h + aerof3h,
                  af1m4h = af50601m4h + af601001m4h + af1007501m4h + aerof4h,
                  af1m5h = af50601m5h + af601001m5h + af1007501m5h + aerof5h,
                  af1m6h = af50601m6h + af601001m6h + af1007501m6h + aerof6h,
                  af1m7h = af50601m7h + af601001m7h + af1007501m7h + aerof7h,
                  af1m8h = af50601m8h + af601001m8h + af1007501m8h + aerof8h,
                  af1m9h = af50601m9h + af601001m9h + af1007501m9h + aerof9h,
                  af1m10h = af50601m10h + af601001m10h + af1007501m10h + aerof10h,
                  af1m11h = af50601m11h + af601001m11h + af1007501m11h + aerof11h,
                  af1m12h = af50601m12h + af601001m12h + af1007501m12h + aerof12h)
dose1m_risk <- select(dose1m, a1m1h:af1m12h)

dose2m <- mutate( dose2m, 
                  a2m1h = a50602m1h + a601002m1h + aero1h ,
                  a2m2h = a50602m2h + a601002m2h + aero2h,
                  a2m3h = a50602m3h + a601002m3h + aero3h,
                  a2m4h = a50602m4h + a601002m4h + aero4h,
                  a2m5h = a50602m5h + a601002m5h + aero5h,
                  a2m6h = a50602m6h + a601002m6h + aero6h,
                  a2m7h = a50602m7h + a601002m7h + aero7h,
                  a2m8h = a50602m8h + a601002m8h + aero8h,
                  a2m9h = a50602m9h + a601002m9h + aero9h,
                  a2m10h = a50602m10h + a601002m10h + aero10h,
                  a2m11h = a50602m11h + a601002m11h + aero11h,
                  a2m12h = a50602m12h + a601002m12h + aero12h,
                  f2m1h = f50602m1h + f601002m1h + f1h,
                  f2m2h = f50602m2h + f601002m2h + f2h,
                  f2m3h = f50602m3h + f601002m3h + f3h,
                  f2m4h = f50602m4h + f601002m4h + f4h,
                  f2m5h = f50602m5h + f601002m5h + f5h,
                  f2m6h = f50602m6h + f601002m6h + f6h,
                  f2m7h = f50602m7h + f601002m7h + f7h,
                  f2m8h = f50602m8h + f601002m8h + f8h,
                  f2m9h = f50602m9h + f601002m9h + f9h,
                  f2m10h = f50602m10h + f601002m10h + f10h,
                  f2m11h = f50602m11h + f601002m11h + f11h,
                  f2m12h = f50602m12h + f601002m12h + f12h,
                  af2m1h = af50602m1h + af601002m1h + aerof1h,
                  af2m2h = af50602m2h + af601002m2h + aerof2h,
                  af2m3h = af50602m3h + af601002m3h + aerof3h,
                  af2m4h = af50602m4h + af601002m4h + aerof4h,
                  af2m5h = af50602m5h + af601002m5h + aerof5h,
                  af2m6h = af50602m6h + af601002m6h + aerof6h,
                  af2m7h = af50602m7h + af601002m7h + aerof7h,
                  af2m8h = af50602m8h + af601002m8h + aerof8h, 
                  af2m9h = af50602m9h + af601002m9h + aerof9h, 
                  af2m10h = af50602m10h + af601002m10h + aerof10h, 
                  af2m11h = af50602m11h + af601002m11h + aerof11h, 
                  af2m12h = af50602m12h + af601002m12h + aerof12h)

dose2m_risk <- select(dose2m, a2m1h:af2m12h)

dose3m <- mutate( dose3m, 
                  a3m1h = a50603m1h + aero1h,
                  a3m2h = a50603m2h + aero2h,
                  a3m3h = a50603m3h + aero3h,
                  a3m4h = a50603m4h + aero4h,
                  a3m5h = a50603m5h + aero5h,
                  a3m6h = a50603m6h + aero6h,
                  a3m7h = a50603m7h + aero7h,
                  a3m8h = a50603m8h + aero8h,
                  a3m9h = a50603m9h + aero9h,
                  a3m10h = a50603m10h + aero10h,
                  a3m11h = a50603m11h + aero11h,
                  a3m12h = a50603m12h + aero12h,
                  f3m1h = f50603m1h + f1h,
                  f3m2h = f50603m2h + f2h,
                  f3m3h = f50603m3h + f3h,
                  f3m4h = f50603m4h + f4h,
                  f3m5h = f50603m5h + f5h,
                  f3m6h = f50603m6h + f6h,
                  f3m7h = f50603m7h + f7h,
                  f3m8h = f50603m8h + f8h,
                  f3m9h = f50603m9h + f9h,
                  f3m10h = f50603m10h + f10h,
                  f3m11h = f50603m11h + f11h,
                  f3m12h = f50603m12h + f12h,
                  af3m1h = af50603m1h + aerof1h,
                  af3m2h = af50603m2h + aerof2h,
                  af3m3h = af50603m3h + aerof3h,
                  af3m4h = af50603m4h + aerof4h,
                  af3m5h = af50603m5h + aerof5h,
                  af3m6h = af50603m6h + aerof6h,
                  af3m7h = af50603m7h + aerof7h,
                  af3m8h = af50603m8h + aerof8h,
                  af3m9h = af50603m9h + aerof9h,
                  af3m10h = af50603m10h + aerof10h,
                  af3m11h = af50603m11h + aerof11h,
                  af3m12h = af50603m12h + aerof12h)

dose3m_risk <- select(dose3m, a3m1h:af3m12h)

Outdoor_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
write.csv(Outdoor_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Outdoor.csv", row.names = FALSE)

# Bus Module -----------------------------------
#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Bus"

#Fine Tune interventions
ifelse(Module == "Bus", mask <- "surgical", mask <- "surgical")
i.index.mask<- mask
i.susceptible.mask<-mask

# Make sure it is in the same working directory as current code

ifelse(Module == "Transport", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Transport.R"),
       ifelse(Module == "Residential", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential.R"),
              ifelse(Module == "Outdoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Outdoor.R"),
                     ifelse(Module == "Bus", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210801_Bus.R"),
                            ifelse(Module == "Break", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Break.R"),
                                   ifelse(Module == "ResBreath", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential_Breathing.R"),
                                          ifelse(Module == "Indoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Indoor.R"), source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R"))))))))

#source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R")

# Set controls/interventions

#Master control for ACH
set.seed(12345)
ifelse(Module == "Transport", m.room.exchange <- mcstoc(runif, type="V",min=0.92, max =4.1), 
       ifelse(Module == "Residential", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max =0.35),
              ifelse(Module == "Outdoor", m.room.exchange <- mcstoc(runif, type="V",min=4.568817715, max =4568.817715),
                     ifelse(Module == "Indoor", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max =0.1),
                            ifelse(Module == "Bus", m.room.exchange <- mcstoc(rtriang, type="V",min=8.335238095, mode = 10.94, max = 14.06571429),
                                   ifelse(Module == "Break", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max = 0.1),
                                          mcstoc(runif, type="V",min=0.1, max =0.1)))))))
#Increased ACH 
set.seed(12345)
ifelse(Module == "Transport" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 0.92, max = 72), 
       ifelse(Module == "Residential" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max =5.82),
              ifelse(Module == "Indoor" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 6, max = 6),
                     ifelse(Module == "Bus" & IncACH == "yes", m.room.exchange <- mcstoc(rtriang, type="V",min= 8.13375, mode = 14.46, max = 42.175),
                            ifelse(Module == "Break" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=6, max = 6),
                                   m.room.exchange <- m.room.exchange)))))

m.Humidity<-"high"

ifelse(Module == "Transport", m.Humidity <- "low",
       ifelse(Module == "Residential", m.Humidity <- "high",
              ifelse(Module == "Outdoor", m.Humidity <- "high",
                     ifelse(Module == "Bus", m.Humidity <- "low",
                            ifelse(Module == "Break", m.Humidity <- "low",
                                   ifelse(Module == "Indoor", m.Humidity <- "low", m.Humidity <- "high"))))))

# Aerosol function call

aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))
#View(aero.dose.clean)
#summary(aero.dose.clean$f4h)


# Close contact function call

dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event, Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50603m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=3.0, Vol.Frac.Dist.Name = "50603m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))


dose5060_2m <- select(dose50602m,
                      starts_with("a"),
                      starts_with("f"))


dose60100_2m <- select(dose601002m,
                       starts_with("a"),
                       starts_with("f"))


dose5060_3m <- select(dose50603m,
                      starts_with("a"),
                      starts_with("f"))


# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)
dose3m <-cbind (dose5060_3m, aero.dose.clean)


dose1m <- mutate( dose1m, 
                  a1m1h = a50601m1h + a601001m1h + a1007501m1h + aero1h,
                  a1m2h = a50601m2h + a601001m2h + a1007501m2h + aero2h,
                  a1m3h = a50601m3h + a601001m3h + a1007501m3h + aero3h,
                  a1m4h = a50601m4h + a601001m4h + a1007501m4h + aero4h,
                  a1m5h = a50601m5h + a601001m5h + a1007501m5h + aero5h,
                  a1m6h = a50601m6h + a601001m6h + a1007501m6h + aero6h,
                  a1m7h = a50601m7h + a601001m7h + a1007501m7h + aero7h,
                  a1m8h = a50601m8h + a601001m8h + a1007501m8h + aero8h,
                  a1m9h = a50601m9h + a601001m9h + a1007501m9h + aero9h,
                  a1m10h = a50601m10h + a601001m10h + a1007501m10h + aero10h,
                  a1m11h = a50601m11h + a601001m11h + a1007501m11h + aero11h,
                  a1m12h = a50601m12h + a601001m12h + a1007501m12h + aero12h,
                  f1m1h = f50601m1h + f601001m1h + f1007501m1h + f1h,
                  f1m2h = f50601m2h + f601001m2h + f1007501m2h + f2h,
                  f1m3h = f50601m3h + f601001m3h + f1007501m3h + f3h,
                  f1m4h = f50601m4h + f601001m4h + f1007501m4h + f4h,
                  f1m5h = f50601m5h + f601001m5h + f1007501m5h + f5h,
                  f1m6h = f50601m6h + f601001m6h + f1007501m6h + f6h,
                  f1m7h = f50601m7h + f601001m7h + f1007501m7h + f7h,
                  f1m8h = f50601m8h + f601001m8h + f1007501m8h + f8h,
                  f1m9h = f50601m9h + f601001m9h + f1007501m9h + f9h,
                  f1m10h = f50601m10h + f601001m10h + f1007501m10h + f10h,
                  f1m11h = f50601m11h + f601001m11h + f1007501m11h + f11h,
                  f1m12h = f50601m12h + f601001m12h + f1007501m12h + f12h,
                  af1m1h = af50601m1h + af601001m1h + af1007501m1h + aerof1h,
                  af1m2h = af50601m2h + af601001m2h + af1007501m2h + aerof2h,
                  af1m3h = af50601m3h + af601001m3h + af1007501m3h + aerof3h,
                  af1m4h = af50601m4h + af601001m4h + af1007501m4h + aerof4h,
                  af1m5h = af50601m5h + af601001m5h + af1007501m5h + aerof5h,
                  af1m6h = af50601m6h + af601001m6h + af1007501m6h + aerof6h,
                  af1m7h = af50601m7h + af601001m7h + af1007501m7h + aerof7h,
                  af1m8h = af50601m8h + af601001m8h + af1007501m8h + aerof8h,
                  af1m9h = af50601m9h + af601001m9h + af1007501m9h + aerof9h,
                  af1m10h = af50601m10h + af601001m10h + af1007501m10h + aerof10h,
                  af1m11h = af50601m11h + af601001m11h + af1007501m11h + aerof11h,
                  af1m12h = af50601m12h + af601001m12h + af1007501m12h + aerof12h)
dose1m_risk <- select(dose1m, a1m1h:af1m12h)

dose2m <- mutate( dose2m, 
                  a2m1h = a50602m1h + a601002m1h + aero1h ,
                  a2m2h = a50602m2h + a601002m2h + aero2h,
                  a2m3h = a50602m3h + a601002m3h + aero3h,
                  a2m4h = a50602m4h + a601002m4h + aero4h,
                  a2m5h = a50602m5h + a601002m5h + aero5h,
                  a2m6h = a50602m6h + a601002m6h + aero6h,
                  a2m7h = a50602m7h + a601002m7h + aero7h,
                  a2m8h = a50602m8h + a601002m8h + aero8h,
                  a2m9h = a50602m9h + a601002m9h + aero9h,
                  a2m10h = a50602m10h + a601002m10h + aero10h,
                  a2m11h = a50602m11h + a601002m11h + aero11h,
                  a2m12h = a50602m12h + a601002m12h + aero12h,
                  f2m1h = f50602m1h + f601002m1h + f1h,
                  f2m2h = f50602m2h + f601002m2h + f2h,
                  f2m3h = f50602m3h + f601002m3h + f3h,
                  f2m4h = f50602m4h + f601002m4h + f4h,
                  f2m5h = f50602m5h + f601002m5h + f5h,
                  f2m6h = f50602m6h + f601002m6h + f6h,
                  f2m7h = f50602m7h + f601002m7h + f7h,
                  f2m8h = f50602m8h + f601002m8h + f8h,
                  f2m9h = f50602m9h + f601002m9h + f9h,
                  f2m10h = f50602m10h + f601002m10h + f10h,
                  f2m11h = f50602m11h + f601002m11h + f11h,
                  f2m12h = f50602m12h + f601002m12h + f12h,
                  af2m1h = af50602m1h + af601002m1h + aerof1h,
                  af2m2h = af50602m2h + af601002m2h + aerof2h,
                  af2m3h = af50602m3h + af601002m3h + aerof3h,
                  af2m4h = af50602m4h + af601002m4h + aerof4h,
                  af2m5h = af50602m5h + af601002m5h + aerof5h,
                  af2m6h = af50602m6h + af601002m6h + aerof6h,
                  af2m7h = af50602m7h + af601002m7h + aerof7h,
                  af2m8h = af50602m8h + af601002m8h + aerof8h, 
                  af2m9h = af50602m9h + af601002m9h + aerof9h, 
                  af2m10h = af50602m10h + af601002m10h + aerof10h, 
                  af2m11h = af50602m11h + af601002m11h + aerof11h, 
                  af2m12h = af50602m12h + af601002m12h + aerof12h)

dose2m_risk <- select(dose2m, a2m1h:af2m12h)

dose3m <- mutate( dose3m, 
                  a3m1h = a50603m1h + aero1h,
                  a3m2h = a50603m2h + aero2h,
                  a3m3h = a50603m3h + aero3h,
                  a3m4h = a50603m4h + aero4h,
                  a3m5h = a50603m5h + aero5h,
                  a3m6h = a50603m6h + aero6h,
                  a3m7h = a50603m7h + aero7h,
                  a3m8h = a50603m8h + aero8h,
                  a3m9h = a50603m9h + aero9h,
                  a3m10h = a50603m10h + aero10h,
                  a3m11h = a50603m11h + aero11h,
                  a3m12h = a50603m12h + aero12h,
                  f3m1h = f50603m1h + f1h,
                  f3m2h = f50603m2h + f2h,
                  f3m3h = f50603m3h + f3h,
                  f3m4h = f50603m4h + f4h,
                  f3m5h = f50603m5h + f5h,
                  f3m6h = f50603m6h + f6h,
                  f3m7h = f50603m7h + f7h,
                  f3m8h = f50603m8h + f8h,
                  f3m9h = f50603m9h + f9h,
                  f3m10h = f50603m10h + f10h,
                  f3m11h = f50603m11h + f11h,
                  f3m12h = f50603m12h + f12h,
                  af3m1h = af50603m1h + aerof1h,
                  af3m2h = af50603m2h + aerof2h,
                  af3m3h = af50603m3h + aerof3h,
                  af3m4h = af50603m4h + aerof4h,
                  af3m5h = af50603m5h + aerof5h,
                  af3m6h = af50603m6h + aerof6h,
                  af3m7h = af50603m7h + aerof7h,
                  af3m8h = af50603m8h + aerof8h,
                  af3m9h = af50603m9h + aerof9h,
                  af3m10h = af50603m10h + aerof10h,
                  af3m11h = af50603m11h + aerof11h,
                  af3m12h = af50603m12h + aerof12h)

dose3m_risk <- select(dose3m, a3m1h:af3m12h)

Bus_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
write.csv(Bus_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Bus.csv", row.names = FALSE)

# Indoor Module ----------
#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Indoor"

#Fine Tune interventions
ifelse(Module == "Indoor", mask <- "surgical", mask <- "surgical")
i.index.mask<- mask
i.susceptible.mask<-mask

# Make sure it is in the same working directory as current code

ifelse(Module == "Transport", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Transport.R"),
       ifelse(Module == "Residential", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential.R"),
              ifelse(Module == "Outdoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Outdoor.R"),
                     ifelse(Module == "Bus", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210801_Bus.R"),
                            ifelse(Module == "Break", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Break.R"),
                                   ifelse(Module == "ResBreath", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential_Breathing.R"),
                                          ifelse(Module == "Indoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Indoor.R"), source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R"))))))))

#source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R")

# Set controls/interventions

#Master control for ACH
set.seed(12345)
ifelse(Module == "Transport", m.room.exchange <- mcstoc(runif, type="V",min=0.92, max =4.1), 
       ifelse(Module == "Residential", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max =0.35),
              ifelse(Module == "Outdoor", m.room.exchange <- mcstoc(runif, type="V",min=4.568817715, max =4568.817715),
                     ifelse(Module == "Indoor", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max =0.1),
                            ifelse(Module == "Bus", m.room.exchange <- mcstoc(rtriang, type="V",min=8.335238095, mode = 10.94, max = 14.06571429),
                                   ifelse(Module == "Break", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max = 0.1),
                                          mcstoc(runif, type="V",min=0.1, max =0.1)))))))
#Increased ACH 
set.seed(12345)
ifelse(Module == "Transport" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 0.92, max = 72), 
       ifelse(Module == "Residential" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max =5.82),
              ifelse(Module == "Indoor" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 6, max = 6),
                     ifelse(Module == "Bus" & IncACH == "yes", m.room.exchange <- mcstoc(rtriang, type="V",min= 8.13375, mode = 14.46, max = 42.175),
                            ifelse(Module == "Break" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=6, max = 6),
                                   m.room.exchange <- m.room.exchange)))))

m.Humidity<-"high"

ifelse(Module == "Transport", m.Humidity <- "low",
       ifelse(Module == "Residential", m.Humidity <- "high",
              ifelse(Module == "Outdoor", m.Humidity <- "high",
                     ifelse(Module == "Bus", m.Humidity <- "low",
                            ifelse(Module == "Break", m.Humidity <- "low",
                                   ifelse(Module == "Indoor", m.Humidity <- "low", m.Humidity <- "high"))))))

# Aerosol function call

aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))
#View(aero.dose.clean)
#summary(aero.dose.clean$f4h)


# Close contact function call

dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event, Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50603m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=3.0, Vol.Frac.Dist.Name = "50603m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))


dose5060_2m <- select(dose50602m,
                      starts_with("a"),
                      starts_with("f"))


dose60100_2m <- select(dose601002m,
                       starts_with("a"),
                       starts_with("f"))


dose5060_3m <- select(dose50603m,
                      starts_with("a"),
                      starts_with("f"))


# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)
dose3m <-cbind (dose5060_3m, aero.dose.clean)


dose1m <- mutate( dose1m, 
                  a1m1h = a50601m1h + a601001m1h + a1007501m1h + aero1h,
                  a1m2h = a50601m2h + a601001m2h + a1007501m2h + aero2h,
                  a1m3h = a50601m3h + a601001m3h + a1007501m3h + aero3h,
                  a1m4h = a50601m4h + a601001m4h + a1007501m4h + aero4h,
                  a1m5h = a50601m5h + a601001m5h + a1007501m5h + aero5h,
                  a1m6h = a50601m6h + a601001m6h + a1007501m6h + aero6h,
                  a1m7h = a50601m7h + a601001m7h + a1007501m7h + aero7h,
                  a1m8h = a50601m8h + a601001m8h + a1007501m8h + aero8h,
                  a1m9h = a50601m9h + a601001m9h + a1007501m9h + aero9h,
                  a1m10h = a50601m10h + a601001m10h + a1007501m10h + aero10h,
                  a1m11h = a50601m11h + a601001m11h + a1007501m11h + aero11h,
                  a1m12h = a50601m12h + a601001m12h + a1007501m12h + aero12h,
                  f1m1h = f50601m1h + f601001m1h + f1007501m1h + f1h,
                  f1m2h = f50601m2h + f601001m2h + f1007501m2h + f2h,
                  f1m3h = f50601m3h + f601001m3h + f1007501m3h + f3h,
                  f1m4h = f50601m4h + f601001m4h + f1007501m4h + f4h,
                  f1m5h = f50601m5h + f601001m5h + f1007501m5h + f5h,
                  f1m6h = f50601m6h + f601001m6h + f1007501m6h + f6h,
                  f1m7h = f50601m7h + f601001m7h + f1007501m7h + f7h,
                  f1m8h = f50601m8h + f601001m8h + f1007501m8h + f8h,
                  f1m9h = f50601m9h + f601001m9h + f1007501m9h + f9h,
                  f1m10h = f50601m10h + f601001m10h + f1007501m10h + f10h,
                  f1m11h = f50601m11h + f601001m11h + f1007501m11h + f11h,
                  f1m12h = f50601m12h + f601001m12h + f1007501m12h + f12h,
                  af1m1h = af50601m1h + af601001m1h + af1007501m1h + aerof1h,
                  af1m2h = af50601m2h + af601001m2h + af1007501m2h + aerof2h,
                  af1m3h = af50601m3h + af601001m3h + af1007501m3h + aerof3h,
                  af1m4h = af50601m4h + af601001m4h + af1007501m4h + aerof4h,
                  af1m5h = af50601m5h + af601001m5h + af1007501m5h + aerof5h,
                  af1m6h = af50601m6h + af601001m6h + af1007501m6h + aerof6h,
                  af1m7h = af50601m7h + af601001m7h + af1007501m7h + aerof7h,
                  af1m8h = af50601m8h + af601001m8h + af1007501m8h + aerof8h,
                  af1m9h = af50601m9h + af601001m9h + af1007501m9h + aerof9h,
                  af1m10h = af50601m10h + af601001m10h + af1007501m10h + aerof10h,
                  af1m11h = af50601m11h + af601001m11h + af1007501m11h + aerof11h,
                  af1m12h = af50601m12h + af601001m12h + af1007501m12h + aerof12h)
dose1m_risk <- select(dose1m, a1m1h:af1m12h)

dose2m <- mutate( dose2m, 
                  a2m1h = a50602m1h + a601002m1h + aero1h ,
                  a2m2h = a50602m2h + a601002m2h + aero2h,
                  a2m3h = a50602m3h + a601002m3h + aero3h,
                  a2m4h = a50602m4h + a601002m4h + aero4h,
                  a2m5h = a50602m5h + a601002m5h + aero5h,
                  a2m6h = a50602m6h + a601002m6h + aero6h,
                  a2m7h = a50602m7h + a601002m7h + aero7h,
                  a2m8h = a50602m8h + a601002m8h + aero8h,
                  a2m9h = a50602m9h + a601002m9h + aero9h,
                  a2m10h = a50602m10h + a601002m10h + aero10h,
                  a2m11h = a50602m11h + a601002m11h + aero11h,
                  a2m12h = a50602m12h + a601002m12h + aero12h,
                  f2m1h = f50602m1h + f601002m1h + f1h,
                  f2m2h = f50602m2h + f601002m2h + f2h,
                  f2m3h = f50602m3h + f601002m3h + f3h,
                  f2m4h = f50602m4h + f601002m4h + f4h,
                  f2m5h = f50602m5h + f601002m5h + f5h,
                  f2m6h = f50602m6h + f601002m6h + f6h,
                  f2m7h = f50602m7h + f601002m7h + f7h,
                  f2m8h = f50602m8h + f601002m8h + f8h,
                  f2m9h = f50602m9h + f601002m9h + f9h,
                  f2m10h = f50602m10h + f601002m10h + f10h,
                  f2m11h = f50602m11h + f601002m11h + f11h,
                  f2m12h = f50602m12h + f601002m12h + f12h,
                  af2m1h = af50602m1h + af601002m1h + aerof1h,
                  af2m2h = af50602m2h + af601002m2h + aerof2h,
                  af2m3h = af50602m3h + af601002m3h + aerof3h,
                  af2m4h = af50602m4h + af601002m4h + aerof4h,
                  af2m5h = af50602m5h + af601002m5h + aerof5h,
                  af2m6h = af50602m6h + af601002m6h + aerof6h,
                  af2m7h = af50602m7h + af601002m7h + aerof7h,
                  af2m8h = af50602m8h + af601002m8h + aerof8h, 
                  af2m9h = af50602m9h + af601002m9h + aerof9h, 
                  af2m10h = af50602m10h + af601002m10h + aerof10h, 
                  af2m11h = af50602m11h + af601002m11h + aerof11h, 
                  af2m12h = af50602m12h + af601002m12h + aerof12h)

dose2m_risk <- select(dose2m, a2m1h:af2m12h)

dose3m <- mutate( dose3m, 
                  a3m1h = a50603m1h + aero1h,
                  a3m2h = a50603m2h + aero2h,
                  a3m3h = a50603m3h + aero3h,
                  a3m4h = a50603m4h + aero4h,
                  a3m5h = a50603m5h + aero5h,
                  a3m6h = a50603m6h + aero6h,
                  a3m7h = a50603m7h + aero7h,
                  a3m8h = a50603m8h + aero8h,
                  a3m9h = a50603m9h + aero9h,
                  a3m10h = a50603m10h + aero10h,
                  a3m11h = a50603m11h + aero11h,
                  a3m12h = a50603m12h + aero12h,
                  f3m1h = f50603m1h + f1h,
                  f3m2h = f50603m2h + f2h,
                  f3m3h = f50603m3h + f3h,
                  f3m4h = f50603m4h + f4h,
                  f3m5h = f50603m5h + f5h,
                  f3m6h = f50603m6h + f6h,
                  f3m7h = f50603m7h + f7h,
                  f3m8h = f50603m8h + f8h,
                  f3m9h = f50603m9h + f9h,
                  f3m10h = f50603m10h + f10h,
                  f3m11h = f50603m11h + f11h,
                  f3m12h = f50603m12h + f12h,
                  af3m1h = af50603m1h + aerof1h,
                  af3m2h = af50603m2h + aerof2h,
                  af3m3h = af50603m3h + aerof3h,
                  af3m4h = af50603m4h + aerof4h,
                  af3m5h = af50603m5h + aerof5h,
                  af3m6h = af50603m6h + aerof6h,
                  af3m7h = af50603m7h + aerof7h,
                  af3m8h = af50603m8h + aerof8h,
                  af3m9h = af50603m9h + aerof9h,
                  af3m10h = af50603m10h + aerof10h,
                  af3m11h = af50603m11h + aerof11h,
                  af3m12h = af50603m12h + aerof12h)

dose3m_risk <- select(dose3m, a3m1h:af3m12h)

Indoor_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
write.csv(Indoor_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Indoor.csv", row.names = FALSE)

# Breakroom Module -------------------

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Break"

#Fine Tune interventions
ifelse(Module == "Break", mask <- "none", mask <- "surgical")
i.index.mask<- mask
i.susceptible.mask<-mask

# Make sure it is in the same working directory as current code

ifelse(Module == "Transport", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Transport.R"),
       ifelse(Module == "Residential", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential.R"),
              ifelse(Module == "Outdoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Outdoor.R"),
                     ifelse(Module == "Bus", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210801_Bus.R"),
                            ifelse(Module == "Break", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Break.R"),
                                   ifelse(Module == "ResBreath", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Residential_Breathing.R"),
                                          ifelse(Module == "Indoor", source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225_Indoor.R"), source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R"))))))))

#source("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Functions_20210225.R")

# Set controls/interventions

#Master control for ACH
set.seed(12345)
ifelse(Module == "Transport", m.room.exchange <- mcstoc(runif, type="V",min=0.92, max =4.1), 
       ifelse(Module == "Residential", m.room.exchange <- mcstoc(runif, type="V",min=0.35, max =0.35),
              ifelse(Module == "Outdoor", m.room.exchange <- mcstoc(runif, type="V",min=4.568817715, max =4568.817715),
                     ifelse(Module == "Indoor", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max =0.1),
                            ifelse(Module == "Bus", m.room.exchange <- mcstoc(rtriang, type="V",min=8.335238095, mode = 10.94, max = 14.06571429),
                                   ifelse(Module == "Break", m.room.exchange <- mcstoc(runif, type="V",min=0.1, max = 0.1),
                                          mcstoc(runif, type="V",min=0.1, max =0.1)))))))
#Increased ACH 
set.seed(12345)
ifelse(Module == "Transport" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 0.92, max = 72), 
       ifelse(Module == "Residential" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=1.98, max =5.82),
              ifelse(Module == "Indoor" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type = "V", min = 6, max = 6),
                     ifelse(Module == "Bus" & IncACH == "yes", m.room.exchange <- mcstoc(rtriang, type="V",min= 8.13375, mode = 14.46, max = 42.175),
                            ifelse(Module == "Break" & IncACH == "yes", m.room.exchange <- mcstoc(runif, type="V",min=6, max = 6),
                                   m.room.exchange <- m.room.exchange)))))

m.Humidity<-"high"

ifelse(Module == "Transport", m.Humidity <- "low",
       ifelse(Module == "Residential", m.Humidity <- "high",
              ifelse(Module == "Outdoor", m.Humidity <- "high",
                     ifelse(Module == "Bus", m.Humidity <- "low",
                            ifelse(Module == "Break", m.Humidity <- "low",
                                   ifelse(Module == "Indoor", m.Humidity <- "low", m.Humidity <- "high"))))))

# Aerosol function call

aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))
#View(aero.dose.clean)
#summary(aero.dose.clean$f4h)


# Close contact function call

dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event, Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50603m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=3.0, Vol.Frac.Dist.Name = "50603m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)


dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))


dose5060_2m <- select(dose50602m,
                      starts_with("a"),
                      starts_with("f"))


dose60100_2m <- select(dose601002m,
                       starts_with("a"),
                       starts_with("f"))


dose5060_3m <- select(dose50603m,
                      starts_with("a"),
                      starts_with("f"))


# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)
dose3m <-cbind (dose5060_3m, aero.dose.clean)


dose1m <- mutate( dose1m, 
                  a1m1h = a50601m1h + a601001m1h + a1007501m1h + aero1h,
                  a1m2h = a50601m2h + a601001m2h + a1007501m2h + aero2h,
                  a1m3h = a50601m3h + a601001m3h + a1007501m3h + aero3h,
                  a1m4h = a50601m4h + a601001m4h + a1007501m4h + aero4h,
                  a1m5h = a50601m5h + a601001m5h + a1007501m5h + aero5h,
                  a1m6h = a50601m6h + a601001m6h + a1007501m6h + aero6h,
                  a1m7h = a50601m7h + a601001m7h + a1007501m7h + aero7h,
                  a1m8h = a50601m8h + a601001m8h + a1007501m8h + aero8h,
                  a1m9h = a50601m9h + a601001m9h + a1007501m9h + aero9h,
                  a1m10h = a50601m10h + a601001m10h + a1007501m10h + aero10h,
                  a1m11h = a50601m11h + a601001m11h + a1007501m11h + aero11h,
                  a1m12h = a50601m12h + a601001m12h + a1007501m12h + aero12h,
                  f1m1h = f50601m1h + f601001m1h + f1007501m1h + f1h,
                  f1m2h = f50601m2h + f601001m2h + f1007501m2h + f2h,
                  f1m3h = f50601m3h + f601001m3h + f1007501m3h + f3h,
                  f1m4h = f50601m4h + f601001m4h + f1007501m4h + f4h,
                  f1m5h = f50601m5h + f601001m5h + f1007501m5h + f5h,
                  f1m6h = f50601m6h + f601001m6h + f1007501m6h + f6h,
                  f1m7h = f50601m7h + f601001m7h + f1007501m7h + f7h,
                  f1m8h = f50601m8h + f601001m8h + f1007501m8h + f8h,
                  f1m9h = f50601m9h + f601001m9h + f1007501m9h + f9h,
                  f1m10h = f50601m10h + f601001m10h + f1007501m10h + f10h,
                  f1m11h = f50601m11h + f601001m11h + f1007501m11h + f11h,
                  f1m12h = f50601m12h + f601001m12h + f1007501m12h + f12h,
                  af1m1h = af50601m1h + af601001m1h + af1007501m1h + aerof1h,
                  af1m2h = af50601m2h + af601001m2h + af1007501m2h + aerof2h,
                  af1m3h = af50601m3h + af601001m3h + af1007501m3h + aerof3h,
                  af1m4h = af50601m4h + af601001m4h + af1007501m4h + aerof4h,
                  af1m5h = af50601m5h + af601001m5h + af1007501m5h + aerof5h,
                  af1m6h = af50601m6h + af601001m6h + af1007501m6h + aerof6h,
                  af1m7h = af50601m7h + af601001m7h + af1007501m7h + aerof7h,
                  af1m8h = af50601m8h + af601001m8h + af1007501m8h + aerof8h,
                  af1m9h = af50601m9h + af601001m9h + af1007501m9h + aerof9h,
                  af1m10h = af50601m10h + af601001m10h + af1007501m10h + aerof10h,
                  af1m11h = af50601m11h + af601001m11h + af1007501m11h + aerof11h,
                  af1m12h = af50601m12h + af601001m12h + af1007501m12h + aerof12h)
dose1m_risk <- select(dose1m, a1m1h:af1m12h)

dose2m <- mutate( dose2m, 
                  a2m1h = a50602m1h + a601002m1h + aero1h ,
                  a2m2h = a50602m2h + a601002m2h + aero2h,
                  a2m3h = a50602m3h + a601002m3h + aero3h,
                  a2m4h = a50602m4h + a601002m4h + aero4h,
                  a2m5h = a50602m5h + a601002m5h + aero5h,
                  a2m6h = a50602m6h + a601002m6h + aero6h,
                  a2m7h = a50602m7h + a601002m7h + aero7h,
                  a2m8h = a50602m8h + a601002m8h + aero8h,
                  a2m9h = a50602m9h + a601002m9h + aero9h,
                  a2m10h = a50602m10h + a601002m10h + aero10h,
                  a2m11h = a50602m11h + a601002m11h + aero11h,
                  a2m12h = a50602m12h + a601002m12h + aero12h,
                  f2m1h = f50602m1h + f601002m1h + f1h,
                  f2m2h = f50602m2h + f601002m2h + f2h,
                  f2m3h = f50602m3h + f601002m3h + f3h,
                  f2m4h = f50602m4h + f601002m4h + f4h,
                  f2m5h = f50602m5h + f601002m5h + f5h,
                  f2m6h = f50602m6h + f601002m6h + f6h,
                  f2m7h = f50602m7h + f601002m7h + f7h,
                  f2m8h = f50602m8h + f601002m8h + f8h,
                  f2m9h = f50602m9h + f601002m9h + f9h,
                  f2m10h = f50602m10h + f601002m10h + f10h,
                  f2m11h = f50602m11h + f601002m11h + f11h,
                  f2m12h = f50602m12h + f601002m12h + f12h,
                  af2m1h = af50602m1h + af601002m1h + aerof1h,
                  af2m2h = af50602m2h + af601002m2h + aerof2h,
                  af2m3h = af50602m3h + af601002m3h + aerof3h,
                  af2m4h = af50602m4h + af601002m4h + aerof4h,
                  af2m5h = af50602m5h + af601002m5h + aerof5h,
                  af2m6h = af50602m6h + af601002m6h + aerof6h,
                  af2m7h = af50602m7h + af601002m7h + aerof7h,
                  af2m8h = af50602m8h + af601002m8h + aerof8h, 
                  af2m9h = af50602m9h + af601002m9h + aerof9h, 
                  af2m10h = af50602m10h + af601002m10h + aerof10h, 
                  af2m11h = af50602m11h + af601002m11h + aerof11h, 
                  af2m12h = af50602m12h + af601002m12h + aerof12h)

dose2m_risk <- select(dose2m, a2m1h:af2m12h)

dose3m <- mutate( dose3m, 
                  a3m1h = a50603m1h + aero1h,
                  a3m2h = a50603m2h + aero2h,
                  a3m3h = a50603m3h + aero3h,
                  a3m4h = a50603m4h + aero4h,
                  a3m5h = a50603m5h + aero5h,
                  a3m6h = a50603m6h + aero6h,
                  a3m7h = a50603m7h + aero7h,
                  a3m8h = a50603m8h + aero8h,
                  a3m9h = a50603m9h + aero9h,
                  a3m10h = a50603m10h + aero10h,
                  a3m11h = a50603m11h + aero11h,
                  a3m12h = a50603m12h + aero12h,
                  f3m1h = f50603m1h + f1h,
                  f3m2h = f50603m2h + f2h,
                  f3m3h = f50603m3h + f3h,
                  f3m4h = f50603m4h + f4h,
                  f3m5h = f50603m5h + f5h,
                  f3m6h = f50603m6h + f6h,
                  f3m7h = f50603m7h + f7h,
                  f3m8h = f50603m8h + f8h,
                  f3m9h = f50603m9h + f9h,
                  f3m10h = f50603m10h + f10h,
                  f3m11h = f50603m11h + f11h,
                  f3m12h = f50603m12h + f12h,
                  af3m1h = af50603m1h + aerof1h,
                  af3m2h = af50603m2h + aerof2h,
                  af3m3h = af50603m3h + aerof3h,
                  af3m4h = af50603m4h + aerof4h,
                  af3m5h = af50603m5h + aerof5h,
                  af3m6h = af50603m6h + aerof6h,
                  af3m7h = af50603m7h + aerof7h,
                  af3m8h = af50603m8h + aerof8h,
                  af3m9h = af50603m9h + aerof9h,
                  af3m10h = af50603m10h + aerof10h,
                  af3m11h = af50603m11h + aerof11h,
                  af3m12h = af50603m12h + aerof12h)

dose3m_risk <- select(dose3m, a3m1h:af3m12h)

Break_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
write.csv(Break_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Break.csv", row.names = FALSE)
