########### Sensitivity Code - September, 2022 ###############

##### Car Transportation Baseline -----------------------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "none"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
  #Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Transport"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                          mRNAvaxxf1h = mRNAvaccine * aerof1h,
                          mRNAvaxxf2h = mRNAvaccine * aerof2h,
                          mRNAvaxxf3h = mRNAvaccine * aerof3h,
                          mRNAvaxxf4h = mRNAvaccine * aerof4h,
                          mRNAvaxxf5h = mRNAvaccine * aerof5h,
                          mRNAvaxxf6h = mRNAvaccine * aerof6h,
                          mRNAvaxxf7h = mRNAvaccine * aerof7h,
                          mRNAvaxxf8h = mRNAvaccine * aerof8h,
                          mRNAvaxxf9h = mRNAvaccine * aerof9h,
                          mRNAvaxxf10h = mRNAvaccine * aerof10h,
                          mRNAvaxxf11h = mRNAvaccine * aerof11h,
                          mRNAvaxxf12h = mRNAvaccine * aerof12h,
                          reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                          reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                          reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                          reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                          reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                          reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                          reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                          reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                          reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                          reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                          reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                          reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Car.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Car.csv", row.names = TRUE)

##### Bus Transportation Baseline -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "none"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Bus"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Bus.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Bus.csv", row.names = TRUE)

##### Indoor 2m Baseline -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "none"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Indoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Indoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Indoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_Indoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_Indoor.csv", row.names = TRUE)

##### Outdoor 1m Baseline -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "none"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Outdoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)

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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Outdoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_Outdoor.csv", row.names = TRUE)

##### Residential Cough 2m Baseline -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "none"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Residential"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_ResCough.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_ResCough.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_ResCough.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_ResCough.csv", row.names = TRUE)

##### Residential Breathing 8h Baseline -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "none"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"breath"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "ResBreath"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_ResBreath.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_ResBreath.csv", row.names = TRUE)

##### Indoor Breakroom 2m Baseline -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "none"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Break"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_IndoorBreak.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_IndoorBreak.csv", row.names = TRUE)


##### Car Transportation Cloth Mask -----------------------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "cloth"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Transport"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Car.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Car.csv", row.names = TRUE)

##### Bus Transportation Cloth Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "cloth"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Bus"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Bus.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Bus.csv", row.names = TRUE)

##### Indoor 2m Cloth Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "cloth"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Indoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Indoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Indoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_Indoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_Indoor.csv", row.names = TRUE)

##### Outdoor 1m Cloth Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "cloth"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Outdoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)

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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Outdoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_Outdoor.csv", row.names = TRUE)

##### Residential Cough 2m Cloth Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "cloth"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Residential"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_ResCough.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_ResCough.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_ResCough.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_ResCough.csv", row.names = TRUE)

##### Residential Breathing 8h Cloth Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "cloth"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"breath"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "ResBreath"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_ResBreath.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_ResBreath.csv", row.names = TRUE)

##### Indoor Breakroom 2m Cloth Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "cloth"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Break"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_IndoorBreak.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_IndoorBreak.csv", row.names = TRUE)



##### Car Transportation Surgical Mask -----------------------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Transport"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Car.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Car.csv", row.names = TRUE)

##### Bus Transportation Surgical Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Bus"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Bus.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Bus.csv", row.names = TRUE)

##### Indoor 2m Surgical Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "surgical"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Indoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Indoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Indoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_Indoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_Indoor.csv", row.names = TRUE)

##### Outdoor 1m Surgical Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "surgical"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Outdoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)

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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Outdoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_Outdoor.csv", row.names = TRUE)

##### Residential Cough 2m Surgical Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Residential"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_ResCough.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_ResCough.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_ResCough.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_ResCough.csv", row.names = TRUE)

##### Residential Breathing 8h Surgical Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"breath"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "ResBreath"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_ResBreath.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_ResBreath.csv", row.names = TRUE)

##### Indoor Breakroom 2m Surgical Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Break"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_IndoorBreak.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_IndoorBreak.csv", row.names = TRUE)




##### Car Transportation Double Mask -----------------------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "double"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Transport"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Car.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Car.csv", row.names = TRUE)

##### Bus Transportation Double Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
#print(risk2m.comb.stats[36,])

# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "double"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Bus"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

#Transportation_Module <- cbind(aero.dose.clean, dose1m_risk, dose2m_risk, dose3m_risk)
#write.csv(Transportation_Module, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv", row.names = FALSE)

# Calculate risk

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)
#View(riskaero.comb.stats)
#write.csv(riskaero.comb.stats, "C:\\Users\\jsoboli\\Desktop\\aerorisk.csv", row.names=TRUE)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Bus.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Bus.csv", row.names = TRUE)

##### Indoor 2m Double Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "double"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Indoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Indoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Indoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_Indoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_Indoor.csv", row.names = TRUE)

##### Outdoor 1m Double Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
i.Clean1<-0
i.Clean2<-0
i.Clean3<-0
i.Clean4<-1
i.Clean5<-0
i.Clean6<-0
i.Clean7<-0
i.Clean8<-1
i.Clean9<-0
i.Clean10<-0
i.Clean11<-0
i.Clean12<-0
mask <- "double"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"yes"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Outdoor"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

dose5060_1m <- select(dose50601m,
                      starts_with("a"),
                      starts_with("f"))

dose60100_1m <- select(dose601001m,
                       starts_with("a"),
                       starts_with("f"))


dose100750_1m <- select(dose1007501m,
                        starts_with("a"),
                        starts_with("f"))

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)

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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Outdoor.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_Outdoor.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_Outdoor.csv", row.names = TRUE)

##### Residential Cough 2m Double Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "double"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Residential"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_ResCough.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_ResCough.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_ResCough.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_ResCough.csv", row.names = TRUE)

##### Residential Breathing 8h Double Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "double"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"breath"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "ResBreath"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_ResBreath.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_ResBreath.csv", row.names = TRUE)

##### Indoor Breakroom 2m Double Mask -----------------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
set.seed(12345)

#10001 simulations - variability
ndvar(10001)

#Intervention controls for both aerosol and close contact modules
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
mask <- "double"
i.index.mask<- mask
i.susceptible.mask<- mask
i.HW<-"no"
i.Glove<-"no"
i.surface.clean.eff <- mcstoc(runif, type = "VU", min = 0.90, max = 0.999)

#Master controls for both aerosol and close contact modules ("cough" or "breath")
m.Event <-"cough"

#Master Control for strain being analyzed and vaccination status for the infected individual
#Options: Baseline.NV, Pre.VOC.NV, Delta.NV, Omicron.NV, Baseline.Vax, Delta.Vax, Omicron.Vax
#m.Strain.VaxStat <- "Baseline.NV"

#Intervention Lever, "yes" or "no"
IncACH <- "no"

#Module Switch - "Transport", "Residential", "Outdoor", "Indoor", "Bus", "Break", "ResBreath"
Module <- "Break"

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
#Strain.VaxStat = m.Strain.VaxStat, 
aero.dose <- Aerofunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

aero.dose.clean <- select(aero.dose,
                          starts_with("a"),
                          starts_with("f"))

# Close contact function call
#Strain.VaxStat = m.Strain.VaxStat, 
dose50601m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=1.0, Vol.Frac.Dist.Name = "50601m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601001m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=1.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose1007501m <- Dosefunc(Event = m.Event,  Volume.Fraction="100+", Distance=1.0, Vol.Frac.Dist.Name = "1007501m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose50602m <- Dosefunc(Event = m.Event, Volume.Fraction="50-60", Distance=2.0, Vol.Frac.Dist.Name = "50602m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
dose601002m <- Dosefunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601002m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)

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

# Combine aerosol and close contact doses

#adding aerosol and aerosol fomite to close contact doses
dose1m <-cbind (dose5060_1m, dose60100_1m, dose100750_1m, aero.dose.clean)
dose2m <-cbind (dose5060_2m, dose60100_2m, aero.dose.clean)


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

# Calculate risk 

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#vaccination for susceptible worker
fullvaccine <- mcstoc(runif, type="V",min=0.01, max =0.23)
partialvaccine <- mcstoc(runif, type="V",min=0.26, max =0.48)
fullvaccine <- unmc (fullvaccine,drop = TRUE)
fullvaccine <- as.data.frame(fullvaccine)
partialvaccine<-unmc (partialvaccine,drop = TRUE)
partialvaccine<-as.data.frame(partialvaccine)

#This is for two doses of Pfizer / Moderna (86-99%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)

mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Pull combined doses through dose response for data frame output of risk
#aerosol module risk (>3m)
#So this will tell us the cumulative risk for aerosol and aerosol-mediated fomites after vaccination
riskaero.df = 1-exp(-krisk*aero.dose.clean)
aeroriskvaxx <-cbind(riskaero.df,mRNAvaccine, reducedVEvaccine)
aeroriskvaxxfull <- mutate(aeroriskvaxx, 
                           mRNAvaxxf1h = mRNAvaccine * aerof1h,
                           mRNAvaxxf2h = mRNAvaccine * aerof2h,
                           mRNAvaxxf3h = mRNAvaccine * aerof3h,
                           mRNAvaxxf4h = mRNAvaccine * aerof4h,
                           mRNAvaxxf5h = mRNAvaccine * aerof5h,
                           mRNAvaxxf6h = mRNAvaccine * aerof6h,
                           mRNAvaxxf7h = mRNAvaccine * aerof7h,
                           mRNAvaxxf8h = mRNAvaccine * aerof8h,
                           mRNAvaxxf9h = mRNAvaccine * aerof9h,
                           mRNAvaxxf10h = mRNAvaccine * aerof10h,
                           mRNAvaxxf11h = mRNAvaccine * aerof11h,
                           mRNAvaxxf12h = mRNAvaccine * aerof12h,
                           reducedVEvaxxf1h = reducedVEvaccine * aerof1h,
                           reducedVEvaxxf2h = reducedVEvaccine * aerof2h,
                           reducedVEvaxxf3h = reducedVEvaccine * aerof3h,
                           reducedVEvaxxf4h = reducedVEvaccine * aerof4h,
                           reducedVEvaxxf5h = reducedVEvaccine * aerof5h,
                           reducedVEvaxxf6h = reducedVEvaccine * aerof6h,
                           reducedVEvaxxf7h = reducedVEvaccine * aerof7h,
                           reducedVEvaxxf8h = reducedVEvaccine * aerof8h,
                           reducedVEvaxxf9h = reducedVEvaccine * aerof9h,
                           reducedVEvaxxf10h = reducedVEvaccine * aerof10h,
                           reducedVEvaxxf11h = reducedVEvaccine * aerof11h,
                           reducedVEvaxxf12h = reducedVEvaccine * aerof12h)

riskaero.quant<-as.data.frame(t(apply(aeroriskvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
riskaero.mean<- as.data.frame(colMeans(aeroriskvaxxfull))
riskaero.comb.stats<- cbind(riskaero.quant, riskaero.mean)

#close contact 1m risk
risk1m.df = 1-exp(-krisk*dose1m_risk)
risk1mvaxx <-cbind(risk1m.df, mRNAvaccine, reducedVEvaccine)
risk1mvaxxfull <-mutate(risk1mvaxx,
                        
                        mRNAvaxx1m1h = mRNAvaccine * af1m1h,
                        mRNAvaxx1m2h = mRNAvaccine * af1m2h,
                        mRNAvaxx1m3h = mRNAvaccine * af1m3h,
                        mRNAvaxx1m4h = mRNAvaccine * af1m4h,
                        mRNAvaxx1m5h = mRNAvaccine * af1m5h,
                        mRNAvaxx1m6h = mRNAvaccine * af1m6h,
                        mRNAvaxx1m7h = mRNAvaccine * af1m7h,
                        mRNAvaxx1m8h = mRNAvaccine * af1m8h,
                        mRNAvaxx1m9h = mRNAvaccine * af1m9h,
                        mRNAvaxx1m10h = mRNAvaccine * af1m10h,
                        mRNAvaxx1m11h = mRNAvaccine * af1m11h,
                        mRNAvaxx1m12h = mRNAvaccine * af1m12h,
                        reducedVEvaxx1m1h = reducedVEvaccine * af1m1h,
                        reducedVEvaxx1m2h = reducedVEvaccine * af1m2h,
                        reducedVEvaxx1m3h = reducedVEvaccine * af1m3h,
                        reducedVEvaxx1m4h = reducedVEvaccine * af1m4h,
                        reducedVEvaxx1m5h = reducedVEvaccine * af1m5h,
                        reducedVEvaxx1m6h = reducedVEvaccine * af1m6h,
                        reducedVEvaxx1m7h = reducedVEvaccine * af1m7h,
                        reducedVEvaxx1m8h = reducedVEvaccine * af1m8h,
                        reducedVEvaxx1m9h = reducedVEvaccine * af1m9h,
                        reducedVEvaxx1m10h = reducedVEvaccine * af1m10h,
                        reducedVEvaxx1m11h = reducedVEvaccine * af1m11h,
                        reducedVEvaxx1m12h = reducedVEvaccine * af1m12h)

risk1m.quant<-as.data.frame(t(apply(risk1mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk1m.mean<- as.data.frame(colMeans(risk1mvaxxfull))
risk1m.comb.stats<- cbind(risk1m.quant, risk1m.mean)

#close contact 2m risk
risk2m.df = 1-exp(-krisk*dose2m_risk)
risk2mvaxx <-cbind(risk2m.df, mRNAvaccine, reducedVEvaccine)
risk2mvaxxfull <-mutate(risk2mvaxx,
                        
                        mRNAvvaxx2m1h = mRNAvaccine * af2m1h,
                        mRNAvvaxx2m2h = mRNAvaccine * af2m2h,
                        mRNAvvaxx2m3h = mRNAvaccine * af2m3h,
                        mRNAvvaxx2m4h = mRNAvaccine * af2m4h,
                        mRNAvaxx2m5h = mRNAvaccine * af2m5h,
                        mRNAvaxx2m6h = mRNAvaccine * af2m6h,
                        mRNAvaxx2m7h = mRNAvaccine * af2m7h,
                        mRNAvaxx2m8h = mRNAvaccine * af2m8h,
                        mRNAvaxx2m9h = mRNAvaccine * af2m9h,
                        mRNAvaxx2m10h = mRNAvaccine * af2m10h,
                        mRNAvaxx2m11h = mRNAvaccine * af2m11h,
                        mRNAvaxx2m12h = mRNAvaccine * af2m12h,
                        reducedVEvaxx2m1h = reducedVEvaccine * af2m1h,
                        reducedVEvaxx2m2h = reducedVEvaccine * af2m2h,
                        reducedVEvaxx2m3h = reducedVEvaccine * af2m3h,
                        reducedVEvaxx2m4h = reducedVEvaccine * af2m4h,
                        reducedVEvaxx2m5h = reducedVEvaccine * af2m5h,
                        reducedVEvaxx2m6h = reducedVEvaccine * af2m6h,
                        reducedVEvaxx2m7h = reducedVEvaccine * af2m7h,
                        reducedVEvaxx2m8h = reducedVEvaccine * af2m8h,
                        reducedVEvaxx2m9h = reducedVEvaccine * af2m9h,
                        reducedVEvaxx2m10h = reducedVEvaccine * af2m10h,
                        reducedVEvaxx2m11h = reducedVEvaccine * af2m11h,
                        reducedVEvaxx2m12h = reducedVEvaccine * af2m12h)

risk2m.quant<-as.data.frame(t(apply(risk2mvaxxfull, 2, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))))
risk2m.mean<- as.data.frame(colMeans(risk2mvaxxfull))
risk2m.comb.stats<- cbind(risk2m.quant, risk2m.mean)

######## Sensitivity Analyses

aero.sense <- AeroSensfunc(Event = m.Event, room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
AeroTor <- tornado(aero.sense, method = "spearman")
AeroRatio <- as.data.frame(mcratio(aero.sense))

droplet.sense <- DoseSensfunc(Event = m.Event, Volume.Fraction="60-100", Distance=2.0, Vol.Frac.Dist.Name = "601001m", room.exchange = m.room.exchange, Clean1=i.Clean1, Clean2=i.Clean2, Clean3=i.Clean3, Clean4=i.Clean4, Clean5=i.Clean5, Clean6=i.Clean6, Clean7=i.Clean7, Clean8=i.Clean8, Clean9=i.Clean9, Clean10=i.Clean10, Clean11=i.Clean11, Clean12=i.Clean12, sc.eff.p = i.surface.clean.eff, Humidity= m.Humidity, Infected.Mask = i.index.mask, HW = i.HW, Glove = i.Glove, Susceptible.Mask = i.susceptible.mask)
DropTor <- tornado(droplet.sense)
DropletRatio <- as.data.frame(mcratio(droplet.sense))

write.csv(AeroRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(AeroTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_IndoorBreak.csv", row.names = TRUE)
write.csv(DropletRatio, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_IndoorBreak.csv", row.names = TRUE)
write.csv(print(DropTor), "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_IndoorBreak.csv", row.names = TRUE)






##### Baseline Spearman Dataset Creation ----------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
library(data.table)
library(openxlsx)

Indoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Indoor.csv", header = TRUE)
Indoor_Spearman_Aero <- data.frame(Indoor_Spearman_Aero, row.names = 1)
Indoor_Spearman_Aero <- as.data.frame(t(Indoor_Spearman_Aero))

Indoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_Indoor.csv", header = TRUE)
Indoor_Spearman_Drop <- data.frame(Indoor_Spearman_Drop, row.names = 1)
Indoor_Spearman_Drop <- as.data.frame(t(Indoor_Spearman_Drop))

Outdoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Aero <- data.frame(Outdoor_Spearman_Aero, row.names = 1)
Outdoor_Spearman_Aero <- as.data.frame(t(Outdoor_Spearman_Aero))

Outdoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Drop <- data.frame(Outdoor_Spearman_Drop, row.names = 1)
Outdoor_Spearman_Drop <- as.data.frame(t(Outdoor_Spearman_Drop))

Car_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Car.csv", header = TRUE)
Car_Spearman_Aero <- data.frame(Car_Spearman_Aero, row.names = 1)
Car_Spearman_Aero <- as.data.frame(t(Car_Spearman_Aero))

Bus_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_Bus.csv", header = TRUE)
Bus_Spearman_Aero <- data.frame(Bus_Spearman_Aero, row.names = 1)
Bus_Spearman_Aero <- as.data.frame(t(Bus_Spearman_Aero))

IndoorBreak_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Aero <- data.frame(IndoorBreak_Spearman_Aero, row.names = 1)
IndoorBreak_Spearman_Aero <- as.data.frame(t(IndoorBreak_Spearman_Aero))

IndoorBreak_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Drop <- data.frame(IndoorBreak_Spearman_Drop, row.names = 1)
IndoorBreak_Spearman_Drop <- as.data.frame(t(IndoorBreak_Spearman_Drop))

ResCough_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_ResCough.csv", header = TRUE)
ResCough_Spearman_Aero <- data.frame(ResCough_Spearman_Aero, row.names = 1)
ResCough_Spearman_Aero <- as.data.frame(t(ResCough_Spearman_Aero))

ResCough_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/SpearmanDrop_ResCough.csv", header = TRUE)
ResCough_Spearman_Drop <- data.frame(ResCough_Spearman_Drop, row.names = 1)
ResCough_Spearman_Drop <- as.data.frame(t(ResCough_Spearman_Drop))

ResBreath_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/Spearman_ResBreath.csv", header = TRUE)
ResBreath_Spearman_Aero <- data.frame(ResBreath_Spearman_Aero, row.names = 1)
ResBreath_Spearman_Aero <- as.data.frame(t(ResBreath_Spearman_Aero))

Cumulative_Spearman_Baseline_Data <- list("Outdoor Aero" = Outdoor_Spearman_Aero,
                                          "Outdoor Drop" = Outdoor_Spearman_Drop,
                                          
                                          "Indoor Aero" = Indoor_Spearman_Aero,
                                          "Indoor Drop" = Indoor_Spearman_Drop,
                                          
                                          "Break Aero" = IndoorBreak_Spearman_Aero,
                                          "Break Drop" = IndoorBreak_Spearman_Drop,
                                          
                                          "Car Aero" = Car_Spearman_Aero,
                                          
                                          "Bus Aero" = Bus_Spearman_Aero,
                                          
                                          "ResCough Aero" = ResCough_Spearman_Aero,
                                          "ResCough Drop" = ResCough_Spearman_Drop,
                                          
                                          "ResBreath Aero" = ResBreath_Spearman_Aero)

write.xlsx(Cumulative_Spearman_Baseline_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline Spearman Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)

##### Baseline Aero Ratio Dataset Creation ----------
Indoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Indoor.csv", header = TRUE)
Indoor_Ratio <- data.frame(Indoor_Ratio, row.names = 1)

Outdoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio <- data.frame(Outdoor_Ratio, row.names = 1)

Car_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Car.csv", header = TRUE)
Car_Ratio <- data.frame(Car_Ratio, row.names = 1)

Bus_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_Bus.csv", header = TRUE)
Bus_Ratio <- data.frame(Bus_Ratio, row.names = 1)

IndoorBreak_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio <- data.frame(IndoorBreak_Ratio, row.names = 1)

ResCough_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_ResCough.csv", header = TRUE)
ResCough_Ratio <- data.frame(ResCough_Ratio, row.names = 1)

ResBreath_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/AeroRatios_ResBreath.csv", header = TRUE)
ResBreath_Ratio <- data.frame(ResBreath_Ratio, row.names = 1)

Cumulative_Ratio_Baseline_Data <- list("Outdoor Ratio" = Outdoor_Ratio,

                                          "Indoor Ratio" = Indoor_Ratio,

                                          "Break Ratio" = IndoorBreak_Ratio,

                                          "Car Ratio" = Car_Ratio,
                                          
                                          "Bus Ratio" = Bus_Ratio,
                                          
                                          "ResCough Ratio" = ResCough_Ratio,

                                          "ResBreath Ratio" = ResBreath_Ratio)

write.xlsx(Cumulative_Ratio_Baseline_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline Aero mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)

##### Baseline Droplet Ratio Dataset Creation ----------

Indoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_Indoor.csv", header = TRUE)
Indoor_Ratio_Drop <- data.frame(Indoor_Ratio_Drop, row.names = 1)

Outdoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio_Drop <- data.frame(Outdoor_Ratio_Drop, row.names = 1)

IndoorBreak_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio_Drop <- data.frame(IndoorBreak_Ratio_Drop, row.names = 1)

ResCough_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline/DropRatios_ResCough.csv", header = TRUE)
ResCough_Ratio_Drop <- data.frame(ResCough_Ratio_Drop, row.names = 1)

Cumulative_Ratio_Baseline_Data_Drop <- list("Outdoor Ratio" = Outdoor_Ratio_Drop,
                                       
                                       "Indoor Ratio" = Indoor_Ratio_Drop,
                                       
                                       "Break Ratio" = IndoorBreak_Ratio_Drop,
                                      
                                       "ResCough Ratio" = ResCough_Ratio_Drop)

write.xlsx(Cumulative_Ratio_Baseline_Data_Drop, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Baseline Drop mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)


##### Cloth Mask Spearman Dataset Creation ----------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
library(data.table)

Indoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Indoor.csv", header = TRUE)
Indoor_Spearman_Aero <- data.frame(Indoor_Spearman_Aero, row.names = 1)
Indoor_Spearman_Aero <- as.data.frame(t(Indoor_Spearman_Aero))

Indoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_Indoor.csv", header = TRUE)
Indoor_Spearman_Drop <- data.frame(Indoor_Spearman_Drop, row.names = 1)
Indoor_Spearman_Drop <- as.data.frame(t(Indoor_Spearman_Drop))

Outdoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Aero <- data.frame(Outdoor_Spearman_Aero, row.names = 1)
Outdoor_Spearman_Aero <- as.data.frame(t(Outdoor_Spearman_Aero))

Outdoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Drop <- data.frame(Outdoor_Spearman_Drop, row.names = 1)
Outdoor_Spearman_Drop <- as.data.frame(t(Outdoor_Spearman_Drop))

Car_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Car.csv", header = TRUE)
Car_Spearman_Aero <- data.frame(Car_Spearman_Aero, row.names = 1)
Car_Spearman_Aero <- as.data.frame(t(Car_Spearman_Aero))

Bus_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_Bus.csv", header = TRUE)
Bus_Spearman_Aero <- data.frame(Bus_Spearman_Aero, row.names = 1)
Bus_Spearman_Aero <- as.data.frame(t(Bus_Spearman_Aero))

IndoorBreak_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Aero <- data.frame(IndoorBreak_Spearman_Aero, row.names = 1)
IndoorBreak_Spearman_Aero <- as.data.frame(t(IndoorBreak_Spearman_Aero))

IndoorBreak_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Drop <- data.frame(IndoorBreak_Spearman_Drop, row.names = 1)
IndoorBreak_Spearman_Drop <- as.data.frame(t(IndoorBreak_Spearman_Drop))

ResCough_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_ResCough.csv", header = TRUE)
ResCough_Spearman_Aero <- data.frame(ResCough_Spearman_Aero, row.names = 1)
ResCough_Spearman_Aero <- as.data.frame(t(ResCough_Spearman_Aero))

ResCough_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/SpearmanDrop_ResCough.csv", header = TRUE)
ResCough_Spearman_Drop <- data.frame(ResCough_Spearman_Drop, row.names = 1)
ResCough_Spearman_Drop <- as.data.frame(t(ResCough_Spearman_Drop))

ResBreath_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/Spearman_ResBreath.csv", header = TRUE)
ResBreath_Spearman_Aero <- data.frame(ResBreath_Spearman_Aero, row.names = 1)
ResBreath_Spearman_Aero <- as.data.frame(t(ResBreath_Spearman_Aero))

Cumulative_Spearman_Cloth_Data <- list("Outdoor Aero" = Outdoor_Spearman_Aero,
                                          "Outdoor Drop" = Outdoor_Spearman_Drop,
                                          
                                          "Indoor Aero" = Indoor_Spearman_Aero,
                                          "Indoor Drop" = Indoor_Spearman_Drop,
                                          
                                          "Break Aero" = IndoorBreak_Spearman_Aero,
                                          "Break Drop" = IndoorBreak_Spearman_Drop,
                                          
                                          "Car Aero" = Car_Spearman_Aero,
                                          
                                          "Bus Aero" = Bus_Spearman_Aero,
                                          
                                          "ResCough Aero" = ResCough_Spearman_Aero,
                                          "ResCough Drop" = ResCough_Spearman_Drop,
                                          
                                          "ResBreath Aero" = ResBreath_Spearman_Aero)

write.xlsx(Cumulative_Spearman_Cloth_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth Mask Spearman Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)

##### Cloth Mask Aero Ratio Dataset Creation ----------

Indoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Indoor.csv", header = TRUE)
Indoor_Ratio <- data.frame(Indoor_Ratio, row.names = 1)

Outdoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio <- data.frame(Outdoor_Ratio, row.names = 1)

Car_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Car.csv", header = TRUE)
Car_Ratio <- data.frame(Car_Ratio, row.names = 1)

Bus_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_Bus.csv", header = TRUE)
Bus_Ratio <- data.frame(Bus_Ratio, row.names = 1)

IndoorBreak_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio <- data.frame(IndoorBreak_Ratio, row.names = 1)

ResCough_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_ResCough.csv", header = TRUE)
ResCough_Ratio <- data.frame(ResCough_Ratio, row.names = 1)

ResBreath_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/AeroRatios_ResBreath.csv", header = TRUE)
ResBreath_Ratio <- data.frame(ResBreath_Ratio, row.names = 1)

Cumulative_Ratio_Cloth_Data <- list("Outdoor Ratio" = Outdoor_Ratio,
                                       
                                       "Indoor Ratio" = Indoor_Ratio,
                                       
                                       "Break Ratio" = IndoorBreak_Ratio,
                                       
                                       "Car Ratio" = Car_Ratio,
                                       
                                       "Bus Ratio" = Bus_Ratio,
                                       
                                       "ResCough Ratio" = ResCough_Ratio,
                                       
                                       "ResBreath Ratio" = ResBreath_Ratio)

write.xlsx(Cumulative_Ratio_Cloth_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth Mask mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)

##### Cloth Mask Droplet Ratio Dataset Creation ----------
Indoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_Indoor.csv", header = TRUE)
Indoor_Ratio_Drop <- data.frame(Indoor_Ratio_Drop, row.names = 1)

Outdoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio_Drop <- data.frame(Outdoor_Ratio_Drop, row.names = 1)

IndoorBreak_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio_Drop <- data.frame(IndoorBreak_Ratio_Drop, row.names = 1)

ResCough_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth/DropRatios_ResCough.csv", header = TRUE)
ResCough_Ratio_Drop <- data.frame(ResCough_Ratio_Drop, row.names = 1)

Cumulative_Ratio_Cloth_Data_Drop <- list("Outdoor Ratio" = Outdoor_Ratio_Drop,
                                            
                                            "Indoor Ratio" = Indoor_Ratio_Drop,
                                            
                                            "Break Ratio" = IndoorBreak_Ratio_Drop,
                                            
                                            "ResCough Ratio" = ResCough_Ratio_Drop)

write.xlsx(Cumulative_Ratio_Cloth_Data_Drop, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Cloth Mask Drop mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)



##### Surgical Mask Spearman Dataset Creation ----------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
library(data.table)

Indoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Indoor.csv", header = TRUE)
Indoor_Spearman_Aero <- data.frame(Indoor_Spearman_Aero, row.names = 1)
Indoor_Spearman_Aero <- as.data.frame(t(Indoor_Spearman_Aero))

Indoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_Indoor.csv", header = TRUE)
Indoor_Spearman_Drop <- data.frame(Indoor_Spearman_Drop, row.names = 1)
Indoor_Spearman_Drop <- as.data.frame(t(Indoor_Spearman_Drop))

Outdoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Aero <- data.frame(Outdoor_Spearman_Aero, row.names = 1)
Outdoor_Spearman_Aero <- as.data.frame(t(Outdoor_Spearman_Aero))

Outdoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Drop <- data.frame(Outdoor_Spearman_Drop, row.names = 1)
Outdoor_Spearman_Drop <- as.data.frame(t(Outdoor_Spearman_Drop))

Car_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Car.csv", header = TRUE)
Car_Spearman_Aero <- data.frame(Car_Spearman_Aero, row.names = 1)
Car_Spearman_Aero <- as.data.frame(t(Car_Spearman_Aero))

Bus_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_Bus.csv", header = TRUE)
Bus_Spearman_Aero <- data.frame(Bus_Spearman_Aero, row.names = 1)
Bus_Spearman_Aero <- as.data.frame(t(Bus_Spearman_Aero))

IndoorBreak_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Aero <- data.frame(IndoorBreak_Spearman_Aero, row.names = 1)
IndoorBreak_Spearman_Aero <- as.data.frame(t(IndoorBreak_Spearman_Aero))

IndoorBreak_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Drop <- data.frame(IndoorBreak_Spearman_Drop, row.names = 1)
IndoorBreak_Spearman_Drop <- as.data.frame(t(IndoorBreak_Spearman_Drop))

ResCough_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_ResCough.csv", header = TRUE)
ResCough_Spearman_Aero <- data.frame(ResCough_Spearman_Aero, row.names = 1)
ResCough_Spearman_Aero <- as.data.frame(t(ResCough_Spearman_Aero))

ResCough_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/SpearmanDrop_ResCough.csv", header = TRUE)
ResCough_Spearman_Drop <- data.frame(ResCough_Spearman_Drop, row.names = 1)
ResCough_Spearman_Drop <- as.data.frame(t(ResCough_Spearman_Drop))

ResBreath_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/Spearman_ResBreath.csv", header = TRUE)
ResBreath_Spearman_Aero <- data.frame(ResBreath_Spearman_Aero, row.names = 1)
ResBreath_Spearman_Aero <- as.data.frame(t(ResBreath_Spearman_Aero))

Cumulative_Spearman_Surgical_Data <- list("Outdoor Aero" = Outdoor_Spearman_Aero,
                                       "Outdoor Drop" = Outdoor_Spearman_Drop,
                                       
                                       "Indoor Aero" = Indoor_Spearman_Aero,
                                       "Indoor Drop" = Indoor_Spearman_Drop,
                                       
                                       "Break Aero" = IndoorBreak_Spearman_Aero,
                                       "Break Drop" = IndoorBreak_Spearman_Drop,
                                       
                                       "Car Aero" = Car_Spearman_Aero,
                                       
                                       "Bus Aero" = Bus_Spearman_Aero,
                                       
                                       "ResCough Aero" = ResCough_Spearman_Aero,
                                       "ResCough Drop" = ResCough_Spearman_Drop,
                                       
                                       "ResBreath Aero" = ResBreath_Spearman_Aero)

write.xlsx(Cumulative_Spearman_Surgical_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical Mask Spearman Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)

##### Surgical Mask Aero Ratio Dataset Creation ----------

Indoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Indoor.csv", header = TRUE)
Indoor_Ratio <- data.frame(Indoor_Ratio, row.names = 1)

Outdoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio <- data.frame(Outdoor_Ratio, row.names = 1)

Car_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Car.csv", header = TRUE)
Car_Ratio <- data.frame(Car_Ratio, row.names = 1)

Bus_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_Bus.csv", header = TRUE)
Bus_Ratio <- data.frame(Bus_Ratio, row.names = 1)

IndoorBreak_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio <- data.frame(IndoorBreak_Ratio, row.names = 1)

ResCough_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_ResCough.csv", header = TRUE)
ResCough_Ratio <- data.frame(ResCough_Ratio, row.names = 1)

ResBreath_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/AeroRatios_ResBreath.csv", header = TRUE)
ResBreath_Ratio <- data.frame(ResBreath_Ratio, row.names = 1)

Cumulative_Ratio_Surgical_Data <- list("Outdoor Ratio" = Outdoor_Ratio,
                                    
                                    "Indoor Ratio" = Indoor_Ratio,
                                    
                                    "Break Ratio" = IndoorBreak_Ratio,
                                    
                                    "Car Ratio" = Car_Ratio,
                                    
                                    "Bus Ratio" = Bus_Ratio,
                                    
                                    "ResCough Ratio" = ResCough_Ratio,
                                    
                                    "ResBreath Ratio" = ResBreath_Ratio)

write.xlsx(Cumulative_Ratio_Surgical_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical Mask mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)
##### Surgical Mask Droplet Ratio Dataset Creation ----------
Indoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_Indoor.csv", header = TRUE)
Indoor_Ratio_Drop <- data.frame(Indoor_Ratio_Drop, row.names = 1)

Outdoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio_Drop <- data.frame(Outdoor_Ratio_Drop, row.names = 1)

IndoorBreak_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio_Drop <- data.frame(IndoorBreak_Ratio_Drop, row.names = 1)

ResCough_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical/DropRatios_ResCough.csv", header = TRUE)
ResCough_Ratio_Drop <- data.frame(ResCough_Ratio_Drop, row.names = 1)

Cumulative_Ratio_Surgical_Data_Drop <- list("Outdoor Ratio" = Outdoor_Ratio_Drop,
                                         
                                         "Indoor Ratio" = Indoor_Ratio_Drop,
                                         
                                         "Break Ratio" = IndoorBreak_Ratio_Drop,
                                         
                                         "ResCough Ratio" = ResCough_Ratio_Drop)

write.xlsx(Cumulative_Ratio_Surgical_Data_Drop, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Surgical Mask Drop mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)



##### Double Mask Spearman Dataset Creation ----------
rm(list=ls()) #clear all variables (this is good to have at the top of the script to clear any preexisting variables in your environment that might interfere)
library(data.table)

Indoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Indoor.csv", header = TRUE)
Indoor_Spearman_Aero <- data.frame(Indoor_Spearman_Aero, row.names = 1)
Indoor_Spearman_Aero <- as.data.frame(t(Indoor_Spearman_Aero))

Indoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_Indoor.csv", header = TRUE)
Indoor_Spearman_Drop <- data.frame(Indoor_Spearman_Drop, row.names = 1)
Indoor_Spearman_Drop <- as.data.frame(t(Indoor_Spearman_Drop))

Outdoor_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Aero <- data.frame(Outdoor_Spearman_Aero, row.names = 1)
Outdoor_Spearman_Aero <- as.data.frame(t(Outdoor_Spearman_Aero))

Outdoor_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_Outdoor.csv", header = TRUE)
Outdoor_Spearman_Drop <- data.frame(Outdoor_Spearman_Drop, row.names = 1)
Outdoor_Spearman_Drop <- as.data.frame(t(Outdoor_Spearman_Drop))

Car_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Car.csv", header = TRUE)
Car_Spearman_Aero <- data.frame(Car_Spearman_Aero, row.names = 1)
Car_Spearman_Aero <- as.data.frame(t(Car_Spearman_Aero))

Bus_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_Bus.csv", header = TRUE)
Bus_Spearman_Aero <- data.frame(Bus_Spearman_Aero, row.names = 1)
Bus_Spearman_Aero <- as.data.frame(t(Bus_Spearman_Aero))

IndoorBreak_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Aero <- data.frame(IndoorBreak_Spearman_Aero, row.names = 1)
IndoorBreak_Spearman_Aero <- as.data.frame(t(IndoorBreak_Spearman_Aero))

IndoorBreak_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_IndoorBreak.csv", header = TRUE)
IndoorBreak_Spearman_Drop <- data.frame(IndoorBreak_Spearman_Drop, row.names = 1)
IndoorBreak_Spearman_Drop <- as.data.frame(t(IndoorBreak_Spearman_Drop))

ResCough_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_ResCough.csv", header = TRUE)
ResCough_Spearman_Aero <- data.frame(ResCough_Spearman_Aero, row.names = 1)
ResCough_Spearman_Aero <- as.data.frame(t(ResCough_Spearman_Aero))

ResCough_Spearman_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/SpearmanDrop_ResCough.csv", header = TRUE)
ResCough_Spearman_Drop <- data.frame(ResCough_Spearman_Drop, row.names = 1)
ResCough_Spearman_Drop <- as.data.frame(t(ResCough_Spearman_Drop))

ResBreath_Spearman_Aero <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/Spearman_ResBreath.csv", header = TRUE)
ResBreath_Spearman_Aero <- data.frame(ResBreath_Spearman_Aero, row.names = 1)
ResBreath_Spearman_Aero <- as.data.frame(t(ResBreath_Spearman_Aero))

Cumulative_Spearman_Double_Data <- list("Outdoor Aero" = Outdoor_Spearman_Aero,
                                          "Outdoor Drop" = Outdoor_Spearman_Drop,
                                          
                                          "Indoor Aero" = Indoor_Spearman_Aero,
                                          "Indoor Drop" = Indoor_Spearman_Drop,
                                          
                                          "Break Aero" = IndoorBreak_Spearman_Aero,
                                          "Break Drop" = IndoorBreak_Spearman_Drop,
                                          
                                          "Car Aero" = Car_Spearman_Aero,
                                          
                                          "Bus Aero" = Bus_Spearman_Aero,
                                          
                                          "ResCough Aero" = ResCough_Spearman_Aero,
                                          "ResCough Drop" = ResCough_Spearman_Drop,
                                          
                                          "ResBreath Aero" = ResBreath_Spearman_Aero)

write.xlsx(Cumulative_Spearman_Double_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double Mask Spearman Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)

##### Double Mask Aero Ratio Dataset Creation ----------
Indoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Indoor.csv", header = TRUE)
Indoor_Ratio <- data.frame(Indoor_Ratio, row.names = 1)

Outdoor_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio <- data.frame(Outdoor_Ratio, row.names = 1)

Car_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Car.csv", header = TRUE)
Car_Ratio <- data.frame(Car_Ratio, row.names = 1)

Bus_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_Bus.csv", header = TRUE)
Bus_Ratio <- data.frame(Bus_Ratio, row.names = 1)

IndoorBreak_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio <- data.frame(IndoorBreak_Ratio, row.names = 1)

ResCough_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_ResCough.csv", header = TRUE)
ResCough_Ratio <- data.frame(ResCough_Ratio, row.names = 1)

ResBreath_Ratio <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/AeroRatios_ResBreath.csv", header = TRUE)
ResBreath_Ratio <- data.frame(ResBreath_Ratio, row.names = 1)

Cumulative_Ratio_Double_Data <- list("Outdoor Ratio" = Outdoor_Ratio,
                                       
                                       "Indoor Ratio" = Indoor_Ratio,
                                       
                                       "Break Ratio" = IndoorBreak_Ratio,
                                       
                                       "Car Ratio" = Car_Ratio,
                                       
                                       "Bus Ratio" = Bus_Ratio,
                                       
                                       "ResCough Ratio" = ResCough_Ratio,
                                       
                                       "ResBreath Ratio" = ResBreath_Ratio)

write.xlsx(Cumulative_Ratio_Double_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double Mask mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)
##### Double Mask Droplet Ratio Dataset Creation ----------
Indoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_Indoor.csv", header = TRUE)
Indoor_Ratio_Drop <- data.frame(Indoor_Ratio_Drop, row.names = 1)

Outdoor_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_Outdoor.csv", header = TRUE)
Outdoor_Ratio_Drop <- data.frame(Outdoor_Ratio_Drop, row.names = 1)

IndoorBreak_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_IndoorBreak.csv", header = TRUE)
IndoorBreak_Ratio_Drop <- data.frame(IndoorBreak_Ratio_Drop, row.names = 1)

ResCough_Ratio_Drop <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double/DropRatios_ResCough.csv", header = TRUE)
ResCough_Ratio_Drop <- data.frame(ResCough_Ratio_Drop, row.names = 1)

Cumulative_Ratio_Double_Data_Drop <- list("Outdoor Ratio" = Outdoor_Ratio_Drop,
                                            
                                            "Indoor Ratio" = Indoor_Ratio_Drop,
                                            
                                            "Break Ratio" = IndoorBreak_Ratio_Drop,
                                            
                                            "ResCough Ratio" = ResCough_Ratio_Drop)

write.xlsx(Cumulative_Ratio_Double_Data_Drop, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Data Files/New 2022 Sensitivity/Double Mask Drop mcRatio Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)

