#This is the most up to date version as of Oct 15 2021 - 

# Initial Organization -------------------------------
# Open Packages
library(mvtnorm)
library(mc2d) 
library(dplyr)
library(openxlsx)
set.seed(12345)
ndvar(10001)
library(data.table)

#dose-response parameter updated to Julian et al 2020 preprint
krisk=0.00680

#This is for two doses of Pfizer / Moderna (86-99%) Mean: 92.3% (95%: 86.3-98.7%)
mRNAvaccine <- mcstoc(runif, type="V",min=0.01, max =0.14)
#This is the vaccine efficacy for someone with comorbidities or is immunocompromised and encompasses variants that aren't as effected by the vaccine
  #64-80% Mean: 72% (95%: 64.4-79.6%)
reducedVEvaccine <- mcstoc(runif, type="V",min=0.20, max =0.36)
mRNAvaccine <- unmc (mRNAvaccine,drop = TRUE)
mRNAvaccine <- as.data.frame(mRNAvaccine)
reducedVEvaccine<-unmc (reducedVEvaccine,drop = TRUE)
reducedVEvaccine<-as.data.frame(reducedVEvaccine)

# Baseline Dataset Acquisition -----------------------

Transport <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/CarTransport.csv")
Transport_Baseline <- Transport %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/Bus.csv")
Bus_Baseline <- Bus %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_Baseline <- Bus %>%
  select(af2m1h)

ResCough <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/ResCough.csv")
ResCough_Baseline <- ResCough %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/ResBreath.csv")
ResBreath_Baseline <- ResBreath %>%
  select(aero8h)

Indoor1m <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/Indoor.csv")
Indoor1m_Baseline <- Indoor1m %>%
  select(af1m11h)

Indoor2m <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/Indoor.csv")
Indoor2m_Baseline <- Indoor2m %>%
  select(af2m11h)

Break <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/Indoor.csv")
Break_Baseline <- Break %>%
  select(af2m1h)

Outdoor1m <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/Outdoor.csv")
Outdoor1m_Baseline <- Outdoor1m %>%
  select(af1m11h)

Outdoor2m <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Baseline Dose/Outdoor.csv")
Outdoor2m_Baseline <- Outdoor2m %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor1m_Baseline <- Outdoor1m_Baseline + Bus_Break_Baseline
Combined_Outdoor1m_Baseline <- Combined_Outdoor1m_Baseline %>%
  rename(Outdoor1mDose12hr = af1m11h)

Combined_Outdoor2m_Baseline <- Outdoor2m_Baseline + Bus_Break_Baseline
Combined_Outdoor2m_Baseline <- Combined_Outdoor2m_Baseline %>%
  rename(Outdoor2mDose12hr = af2m11h)

Combined_Indoor1m_Baseline <- Indoor1m_Baseline + Break_Baseline
Combined_Indoor1m_Baseline <- Combined_Indoor1m_Baseline %>%
  rename(Indoor1mDose12hr = af1m11h)

Combined_Indoor2m_Baseline <- Indoor2m_Baseline + Break_Baseline
Combined_Indoor2m_Baseline <- Combined_Indoor2m_Baseline %>%
  rename(Indoor2mDose12hr = af2m11h)

Combined_Residential_Baseline <- ResBreath_Baseline + ResCough_Baseline
Combined_Residential_Baseline <- Combined_Residential_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor1m_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_Baseline, Transport_Baseline)
Indoor1m_Baseline_Cumulative_Dose <- Indoor1m_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor2m_Baseline_Cumulative_Dose <- cbind(Combined_Indoor2m_Baseline, Transport_Baseline)
Indoor2m_Baseline_Cumulative_Dose <- Indoor2m_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor1m_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor1m_Baseline,Combined_Residential_Baseline, Bus_Baseline)
Outdoor1m_Baseline_Cumulative_Dose <- Outdoor1m_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_Baseline,Combined_Residential_Baseline, Bus_Baseline)
Outdoor2m_Baseline_Cumulative_Dose <- Outdoor2m_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Baseline Risk ----------------------------------------------------------

# Risk for Indoor Worker (1m)
Indoor1m_Risk = 1-exp(-krisk*Indoor1m_Baseline_Cumulative_Dose)
Indoor1m_Risk_Vax <- cbind(Indoor1m_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_Risk_Vax_Full <- mutate(Indoor1m_Risk_Vax,
                               mRNA_Indoor = mRNAvaccine * Indoor1mDose12hr,
                               mRNA_Car = mRNAvaccine * CarDose2hr,
                               mRNA_Total = mRNAvaccine * TotalDose,
                               
                               RedVE_Indoor = reducedVEvaccine * Indoor1mDose12hr,
                               RedVE_Car = reducedVEvaccine * CarDose2hr,
                               RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(Indoor1mRisk12hr = Indoor1mDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_Risk_Quant <- as.data.frame(t(apply(Indoor1m_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_Risk_Mean <- as.data.frame(colMeans(Indoor1m_Risk_Vax_Full))
Indoor1m_Baseline_Combined_Risk <- cbind(Indoor1m_Risk_Quant, Indoor1m_Risk_Mean)
write.csv(Indoor1m_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_Baseline.csv", row.names = TRUE)

# Risk for Indoor Worker (2m)
Indoor2m_Risk = 1-exp(-krisk*Indoor2m_Baseline_Cumulative_Dose)
Indoor2m_Risk_Vax <- cbind(Indoor2m_Risk, mRNAvaccine, reducedVEvaccine)
Indoor2m_Risk_Vax_Full <- mutate(Indoor2m_Risk_Vax,
                                 mRNA_Indoor = mRNAvaccine * Indoor2mDose12hr,
                                 mRNA_Car = mRNAvaccine * CarDose2hr,
                                 mRNA_Total = mRNAvaccine * TotalDose,
                                 
                                 RedVE_Indoor = reducedVEvaccine * Indoor2mDose12hr,
                                 RedVE_Car = reducedVEvaccine * CarDose2hr,
                                 RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(Indoor2mRisk12hr = Indoor2mDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor2m_Risk_Quant <- as.data.frame(t(apply(Indoor2m_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor2m_Risk_Mean <- as.data.frame(colMeans(Indoor2m_Risk_Vax_Full))
Indoor2m_Baseline_Combined_Risk <- cbind(Indoor2m_Risk_Quant, Indoor2m_Risk_Mean)
write.csv(Indoor2m_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor2m_Baseline.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor1m_Risk = 1-exp(-krisk*Outdoor1m_Baseline_Cumulative_Dose)
Outdoor1m_Risk_Vax <- cbind(Outdoor1m_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor1m_Risk_Vax_Full <- mutate(Outdoor1m_Risk_Vax,
                               mRNA_Outdoor = mRNAvaccine * Outdoor1mDose12hr,
                               mRNA_Res = mRNAvaccine * ResDose10hr,
                               mRNA_Bus = mRNAvaccine * BusDose2hr,
                               mRNA_Total = mRNAvaccine * TotalDose,
                               
                               RedVE_Outdoor = reducedVEvaccine * Outdoor1mDose12hr,
                               RedVE_Res = reducedVEvaccine * ResDose10hr,
                               RedVE_Bus = reducedVEvaccine * BusDose2hr,
                               RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(Outdoor1mRisk12hr = Outdoor1mDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor1m_Risk_Quant <- as.data.frame(t(apply(Outdoor1m_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor1m_Risk_Mean <- as.data.frame(colMeans(Outdoor1m_Risk_Vax_Full))
Outdoor1m_Baseline_Combined_Risk <- cbind(Outdoor1m_Risk_Quant, Outdoor1m_Risk_Mean)
write.csv(Outdoor1m_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor1m_Baseline.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_Risk = 1-exp(-krisk*Outdoor2m_Baseline_Cumulative_Dose)
Outdoor2m_Risk_Vax <- cbind(Outdoor2m_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_Risk_Vax_Full <- mutate(Outdoor2m_Risk_Vax,
                                  mRNA_Outdoor = mRNAvaccine * Outdoor2mDose12hr,
                                  mRNA_Res = mRNAvaccine * ResDose10hr,
                                  mRNA_Bus = mRNAvaccine * BusDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Outdoor = reducedVEvaccine * Outdoor2mDose12hr,
                                  RedVE_Res = reducedVEvaccine * ResDose10hr,
                                  RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(Outdoor2mRisk12hr = Outdoor2mDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_Risk_Vax_Full))
Outdoor2m_Baseline_Combined_Risk <- cbind(Outdoor2m_Risk_Quant, Outdoor2m_Risk_Mean)
write.csv(Outdoor2m_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_Baseline.csv", row.names = TRUE)

# Hand-Hygiene Dataset Acquisition -------------------------
Transport_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/CarTransport.csv")
Transport_HH_Baseline <- Transport_HH %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/Bus.csv")
Bus_HH_Baseline <- Bus_HH %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_HH_Baseline <- Bus_HH %>%
  select(af2m1h)

ResCough_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/ResCough.csv")
ResCough_HH_Baseline <- ResCough_HH %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/ResBreath.csv")
ResBreath_HH_Baseline <- ResBreath_HH %>%
  select(aero8h)

Indoor_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/Indoor.csv")
Indoor_HH_Baseline <- Indoor_HH %>%
  select(af2m11h)

Indoor1m_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/Indoor.csv")
Indoor1m_HH_Baseline <- Indoor1m_HH %>%
  select(af1m11h)

Break_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/Indoor.csv")
Break_HH_Baseline <- Break_HH %>%
  select(af2m1h)

Outdoor_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/Outdoor.csv")
Outdoor_HH_Baseline <- Outdoor_HH %>%
  select(af1m11h)

Outdoor2m_HH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Hand Hygiene Dose/Outdoor.csv")
Outdoor2m_HH_Baseline <- Outdoor2m_HH %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor_HH_Baseline <- Outdoor_HH_Baseline + Bus_Break_HH_Baseline
Combined_Outdoor_HH_Baseline <- Combined_Outdoor_HH_Baseline %>%
  rename(OutdoorDose12hr = af1m11h)

Combined_Outdoor2m_HH_Baseline <- Outdoor2m_HH_Baseline + Bus_Break_HH_Baseline
Combined_Outdoor2m_HH_Baseline <- Combined_Outdoor2m_HH_Baseline %>%
  rename(OutdoorDose12hr = af2m11h)

Combined_Indoor_HH_Baseline <- Indoor_HH_Baseline + Break_HH_Baseline
Combined_Indoor_HH_Baseline <- Combined_Indoor_HH_Baseline %>%
  rename(IndoorDose12hr = af2m11h)

Combined_Indoor1m_HH_Baseline <- Indoor1m_HH_Baseline + Break_HH_Baseline
Combined_Indoor1m_HH_Baseline <- Combined_Indoor1m_HH_Baseline %>%
  rename(IndoorDose12hr = af1m11h)

Combined_Residential_HH_Baseline <- ResBreath_HH_Baseline + ResCough_HH_Baseline
Combined_Residential_HH_Baseline <- Combined_Residential_HH_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor_HH_Baseline_Cumulative_Dose <- cbind(Combined_Indoor_HH_Baseline, Transport_HH_Baseline)
Indoor_HH_Baseline_Cumulative_Dose <- Indoor_HH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor1m_HH_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_HH_Baseline, Transport_HH_Baseline)
Indoor1m_HH_Baseline_Cumulative_Dose <- Indoor1m_HH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor_HH_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor_HH_Baseline,Combined_Residential_HH_Baseline, Bus_HH_Baseline)
Outdoor_HH_Baseline_Cumulative_Dose <- Outdoor_HH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_HH_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_HH_Baseline,Combined_Residential_HH_Baseline, Bus_HH_Baseline)
Outdoor2m_HH_Baseline_Cumulative_Dose <- Outdoor2m_HH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Hand Hygiene Risk ----------------------------------------------------------

# Risk for Indoor Worker (2m)
Indoor_HH_Risk = 1-exp(-krisk*Indoor_HH_Baseline_Cumulative_Dose)
Indoor_HH_Risk_Vax <- cbind(Indoor_HH_Risk, mRNAvaccine, reducedVEvaccine)
Indoor_HH_Risk_Vax_Full <- mutate(Indoor_HH_Risk_Vax,
                               mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                               mRNA_Car = mRNAvaccine * CarDose2hr,
                               mRNA_Total = mRNAvaccine * TotalDose,
                               
                               RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                               RedVE_Car = reducedVEvaccine * CarDose2hr,
                               RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor_HH_Risk_Quant <- as.data.frame(t(apply(Indoor_HH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor_HH_Risk_Mean <- as.data.frame(colMeans(Indoor_HH_Risk_Vax_Full))
Indoor_HH_Baseline_Combined_Risk <- cbind(Indoor_HH_Risk_Quant, Indoor_HH_Risk_Mean)
write.csv(Indoor_HH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_HH.csv", row.names = TRUE)

# Risk for Indoor Worker (1m)
Indoor1m_HH_Risk = 1-exp(-krisk*Indoor1m_HH_Baseline_Cumulative_Dose)
Indoor1m_HH_Risk_Vax <- cbind(Indoor1m_HH_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_HH_Risk_Vax_Full <- mutate(Indoor1m_HH_Risk_Vax,
                                  mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                  mRNA_Car = mRNAvaccine * CarDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                  RedVE_Car = reducedVEvaccine * CarDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_HH_Risk_Quant <- as.data.frame(t(apply(Indoor1m_HH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_HH_Risk_Mean <- as.data.frame(colMeans(Indoor1m_HH_Risk_Vax_Full))
Indoor1m_HH_Baseline_Combined_Risk <- cbind(Indoor1m_HH_Risk_Quant, Indoor1m_HH_Risk_Mean)
write.csv(Indoor1m_HH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_HH.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor_HH_Risk = 1-exp(-krisk*Outdoor_HH_Baseline_Cumulative_Dose)
Outdoor_HH_Risk_Vax <- cbind(Outdoor_HH_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor_HH_Risk_Vax_Full <- mutate(Outdoor_HH_Risk_Vax,
                                mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                mRNA_Res = mRNAvaccine * ResDose10hr,
                                mRNA_Bus = mRNAvaccine * BusDose2hr,
                                mRNA_Total = mRNAvaccine * TotalDose,
                                
                                RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                RedVE_Res = reducedVEvaccine * ResDose10hr,
                                RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor_HH_Risk_Quant <- as.data.frame(t(apply(Outdoor_HH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor_HH_Risk_Mean <- as.data.frame(colMeans(Outdoor_HH_Risk_Vax_Full))
Outdoor_HH_Baseline_Combined_Risk <- cbind(Outdoor_HH_Risk_Quant, Outdoor_HH_Risk_Mean)
write.csv(Outdoor_HH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_HH.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_HH_Risk = 1-exp(-krisk*Outdoor2m_HH_Baseline_Cumulative_Dose)
Outdoor2m_HH_Risk_Vax <- cbind(Outdoor2m_HH_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_HH_Risk_Vax_Full <- mutate(Outdoor2m_HH_Risk_Vax,
                                   mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                   mRNA_Res = mRNAvaccine * ResDose10hr,
                                   mRNA_Bus = mRNAvaccine * BusDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                   RedVE_Res = reducedVEvaccine * ResDose10hr,
                                   RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_HH_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_HH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_HH_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_HH_Risk_Vax_Full))
Outdoor2m_HH_Baseline_Combined_Risk <- cbind(Outdoor2m_HH_Risk_Quant, Outdoor2m_HH_Risk_Mean)
write.csv(Outdoor2m_HH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_HH.csv", row.names = TRUE)


# Surgical Mask Dataset Acquisition -------------------------
Transport_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/CarTransport.csv")
Transport_SM_Baseline <- Transport_SM %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Bus.csv")
Bus_SM_Baseline <- Bus_SM %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_SM_Baseline <- Bus_Break_Baseline %>%
  select(af2m1h)

ResCough_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/ResCough.csv")
ResCough_SM_Baseline <- ResCough_SM %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/ResBreath.csv")
ResBreath_SM_Baseline <- ResBreath_SM %>%
  select(aero8h)

Indoor_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Indoor.csv")
Indoor_SM_Baseline <- Indoor_SM %>%
  select(af2m11h)

Indoor1m_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Indoor.csv")
Indoor1m_SM_Baseline <- Indoor1m_SM %>%
  select(af1m11h)

Break_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Indoor.csv")
Break_SM_Baseline <- Break_SM %>%
  select(af2m1h)

Outdoor_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Outdoor.csv")
Outdoor_SM_Baseline <- Outdoor_SM %>%
  select(af1m11h)

Outdoor2m_SM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Mask Dose/Outdoor.csv")
Outdoor2m_SM_Baseline <- Outdoor2m_SM %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor_SM_Baseline <- Outdoor_SM_Baseline + Bus_Break_SM_Baseline
Combined_Outdoor_SM_Baseline <- Combined_Outdoor_SM_Baseline %>%
  rename(OutdoorDose12hr = af1m11h)

Combined_Outdoor2m_SM_Baseline <- Outdoor2m_SM_Baseline + Bus_Break_SM_Baseline
Combined_Outdoor2m_SM_Baseline <- Combined_Outdoor2m_SM_Baseline %>%
  rename(OutdoorDose12hr = af2m11h)

Combined_Indoor_SM_Baseline <- Indoor_SM_Baseline + Break_SM_Baseline
Combined_Indoor_SM_Baseline <- Combined_Indoor_SM_Baseline %>%
  rename(IndoorDose12hr = af2m11h)

Combined_Indoor1m_SM_Baseline <- Indoor1m_SM_Baseline + Break_SM_Baseline
Combined_Indoor1m_SM_Baseline <- Combined_Indoor1m_SM_Baseline %>%
  rename(IndoorDose12hr = af1m11h)

Combined_Residential_SM_Baseline <- ResBreath_SM_Baseline + ResCough_SM_Baseline
Combined_Residential_SM_Baseline <- Combined_Residential_SM_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor_SM_Baseline_Cumulative_Dose <- cbind(Combined_Indoor_SM_Baseline, Transport_SM_Baseline)
Indoor_SM_Baseline_Cumulative_Dose <- Indoor_SM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor1m_SM_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_SM_Baseline, Transport_SM_Baseline)
Indoor1m_SM_Baseline_Cumulative_Dose <- Indoor1m_SM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor_SM_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor_SM_Baseline,Combined_Residential_SM_Baseline, Bus_SM_Baseline)
Outdoor_SM_Baseline_Cumulative_Dose <- Outdoor_SM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_SM_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_SM_Baseline,Combined_Residential_SM_Baseline, Bus_SM_Baseline)
Outdoor2m_SM_Baseline_Cumulative_Dose <- Outdoor2m_SM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Surgical Mask Risk ----------------------------------------------------------

# Risk for Indoor Worker (2m)
Indoor_SM_Risk = 1-exp(-krisk*Indoor_SM_Baseline_Cumulative_Dose)
Indoor_SM_Risk_Vax <- cbind(Indoor_SM_Risk, mRNAvaccine, reducedVEvaccine)
Indoor_SM_Risk_Vax_Full <- mutate(Indoor_SM_Risk_Vax,
                                  mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                  mRNA_Car = mRNAvaccine * CarDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                  RedVE_Car = reducedVEvaccine * CarDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor_SM_Risk_Quant <- as.data.frame(t(apply(Indoor_SM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor_SM_Risk_Mean <- as.data.frame(colMeans(Indoor_SM_Risk_Vax_Full))
Indoor_SM_Baseline_Combined_Risk <- cbind(Indoor_SM_Risk_Quant, Indoor_SM_Risk_Mean)
write.csv(Indoor_SM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_SMask.csv", row.names = TRUE)

# Risk for Indoor Worker (1m)
Indoor1m_SM_Risk = 1-exp(-krisk*Indoor1m_SM_Baseline_Cumulative_Dose)
Indoor1m_SM_Risk_Vax <- cbind(Indoor1m_SM_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_SM_Risk_Vax_Full <- mutate(Indoor1m_SM_Risk_Vax,
                                  mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                  mRNA_Car = mRNAvaccine * CarDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                  RedVE_Car = reducedVEvaccine * CarDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_SM_Risk_Quant <- as.data.frame(t(apply(Indoor1m_SM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_SM_Risk_Mean <- as.data.frame(colMeans(Indoor1m_SM_Risk_Vax_Full))
Indoor1m_SM_Baseline_Combined_Risk <- cbind(Indoor1m_SM_Risk_Quant, Indoor1m_SM_Risk_Mean)
write.csv(Indoor1m_SM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_SMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor_SM_Risk = 1-exp(-krisk*Outdoor_SM_Baseline_Cumulative_Dose)
Outdoor_SM_Risk_Vax <- cbind(Outdoor_SM_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor_SM_Risk_Vax_Full <- mutate(Outdoor_SM_Risk_Vax,
                                   mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                   mRNA_Res = mRNAvaccine * ResDose10hr,
                                   mRNA_Bus = mRNAvaccine * BusDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                   RedVE_Res = reducedVEvaccine * ResDose10hr,
                                   RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor_SM_Risk_Quant <- as.data.frame(t(apply(Outdoor_SM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor_SM_Risk_Mean <- as.data.frame(colMeans(Outdoor_SM_Risk_Vax_Full))
Outdoor_SM_Baseline_Combined_Risk <- cbind(Outdoor_SM_Risk_Quant, Outdoor_SM_Risk_Mean)
write.csv(Outdoor_SM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_SMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_SM_Risk = 1-exp(-krisk*Outdoor2m_SM_Baseline_Cumulative_Dose)
Outdoor2m_SM_Risk_Vax <- cbind(Outdoor2m_SM_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_SM_Risk_Vax_Full <- mutate(Outdoor2m_SM_Risk_Vax,
                                   mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                   mRNA_Res = mRNAvaccine * ResDose10hr,
                                   mRNA_Bus = mRNAvaccine * BusDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                   RedVE_Res = reducedVEvaccine * ResDose10hr,
                                   RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_SM_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_SM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_SM_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_SM_Risk_Vax_Full))
Outdoor2m_SM_Baseline_Combined_Risk <- cbind(Outdoor2m_SM_Risk_Quant, Outdoor2m_SM_Risk_Mean)
write.csv(Outdoor2m_SM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_SMask.csv", row.names = TRUE)

# Cloth Mask Dataset Acquisition -------------------------
Transport_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/CarTransport.csv")
Transport_CM_Baseline <- Transport_CM %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/Bus.csv")
Bus_CM_Baseline <- Bus_CM %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_CM_Baseline <- Bus_Break_Baseline %>%
  select(af2m1h)

ResCough_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/ResCough.csv")
ResCough_CM_Baseline <- ResCough_CM %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/ResBreath.csv")
ResBreath_CM_Baseline <- ResBreath_CM %>%
  select(aero8h)

Indoor_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/Indoor.csv")
Indoor_CM_Baseline <- Indoor_CM %>%
  select(af2m11h)

Indoor1m_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/Indoor.csv")
Indoor1m_CM_Baseline <- Indoor1m_CM %>%
  select(af1m11h)

Break_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/Indoor.csv")
Break_CM_Baseline <- Break_CM %>%
  select(af2m1h)

Outdoor_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/Outdoor.csv")
Outdoor_CM_Baseline <- Outdoor_CM %>%
  select(af1m11h)

Outdoor2m_CM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Cloth Mask Dose/Outdoor.csv")
Outdoor2m_CM_Baseline <- Outdoor2m_CM %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor_CM_Baseline <- Outdoor_CM_Baseline + Bus_Break_CM_Baseline
Combined_Outdoor_CM_Baseline <- Combined_Outdoor_CM_Baseline %>%
  rename(OutdoorDose12hr = af1m11h)

Combined_Outdoor2m_CM_Baseline <- Outdoor2m_CM_Baseline + Bus_Break_CM_Baseline
Combined_Outdoor2m_CM_Baseline <- Combined_Outdoor2m_CM_Baseline %>%
  rename(OutdoorDose12hr = af2m11h)

Combined_Indoor_CM_Baseline <- Indoor_CM_Baseline + Break_CM_Baseline
Combined_Indoor_CM_Baseline <- Combined_Indoor_CM_Baseline %>%
  rename(IndoorDose12hr = af2m11h)

Combined_Indoor1m_CM_Baseline <- Indoor1m_CM_Baseline + Break_CM_Baseline
Combined_Indoor1m_CM_Baseline <- Combined_Indoor1m_CM_Baseline %>%
  rename(IndoorDose12hr = af1m11h)

Combined_Residential_CM_Baseline <- ResBreath_CM_Baseline + ResCough_CM_Baseline
Combined_Residential_CM_Baseline <- Combined_Residential_CM_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor_CM_Baseline_Cumulative_Dose <- cbind(Combined_Indoor_CM_Baseline, Transport_CM_Baseline)
Indoor_CM_Baseline_Cumulative_Dose <- Indoor_CM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor1m_CM_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_CM_Baseline, Transport_CM_Baseline)
Indoor1m_CM_Baseline_Cumulative_Dose <- Indoor1m_CM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor_CM_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor_CM_Baseline,Combined_Residential_CM_Baseline, Bus_CM_Baseline)
Outdoor_CM_Baseline_Cumulative_Dose <- Outdoor_CM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_CM_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_CM_Baseline,Combined_Residential_CM_Baseline, Bus_CM_Baseline)
Outdoor2m_CM_Baseline_Cumulative_Dose <- Outdoor2m_CM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Cloth Mask Risk ----------------------------------------------------------

# Risk for Indoor Worker (2m)
Indoor_CM_Risk = 1-exp(-krisk*Indoor_CM_Baseline_Cumulative_Dose)
Indoor_CM_Risk_Vax <- cbind(Indoor_CM_Risk, mRNAvaccine, reducedVEvaccine)
Indoor_CM_Risk_Vax_Full <- mutate(Indoor_CM_Risk_Vax,
                                  mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                  mRNA_Car = mRNAvaccine * CarDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                  RedVE_Car = reducedVEvaccine * CarDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor_CM_Risk_Quant <- as.data.frame(t(apply(Indoor_CM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor_CM_Risk_Mean <- as.data.frame(colMeans(Indoor_CM_Risk_Vax_Full))
Indoor_CM_Baseline_Combined_Risk <- cbind(Indoor_CM_Risk_Quant, Indoor_CM_Risk_Mean)
write.csv(Indoor_CM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_CMask.csv", row.names = TRUE)

# Risk for Indoor Worker (1m)
Indoor1m_CM_Risk = 1-exp(-krisk*Indoor1m_CM_Baseline_Cumulative_Dose)
Indoor1m_CM_Risk_Vax <- cbind(Indoor1m_CM_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_CM_Risk_Vax_Full <- mutate(Indoor1m_CM_Risk_Vax,
                                    mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                    mRNA_Car = mRNAvaccine * CarDose2hr,
                                    mRNA_Total = mRNAvaccine * TotalDose,
                                    
                                    RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                    RedVE_Car = reducedVEvaccine * CarDose2hr,
                                    RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_CM_Risk_Quant <- as.data.frame(t(apply(Indoor1m_CM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_CM_Risk_Mean <- as.data.frame(colMeans(Indoor1m_CM_Risk_Vax_Full))
Indoor1m_CM_Baseline_Combined_Risk <- cbind(Indoor1m_CM_Risk_Quant, Indoor1m_CM_Risk_Mean)
write.csv(Indoor1m_CM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_CMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor_CM_Risk = 1-exp(-krisk*Outdoor_CM_Baseline_Cumulative_Dose)
Outdoor_CM_Risk_Vax <- cbind(Outdoor_CM_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor_CM_Risk_Vax_Full <- mutate(Outdoor_CM_Risk_Vax,
                                   mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                   mRNA_Res = mRNAvaccine * ResDose10hr,
                                   mRNA_Bus = mRNAvaccine * BusDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                   RedVE_Res = reducedVEvaccine * ResDose10hr,
                                   RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor_CM_Risk_Quant <- as.data.frame(t(apply(Outdoor_CM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor_CM_Risk_Mean <- as.data.frame(colMeans(Outdoor_CM_Risk_Vax_Full))
Outdoor_CM_Baseline_Combined_Risk <- cbind(Outdoor_CM_Risk_Quant, Outdoor_CM_Risk_Mean)
write.csv(Outdoor_CM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_CMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_CM_Risk = 1-exp(-krisk*Outdoor2m_CM_Baseline_Cumulative_Dose)
Outdoor2m_CM_Risk_Vax <- cbind(Outdoor2m_CM_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_CM_Risk_Vax_Full <- mutate(Outdoor2m_CM_Risk_Vax,
                                     mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                     mRNA_Res = mRNAvaccine * ResDose10hr,
                                     mRNA_Bus = mRNAvaccine * BusDose2hr,
                                     mRNA_Total = mRNAvaccine * TotalDose,
                                     
                                     RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                     RedVE_Res = reducedVEvaccine * ResDose10hr,
                                     RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                     RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_CM_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_CM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_CM_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_CM_Risk_Vax_Full))
Outdoor2m_CM_Baseline_Combined_Risk <- cbind(Outdoor2m_CM_Risk_Quant, Outdoor2m_CM_Risk_Mean)
write.csv(Outdoor2m_CM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_CMask.csv", row.names = TRUE)

# Double Mask Dataset Acquisition -------------------------
Transport_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/CarTransport.csv")
Transport_DM_Baseline <- Transport_DM %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/Bus.csv")
Bus_DM_Baseline <- Bus_DM %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_DM_Baseline <- Bus_Break_Baseline %>%
  select(af2m1h)

ResCough_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/ResCough.csv")
ResCough_DM_Baseline <- ResCough_DM %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/ResBreath.csv")
ResBreath_DM_Baseline <- ResBreath_DM %>%
  select(aero8h)

Indoor_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/Indoor.csv")
Indoor_DM_Baseline <- Indoor_DM %>%
  select(af2m11h)

Indoor1m_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/Indoor.csv")
Indoor1m_DM_Baseline <- Indoor1m_DM %>%
  select(af1m11h)

Break_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/Indoor.csv")
Break_DM_Baseline <- Break_DM %>%
  select(af2m1h)

Outdoor_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/Outdoor.csv")
Outdoor_DM_Baseline <- Outdoor_DM %>%
  select(af1m11h)

Outdoor2m_DM <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Double Mask Dose/Outdoor.csv")
Outdoor2m_DM_Baseline <- Outdoor2m_DM %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor_DM_Baseline <- Outdoor_DM_Baseline + Bus_Break_DM_Baseline
Combined_Outdoor_DM_Baseline <- Combined_Outdoor_DM_Baseline %>%
  rename(OutdoorDose12hr = af1m11h)

Combined_Outdoor2m_DM_Baseline <- Outdoor2m_DM_Baseline + Bus_Break_DM_Baseline
Combined_Outdoor2m_DM_Baseline <- Combined_Outdoor2m_DM_Baseline %>%
  rename(OutdoorDose12hr = af2m11h)

Combined_Indoor_DM_Baseline <- Indoor_DM_Baseline + Break_DM_Baseline
Combined_Indoor_DM_Baseline <- Combined_Indoor_DM_Baseline %>%
  rename(IndoorDose12hr = af2m11h)

Combined_Indoor1m_DM_Baseline <- Indoor1m_DM_Baseline + Break_DM_Baseline
Combined_Indoor1m_DM_Baseline <- Combined_Indoor1m_DM_Baseline %>%
  rename(IndoorDose12hr = af1m11h)

Combined_Residential_DM_Baseline <- ResBreath_DM_Baseline + ResCough_DM_Baseline
Combined_Residential_DM_Baseline <- Combined_Residential_DM_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor_DM_Baseline_Cumulative_Dose <- cbind(Combined_Indoor_DM_Baseline, Transport_DM_Baseline)
Indoor_DM_Baseline_Cumulative_Dose <- Indoor_DM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor1m_DM_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_DM_Baseline, Transport_DM_Baseline)
Indoor1m_DM_Baseline_Cumulative_Dose <- Indoor1m_DM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor_DM_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor_DM_Baseline,Combined_Residential_DM_Baseline, Bus_DM_Baseline)
Outdoor_DM_Baseline_Cumulative_Dose <- Outdoor_DM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_DM_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_DM_Baseline,Combined_Residential_DM_Baseline, Bus_DM_Baseline)
Outdoor2m_DM_Baseline_Cumulative_Dose <- Outdoor2m_DM_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Double Mask Risk ----------------------------------------------------------

# Risk for Indoor Worker (2m)
Indoor_DM_Risk = 1-exp(-krisk*Indoor_DM_Baseline_Cumulative_Dose)
Indoor_DM_Risk_Vax <- cbind(Indoor_DM_Risk, mRNAvaccine, reducedVEvaccine)
Indoor_DM_Risk_Vax_Full <- mutate(Indoor_DM_Risk_Vax,
                                  mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                  mRNA_Car = mRNAvaccine * CarDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                  RedVE_Car = reducedVEvaccine * CarDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor_DM_Risk_Quant <- as.data.frame(t(apply(Indoor_DM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor_DM_Risk_Mean <- as.data.frame(colMeans(Indoor_DM_Risk_Vax_Full))
Indoor_DM_Baseline_Combined_Risk <- cbind(Indoor_DM_Risk_Quant, Indoor_DM_Risk_Mean)
write.csv(Indoor_DM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_DMask.csv", row.names = TRUE)

# Risk for Indoor Worker (1m)
Indoor1m_DM_Risk = 1-exp(-krisk*Indoor1m_DM_Baseline_Cumulative_Dose)
Indoor1m_DM_Risk_Vax <- cbind(Indoor1m_DM_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_DM_Risk_Vax_Full <- mutate(Indoor1m_DM_Risk_Vax,
                                    mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                    mRNA_Car = mRNAvaccine * CarDose2hr,
                                    mRNA_Total = mRNAvaccine * TotalDose,
                                    
                                    RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                    RedVE_Car = reducedVEvaccine * CarDose2hr,
                                    RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_DM_Risk_Quant <- as.data.frame(t(apply(Indoor1m_DM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_DM_Risk_Mean <- as.data.frame(colMeans(Indoor1m_DM_Risk_Vax_Full))
Indoor1m_DM_Baseline_Combined_Risk <- cbind(Indoor1m_DM_Risk_Quant, Indoor1m_DM_Risk_Mean)
write.csv(Indoor1m_DM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_DMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor_DM_Risk = 1-exp(-krisk*Outdoor_DM_Baseline_Cumulative_Dose)
Outdoor_DM_Risk_Vax <- cbind(Outdoor_DM_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor_DM_Risk_Vax_Full <- mutate(Outdoor_DM_Risk_Vax,
                                   mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                   mRNA_Res = mRNAvaccine * ResDose10hr,
                                   mRNA_Bus = mRNAvaccine * BusDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                   RedVE_Res = reducedVEvaccine * ResDose10hr,
                                   RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor_DM_Risk_Quant <- as.data.frame(t(apply(Outdoor_DM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor_DM_Risk_Mean <- as.data.frame(colMeans(Outdoor_DM_Risk_Vax_Full))
Outdoor_DM_Baseline_Combined_Risk <- cbind(Outdoor_DM_Risk_Quant, Outdoor_DM_Risk_Mean)
write.csv(Outdoor_DM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_DMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_DM_Risk = 1-exp(-krisk*Outdoor2m_DM_Baseline_Cumulative_Dose)
Outdoor2m_DM_Risk_Vax <- cbind(Outdoor2m_DM_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_DM_Risk_Vax_Full <- mutate(Outdoor2m_DM_Risk_Vax,
                                     mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                     mRNA_Res = mRNAvaccine * ResDose10hr,
                                     mRNA_Bus = mRNAvaccine * BusDose2hr,
                                     mRNA_Total = mRNAvaccine * TotalDose,
                                     
                                     RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                     RedVE_Res = reducedVEvaccine * ResDose10hr,
                                     RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                     RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_DM_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_DM_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_DM_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_DM_Risk_Vax_Full))
Outdoor2m_DM_Baseline_Combined_Risk <- cbind(Outdoor2m_DM_Risk_Quant, Outdoor2m_DM_Risk_Mean)
write.csv(Outdoor2m_DM_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_DMask.csv", row.names = TRUE)

# Increased ACH  Dataset Acquisition -------------------------
Transport_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/CarTransport.csv")
Transport_ACH_Baseline <- Transport_ACH %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/Bus.csv")
Bus_ACH_Baseline <- Bus_ACH %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_ACH_Baseline <- Bus_Break_Baseline %>%
  select(af2m1h)

ResCough_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/ResCough.csv")
ResCough_ACH_Baseline <- ResCough_ACH %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/ResBreath.csv")
ResBreath_ACH_Baseline <- ResBreath_ACH %>%
  select(aero8h)

Indoor_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/Indoor.csv")
Indoor_ACH_Baseline <- Indoor_ACH %>%
  select(af2m11h)

Indoor1m_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/Indoor.csv")
Indoor1m_ACH_Baseline <- Indoor1m_ACH %>%
  select(af1m11h)

Break_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/Indoor.csv")
Break_ACH_Baseline <- Break_ACH %>%
  select(af2m1h)

Outdoor_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/Outdoor.csv")
Outdoor_ACH_Baseline <- Outdoor_ACH %>%
  select(af1m11h)

Outdoor2m_ACH <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/ACH Dose/Outdoor.csv")
Outdoor2m_ACH_Baseline <- Outdoor2m_ACH %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor_ACH_Baseline <- Outdoor_ACH_Baseline + Bus_Break_ACH_Baseline
Combined_Outdoor_ACH_Baseline <- Combined_Outdoor_ACH_Baseline %>%
  rename(OutdoorDose12hr = af1m11h)

Combined_Outdoor2m_ACH_Baseline <- Outdoor2m_ACH_Baseline + Bus_Break_ACH_Baseline
Combined_Outdoor2m_ACH_Baseline <- Combined_Outdoor2m_ACH_Baseline %>%
  rename(OutdoorDose12hr = af2m11h)

Combined_Indoor_ACH_Baseline <- Indoor_ACH_Baseline + Break_ACH_Baseline
Combined_Indoor_ACH_Baseline <- Combined_Indoor_ACH_Baseline %>%
  rename(IndoorDose12hr = af2m11h)

Combined_Indoor1m_ACH_Baseline <- Indoor1m_ACH_Baseline + Break_ACH_Baseline
Combined_Indoor1m_ACH_Baseline <- Combined_Indoor1m_ACH_Baseline %>%
  rename(IndoorDose12hr = af1m11h)

Combined_Residential_ACH_Baseline <- ResBreath_ACH_Baseline + ResCough_ACH_Baseline
Combined_Residential_ACH_Baseline <- Combined_Residential_ACH_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor_ACH_Baseline_Cumulative_Dose <- cbind(Combined_Indoor_ACH_Baseline, Transport_ACH_Baseline)
Indoor_ACH_Baseline_Cumulative_Dose <- Indoor_ACH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor1m_ACH_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_ACH_Baseline, Transport_ACH_Baseline)
Indoor1m_ACH_Baseline_Cumulative_Dose <- Indoor1m_ACH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor_ACH_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor_ACH_Baseline,Combined_Residential_ACH_Baseline, Bus_ACH_Baseline)
Outdoor_ACH_Baseline_Cumulative_Dose <- Outdoor_ACH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_ACH_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_ACH_Baseline,Combined_Residential_ACH_Baseline, Bus_ACH_Baseline)
Outdoor2m_ACH_Baseline_Cumulative_Dose <- Outdoor2m_ACH_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Increased ACH Risk ----------------------------------------------------------

# Risk for Indoor Worker (2m)
Indoor_ACH_Risk = 1-exp(-krisk*Indoor_ACH_Baseline_Cumulative_Dose)
Indoor_ACH_Risk_Vax <- cbind(Indoor_ACH_Risk, mRNAvaccine, reducedVEvaccine)
Indoor_ACH_Risk_Vax_Full <- mutate(Indoor_ACH_Risk_Vax,
                                  mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                  mRNA_Car = mRNAvaccine * CarDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                  RedVE_Car = reducedVEvaccine * CarDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor_ACH_Risk_Quant <- as.data.frame(t(apply(Indoor_ACH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor_ACH_Risk_Mean <- as.data.frame(colMeans(Indoor_ACH_Risk_Vax_Full))
Indoor_ACH_Baseline_Combined_Risk <- cbind(Indoor_ACH_Risk_Quant, Indoor_ACH_Risk_Mean)
write.csv(Indoor_ACH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_ACH.csv", row.names = TRUE)

# Risk for Indoor Worker (1m)
Indoor1m_ACH_Risk = 1-exp(-krisk*Indoor1m_ACH_Baseline_Cumulative_Dose)
Indoor1m_ACH_Risk_Vax <- cbind(Indoor1m_ACH_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_ACH_Risk_Vax_Full <- mutate(Indoor1m_ACH_Risk_Vax,
                                   mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                   mRNA_Car = mRNAvaccine * CarDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                   RedVE_Car = reducedVEvaccine * CarDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_ACH_Risk_Quant <- as.data.frame(t(apply(Indoor1m_ACH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_ACH_Risk_Mean <- as.data.frame(colMeans(Indoor1m_ACH_Risk_Vax_Full))
Indoor1m_ACH_Baseline_Combined_Risk <- cbind(Indoor1m_ACH_Risk_Quant, Indoor1m_ACH_Risk_Mean)
write.csv(Indoor1m_ACH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_ACH.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor_ACH_Risk = 1-exp(-krisk*Outdoor_ACH_Baseline_Cumulative_Dose)
Outdoor_ACH_Risk_Vax <- cbind(Outdoor_ACH_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor_ACH_Risk_Vax_Full <- mutate(Outdoor_ACH_Risk_Vax,
                                   mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                   mRNA_Res = mRNAvaccine * ResDose10hr,
                                   mRNA_Bus = mRNAvaccine * BusDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                   RedVE_Res = reducedVEvaccine * ResDose10hr,
                                   RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor_ACH_Risk_Quant <- as.data.frame(t(apply(Outdoor_ACH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor_ACH_Risk_Mean <- as.data.frame(colMeans(Outdoor_ACH_Risk_Vax_Full))
Outdoor_ACH_Baseline_Combined_Risk <- cbind(Outdoor_ACH_Risk_Quant, Outdoor_ACH_Risk_Mean)
write.csv(Outdoor_ACH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_ACH.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_ACH_Risk = 1-exp(-krisk*Outdoor2m_ACH_Baseline_Cumulative_Dose)
Outdoor2m_ACH_Risk_Vax <- cbind(Outdoor2m_ACH_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_ACH_Risk_Vax_Full <- mutate(Outdoor2m_ACH_Risk_Vax,
                                    mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                    mRNA_Res = mRNAvaccine * ResDose10hr,
                                    mRNA_Bus = mRNAvaccine * BusDose2hr,
                                    mRNA_Total = mRNAvaccine * TotalDose,
                                    
                                    RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                    RedVE_Res = reducedVEvaccine * ResDose10hr,
                                    RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                    RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_ACH_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_ACH_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_ACH_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_ACH_Risk_Vax_Full))
Outdoor2m_ACH_Baseline_Combined_Risk <- cbind(Outdoor2m_ACH_Risk_Quant, Outdoor2m_ACH_Risk_Mean)
write.csv(Outdoor2m_ACH_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_ACH.csv", row.names = TRUE)

# Packaged Interventions Dataset Acquisition -------------------------
Transport_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/CarTransport.csv")
Transport_Packaged_Baseline <- Transport_Packaged %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/Bus.csv")
Bus_Packaged_Baseline <- Bus_Packaged %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_Packaged_Baseline <- Bus_Break_Baseline %>%
  select(af2m1h)

ResCough_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/ResCough.csv")
ResCough_Packaged_Baseline <- ResCough_Packaged %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/ResBreath.csv")
ResBreath_Packaged_Baseline <- ResBreath_Packaged %>%
  select(aero8h)

Indoor_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/Indoor.csv")
Indoor_Packaged_Baseline <- Indoor_Packaged %>%
  select(af2m11h)

Indoor1m_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/Indoor.csv")
Indoor1m_Packaged_Baseline <- Indoor1m_Packaged %>%
  select(af1m11h)

Break_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/Indoor.csv")
Break_Packaged_Baseline <- Break_Packaged %>%
  select(af2m1h)

Outdoor_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/Outdoor.csv")
Outdoor_Packaged_Baseline <- Outdoor_Packaged %>%
  select(af1m11h)

Outdoor2m_Packaged <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Packaged Dose/Outdoor.csv" )
Outdoor2m_Packaged_Baseline <- Outdoor2m_Packaged %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor_Packaged_Baseline <- Outdoor_Packaged_Baseline + Bus_Break_Packaged_Baseline
Combined_Outdoor_Packaged_Baseline <- Combined_Outdoor_Packaged_Baseline %>%
  rename(OutdoorDose12hr = af1m11h)

Combined_Outdoor2m_Packaged_Baseline <- Outdoor2m_Packaged_Baseline + Bus_Break_Packaged_Baseline
Combined_Outdoor2m_Packaged_Baseline <- Combined_Outdoor2m_Packaged_Baseline %>%
  rename(OutdoorDose12hr = af2m11h)

Combined_Indoor_Packaged_Baseline <- Indoor_Packaged_Baseline + Break_Packaged_Baseline
Combined_Indoor_Packaged_Baseline <- Combined_Indoor_Packaged_Baseline %>%
  rename(IndoorDose12hr = af2m11h)

Combined_Indoor1m_Packaged_Baseline <- Indoor1m_Packaged_Baseline + Break_Packaged_Baseline
Combined_Indoor1m_Packaged_Baseline <- Combined_Indoor1m_Packaged_Baseline %>%
  rename(IndoorDose12hr = af1m11h)

Combined_Residential_Packaged_Baseline <- ResBreath_Packaged_Baseline + ResCough_Packaged_Baseline
Combined_Residential_Packaged_Baseline <- Combined_Residential_Packaged_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor_Packaged_Baseline_Cumulative_Dose <- cbind(Combined_Indoor_Packaged_Baseline, Transport_Packaged_Baseline)
Indoor_Packaged_Baseline_Cumulative_Dose <- Indoor_Packaged_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor1m_Packaged_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_Packaged_Baseline, Transport_Packaged_Baseline)
Indoor1m_Packaged_Baseline_Cumulative_Dose <- Indoor1m_Packaged_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor_Packaged_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor_Packaged_Baseline,Combined_Residential_Packaged_Baseline, Bus_Packaged_Baseline)
Outdoor_Packaged_Baseline_Cumulative_Dose <- Outdoor_Packaged_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_Packaged_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_Packaged_Baseline,Combined_Residential_Packaged_Baseline, Bus_Packaged_Baseline)
Outdoor2m_Packaged_Baseline_Cumulative_Dose <- Outdoor2m_Packaged_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Packaged Intervention Risk ----------------------------------------------------------

# Risk for Indoor Worker (2m)
Indoor_Packaged_Risk = 1-exp(-krisk*Indoor_Packaged_Baseline_Cumulative_Dose)
Indoor_Packaged_Risk_Vax <- cbind(Indoor_Packaged_Risk, mRNAvaccine, reducedVEvaccine)
Indoor_Packaged_Risk_Vax_Full <- mutate(Indoor_Packaged_Risk_Vax,
                                   mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                   mRNA_Car = mRNAvaccine * CarDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                   RedVE_Car = reducedVEvaccine * CarDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor_Packaged_Risk_Quant <- as.data.frame(t(apply(Indoor_Packaged_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor_Packaged_Risk_Mean <- as.data.frame(colMeans(Indoor_Packaged_Risk_Vax_Full))
Indoor_Packaged_Baseline_Combined_Risk <- cbind(Indoor_Packaged_Risk_Quant, Indoor_Packaged_Risk_Mean)
write.csv(Indoor_Packaged_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_Packaged.csv", row.names = TRUE)

# Risk for Indoor Worker (1m)
Indoor1m_Packaged_Risk = 1-exp(-krisk*Indoor1m_Packaged_Baseline_Cumulative_Dose)
Indoor1m_Packaged_Risk_Vax <- cbind(Indoor1m_Packaged_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_Packaged_Risk_Vax_Full <- mutate(Indoor1m_Packaged_Risk_Vax,
                                        mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                        mRNA_Car = mRNAvaccine * CarDose2hr,
                                        mRNA_Total = mRNAvaccine * TotalDose,
                                        
                                        RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                        RedVE_Car = reducedVEvaccine * CarDose2hr,
                                        RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_Packaged_Risk_Quant <- as.data.frame(t(apply(Indoor1m_Packaged_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_Packaged_Risk_Mean <- as.data.frame(colMeans(Indoor1m_Packaged_Risk_Vax_Full))
Indoor1m_Packaged_Baseline_Combined_Risk <- cbind(Indoor1m_Packaged_Risk_Quant, Indoor1m_Packaged_Risk_Mean)
write.csv(Indoor1m_Packaged_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_Packaged.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor_Packaged_Risk = 1-exp(-krisk*Outdoor_Packaged_Baseline_Cumulative_Dose)
Outdoor_Packaged_Risk_Vax <- cbind(Outdoor_Packaged_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor_Packaged_Risk_Vax_Full <- mutate(Outdoor_Packaged_Risk_Vax,
                                    mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                    mRNA_Res = mRNAvaccine * ResDose10hr,
                                    mRNA_Bus = mRNAvaccine * BusDose2hr,
                                    mRNA_Total = mRNAvaccine * TotalDose,
                                    
                                    RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                    RedVE_Res = reducedVEvaccine * ResDose10hr,
                                    RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                    RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor_Packaged_Risk_Quant <- as.data.frame(t(apply(Outdoor_Packaged_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor_Packaged_Risk_Mean <- as.data.frame(colMeans(Outdoor_Packaged_Risk_Vax_Full))
Outdoor_Packaged_Baseline_Combined_Risk <- cbind(Outdoor_Packaged_Risk_Quant, Outdoor_Packaged_Risk_Mean)
write.csv(Outdoor_Packaged_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_Packaged.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_Packaged_Risk = 1-exp(-krisk*Outdoor2m_Packaged_Baseline_Cumulative_Dose)
Outdoor2m_Packaged_Risk_Vax <- cbind(Outdoor2m_Packaged_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_Packaged_Risk_Vax_Full <- mutate(Outdoor2m_Packaged_Risk_Vax,
                                         mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                         mRNA_Res = mRNAvaccine * ResDose10hr,
                                         mRNA_Bus = mRNAvaccine * BusDose2hr,
                                         mRNA_Total = mRNAvaccine * TotalDose,
                                         
                                         RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                         RedVE_Res = reducedVEvaccine * ResDose10hr,
                                         RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                         RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_Packaged_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_Packaged_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_Packaged_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_Packaged_Risk_Vax_Full))
Outdoor2m_Packaged_Baseline_Combined_Risk <- cbind(Outdoor2m_Packaged_Risk_Quant, Outdoor2m_Packaged_Risk_Mean)
write.csv(Outdoor2m_Packaged_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_Packaged.csv", row.names = TRUE)

# KN95 Mask Dataset Acquisition -------------------------
Transport_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/CarTransport.csv")
Transport_KN_Baseline <- Transport_KN %>%
  select(aero1h) %>% 
  mutate(aero1h = aero1h*2) %>%
  rename(CarDose2hr = aero1h) 

Bus_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/Bus.csv")
Bus_KN_Baseline <- Bus_KN %>%
  select(aero1h) %>%
  mutate(aero1h = aero1h*2) %>%
  rename(BusDose2hr = aero1h)
Bus_Break_KN_Baseline <- Bus_Break_Baseline %>%
  select(af2m1h)

ResCough_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/ResCough.csv")
ResCough_KN_Baseline <- ResCough_KN %>%
  select(af2m1h) %>%
  mutate(af2m1h = af2m1h*2) %>%
  rename(af2m2h = af2m1h)

ResBreath_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/ResBreath.csv")
ResBreath_KN_Baseline <- ResBreath_KN %>%
  select(aero8h)

Indoor_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/Indoor.csv")
Indoor_KN_Baseline <- Indoor_KN %>%
  select(af2m11h)

Indoor1m_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/Indoor.csv")
Indoor1m_KN_Baseline <- Indoor1m_KN %>%
  select(af1m11h)

Break_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/Indoor.csv")
Break_KN_Baseline <- Break_KN %>%
  select(af2m1h)

Outdoor_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/Outdoor.csv")
Outdoor_KN_Baseline <- Outdoor_KN %>%
  select(af1m11h)

Outdoor2m_KN <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/N95 Mask Dose/Outdoor.csv")
Outdoor2m_KN_Baseline <- Outdoor2m_KN %>%
  select(af2m11h)

# Combine doses together for each module

Combined_Outdoor_KN_Baseline <- Outdoor_KN_Baseline + Bus_Break_KN_Baseline
Combined_Outdoor_KN_Baseline <- Combined_Outdoor_KN_Baseline %>%
  rename(OutdoorDose12hr = af1m11h)

Combined_Outdoor2m_KN_Baseline <- Outdoor2m_KN_Baseline + Bus_Break_KN_Baseline
Combined_Outdoor2m_KN_Baseline <- Combined_Outdoor2m_KN_Baseline %>%
  rename(OutdoorDose12hr = af2m11h)

Combined_Indoor_KN_Baseline <- Indoor_KN_Baseline + Break_KN_Baseline
Combined_Indoor_KN_Baseline <- Combined_Indoor_KN_Baseline %>%
  rename(IndoorDose12hr = af2m11h)

Combined_Indoor1m_KN_Baseline <- Indoor1m_KN_Baseline + Break_KN_Baseline
Combined_Indoor1m_KN_Baseline <- Combined_Indoor1m_KN_Baseline %>%
  rename(IndoorDose12hr = af1m11h)

Combined_Residential_KN_Baseline <- ResBreath_KN_Baseline + ResCough_KN_Baseline
Combined_Residential_KN_Baseline <- Combined_Residential_KN_Baseline %>% 
  rename(ResDose10hr = aero8h)

# Combine modules together for cumulative dose
Indoor_KN_Baseline_Cumulative_Dose <- cbind(Combined_Indoor_KN_Baseline, Transport_KN_Baseline)
Indoor_KN_Baseline_Cumulative_Dose <- Indoor_KN_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Indoor1m_KN_Baseline_Cumulative_Dose <- cbind(Combined_Indoor1m_KN_Baseline, Transport_KN_Baseline)
Indoor1m_KN_Baseline_Cumulative_Dose <- Indoor1m_KN_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor_KN_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor_KN_Baseline,Combined_Residential_KN_Baseline, Bus_KN_Baseline)
Outdoor_KN_Baseline_Cumulative_Dose <- Outdoor_KN_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

Outdoor2m_KN_Baseline_Cumulative_Dose <- cbind(Combined_Outdoor2m_KN_Baseline,Combined_Residential_KN_Baseline, Bus_KN_Baseline)
Outdoor2m_KN_Baseline_Cumulative_Dose <- Outdoor2m_KN_Baseline_Cumulative_Dose %>%
  mutate(TotalDose = rowSums(.))

# Calculate Double Mask Risk ----------------------------------------------------------

# Risk for Indoor Worker (2m)
Indoor_KN_Risk = 1-exp(-krisk*Indoor_KN_Baseline_Cumulative_Dose)
Indoor_KN_Risk_Vax <- cbind(Indoor_KN_Risk, mRNAvaccine, reducedVEvaccine)
Indoor_KN_Risk_Vax_Full <- mutate(Indoor_KN_Risk_Vax,
                                  mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                  mRNA_Car = mRNAvaccine * CarDose2hr,
                                  mRNA_Total = mRNAvaccine * TotalDose,
                                  
                                  RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                  RedVE_Car = reducedVEvaccine * CarDose2hr,
                                  RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor_KN_Risk_Quant <- as.data.frame(t(apply(Indoor_KN_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor_KN_Risk_Mean <- as.data.frame(colMeans(Indoor_KN_Risk_Vax_Full))
Indoor_KN_Baseline_Combined_Risk <- cbind(Indoor_KN_Risk_Quant, Indoor_KN_Risk_Mean)
write.csv(Indoor_KN_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_KNMask.csv", row.names = TRUE)

# Risk for Indoor Worker (1m)
Indoor1m_KN_Risk = 1-exp(-krisk*Indoor1m_KN_Baseline_Cumulative_Dose)
Indoor1m_KN_Risk_Vax <- cbind(Indoor1m_KN_Risk, mRNAvaccine, reducedVEvaccine)
Indoor1m_KN_Risk_Vax_Full <- mutate(Indoor1m_KN_Risk_Vax,
                                    mRNA_Indoor = mRNAvaccine * IndoorDose12hr,
                                    mRNA_Car = mRNAvaccine * CarDose2hr,
                                    mRNA_Total = mRNAvaccine * TotalDose,
                                    
                                    RedVE_Indoor = reducedVEvaccine * IndoorDose12hr,
                                    RedVE_Car = reducedVEvaccine * CarDose2hr,
                                    RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(IndoorRisk12hr = IndoorDose12hr,
         CarRisk2hr = CarDose2hr,
         TotalRisk = TotalDose)

Indoor1m_KN_Risk_Quant <- as.data.frame(t(apply(Indoor1m_KN_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Indoor1m_KN_Risk_Mean <- as.data.frame(colMeans(Indoor1m_KN_Risk_Vax_Full))
Indoor1m_KN_Baseline_Combined_Risk <- cbind(Indoor1m_KN_Risk_Quant, Indoor1m_KN_Risk_Mean)
write.csv(Indoor1m_KN_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_KNMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (1m)
Outdoor_KN_Risk = 1-exp(-krisk*Outdoor_KN_Baseline_Cumulative_Dose)
Outdoor_KN_Risk_Vax <- cbind(Outdoor_KN_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor_KN_Risk_Vax_Full <- mutate(Outdoor_KN_Risk_Vax,
                                   mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                   mRNA_Res = mRNAvaccine * ResDose10hr,
                                   mRNA_Bus = mRNAvaccine * BusDose2hr,
                                   mRNA_Total = mRNAvaccine * TotalDose,
                                   
                                   RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                   RedVE_Res = reducedVEvaccine * ResDose10hr,
                                   RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                   RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor_KN_Risk_Quant <- as.data.frame(t(apply(Outdoor_KN_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor_KN_Risk_Mean <- as.data.frame(colMeans(Outdoor_KN_Risk_Vax_Full))
Outdoor_KN_Baseline_Combined_Risk <- cbind(Outdoor_KN_Risk_Quant, Outdoor_KN_Risk_Mean)
write.csv(Outdoor_KN_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_KNMask.csv", row.names = TRUE)

#Risk for Outdoor Worker (2m)
Outdoor2m_KN_Risk = 1-exp(-krisk*Outdoor2m_KN_Baseline_Cumulative_Dose)
Outdoor2m_KN_Risk_Vax <- cbind(Outdoor2m_KN_Risk, mRNAvaccine, reducedVEvaccine)
Outdoor2m_KN_Risk_Vax_Full <- mutate(Outdoor2m_KN_Risk_Vax,
                                     mRNA_Outdoor = mRNAvaccine * OutdoorDose12hr,
                                     mRNA_Res = mRNAvaccine * ResDose10hr,
                                     mRNA_Bus = mRNAvaccine * BusDose2hr,
                                     mRNA_Total = mRNAvaccine * TotalDose,
                                     
                                     RedVE_Outdoor = reducedVEvaccine * OutdoorDose12hr,
                                     RedVE_Res = reducedVEvaccine * ResDose10hr,
                                     RedVE_Bus = reducedVEvaccine * BusDose2hr,
                                     RedVE_Total = reducedVEvaccine * TotalDose) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(OutdoorRisk12hr = OutdoorDose12hr,
         ResRisk10hr = ResDose10hr,
         BusRisk2hr = BusDose2hr,
         TotalRisk = TotalDose)

Outdoor2m_KN_Risk_Quant <- as.data.frame(t(apply(Outdoor2m_KN_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Outdoor2m_KN_Risk_Mean <- as.data.frame(colMeans(Outdoor2m_KN_Risk_Vax_Full))
Outdoor2m_KN_Baseline_Combined_Risk <- cbind(Outdoor2m_KN_Risk_Quant, Outdoor2m_KN_Risk_Mean)
write.csv(Outdoor2m_KN_Baseline_Combined_Risk, "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_KNMask.csv", row.names = TRUE)

# Risk for the baseline individual Break Modules ---------
Risk_Bus_Break = 1-exp(-krisk*Bus_Break_Baseline)
Risk_Bus_Break_Vax <- cbind(Risk_Bus_Break, mRNAvaccine, reducedVEvaccine)
Bus_Break_Risk_Vax_Full <- mutate(Risk_Bus_Break_Vax,
                                  mRNA_BusBreak = mRNAvaccine * af2m1h,
                                  
                                  RedVE_BusBreak = reducedVEvaccine * af2m1h) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(BaselineBusBreak = af2m1h)
Risk_Bus_Quant <- as.data.frame(t(apply(Bus_Break_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Risk_Bus_Mean <- as.data.frame(colMeans(Bus_Break_Risk_Vax_Full))
Risk_Bus_Combined_Risk <- cbind(Risk_Bus_Quant, Risk_Bus_Mean)

Risk_Indoor_Break = 1-exp(-krisk*Break_Baseline)
Risk_Indoor_Break_Vax <- cbind(Risk_Indoor_Break, mRNAvaccine, reducedVEvaccine)
Indoor_Break_Risk_Vax_Full <- mutate(Risk_Indoor_Break_Vax,
                                     mRNA_IndoorBreak = mRNAvaccine * af2m1h,
                                     
                                     RedVE_IndoorBreak = reducedVEvaccine * af2m1h) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(BaselineIndoorBreak = af2m1h)
Risk_Indoor_Break_Quant <- as.data.frame(t(apply(Indoor_Break_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Risk_Indoor_Break_Mean <- as.data.frame(colMeans(Indoor_Break_Risk_Vax_Full))
Risk_Indoor_Break_Combined_Risk <- cbind(Risk_Indoor_Break_Quant, Risk_Indoor_Break_Mean)

Risk_Indoor_ACH_Break = 1-exp(-krisk*Break_ACH_Baseline)
Risk_Indoor_ACH_Break_Vax <- cbind(Risk_Indoor_ACH_Break, mRNAvaccine, reducedVEvaccine)
Indoor_ACH_Break_Risk_Vax_Full <- mutate(Risk_Indoor_ACH_Break_Vax,
                                         mRNA_IndoorBreak = mRNAvaccine * af2m1h,
                                         
                                         RedVE_IndoorBreak = reducedVEvaccine * af2m1h) %>%
  select(!c(mRNAvaccine, reducedVEvaccine)) %>%
  rename(BaselineIndoorBreak = af2m1h) 
Risk_Indoor_ACH_Break_Quant <- as.data.frame(t(apply(Indoor_ACH_Break_Risk_Vax_Full, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))))
Risk_Indoor_ACH_Break_Mean <- as.data.frame(colMeans(Indoor_ACH_Break_Risk_Vax_Full))
Risk_Indoor_ACH_Break_Combined_Risk <- cbind(Risk_Indoor_ACH_Break_Quant, Risk_Indoor_ACH_Break_Mean)


Indoor2m_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor2m_Baseline.csv")
Indoor_HH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_HH.csv")
Indoor_SM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_SMask.csv")
Indoor_CM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_CMask.csv")
Indoor_DM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_DMask.csv")
Indoor_ACH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_ACH.csv")
Indoor_Packaged_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_Packaged.csv")
Indoor_KN_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor_KNMask.csv")
#Risk_Indoor_Break_Combined_Risk <- fread("")

Indoor1m_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_Baseline.csv")
Indoor1m_HH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_HH.csv")
Indoor1m_SM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_SMask.csv")
Indoor1m_CM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_CMask.csv")
Indoor1m_DM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_DMask.csv")
Indoor1m_ACH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_ACH.csv")
Indoor1m_Packaged_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_Packaged.csv")
Indoor1m_KN_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Indoor1m_KNMask.csv")

Outdoor1m_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor1m_Baseline.csv")
Outdoor_HH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_HH.csv")
Outdoor_SM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_SMask.csv")
Outdoor_CM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_CMask.csv")
Outdoor_DM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_DMask.csv")
Outdoor_ACH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_ACH.csv")
Outdoor_Packaged_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_Packaged.csv")
Outdoor_KN_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor_KNMask.csv")
#Risk_Bus_Combined_Risk <- fread("")

Outdoor2m_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_Baseline.csv")
Outdoor2m_HH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_HH.csv")
Outdoor2m_SM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_SMask.csv")
Outdoor2m_CM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_CMask.csv")
Outdoor2m_DM_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_DMask.csv")
Outdoor2m_ACH_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_ACH.csv")
Outdoor2m_Packaged_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_Packaged.csv")
Outdoor2m_KN_Baseline_Combined_Risk <- fread("/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Outdoor2m_KNMask.csv")

# Cumulative Dataset Creation ------------------------------
Manuscript_Cumulative_Data <- list("Indoor 2m Baseline" = Indoor2m_Baseline_Combined_Risk,
                                   "Indoor 2m HH" = Indoor_HH_Baseline_Combined_Risk,
                                   "Indoor 2m SMask" = Indoor_SM_Baseline_Combined_Risk,
                                   "Indoor 2m CMask" = Indoor_CM_Baseline_Combined_Risk,
                                   "Indoor 2m DMask" = Indoor_DM_Baseline_Combined_Risk,
                                   "Indoor 2m ACH" = Indoor_ACH_Baseline_Combined_Risk,
                                   "Indoor 2m Packaged" = Indoor_Packaged_Baseline_Combined_Risk,
                                   "Indoor 2m KN95" = Indoor_KN_Baseline_Combined_Risk,
                                   "Indoor Break Baseline" = Risk_Indoor_Break_Combined_Risk,
                                   
                                   "Indoor 1m Baseline" = Indoor1m_Baseline_Combined_Risk,
                                   "Indoor 1m HH" = Indoor1m_HH_Baseline_Combined_Risk,
                                   "Indoor 1m SMask" = Indoor1m_SM_Baseline_Combined_Risk,
                                   "Indoor 1m CMask" = Indoor1m_CM_Baseline_Combined_Risk,
                                   "Indoor 1m DMask" = Indoor1m_DM_Baseline_Combined_Risk,
                                   "Indoor 1m ACH" = Indoor1m_ACH_Baseline_Combined_Risk,
                                   "Indoor 1m Packaged" = Indoor1m_Packaged_Baseline_Combined_Risk,
                                   "Indoor 1m KN95" = Indoor1m_KN_Baseline_Combined_Risk,

                                   "Outdoor 1m Baseline" = Outdoor1m_Baseline_Combined_Risk,
                                   "Outdoor 1m HH" = Outdoor_HH_Baseline_Combined_Risk,
                                   "Outdoor 1m SMask" = Outdoor_SM_Baseline_Combined_Risk,
                                   "Outdoor 1m CMask" = Outdoor_CM_Baseline_Combined_Risk,
                                   "Outdoor 1m DMask" = Outdoor_DM_Baseline_Combined_Risk,
                                   "Outdoor 1m ACH" = Outdoor_ACH_Baseline_Combined_Risk,
                                   "Outdoor 1m Packaged" = Outdoor_Packaged_Baseline_Combined_Risk,
                                   "Outdoor 1m KN95" = Outdoor_KN_Baseline_Combined_Risk,
                                   "Outdoor 1m Break Baseline" = Risk_Bus_Combined_Risk,

                                   "Outdoor 2m Baseline" = Outdoor2m_Baseline_Combined_Risk,
                                   "Outdoor 2m HH" = Outdoor2m_HH_Baseline_Combined_Risk,
                                   "Outdoor 2m SMask" = Outdoor2m_SM_Baseline_Combined_Risk,
                                   "Outdoor 2m CMask" = Outdoor2m_CM_Baseline_Combined_Risk,
                                   "Outdoor 2m DMask" = Outdoor2m_DM_Baseline_Combined_Risk,
                                   "Outdoor 2m ACH" = Outdoor2m_ACH_Baseline_Combined_Risk,
                                   "Outdoor 2m Packaged" = Outdoor2m_Packaged_Baseline_Combined_Risk,
                                   "Outdoor 2m KN95" = Outdoor2m_KN_Baseline_Combined_Risk)

write.xlsx(Manuscript_Cumulative_Data, 
           file = "/Users/kane/Desktop/Leon Lab/Thesis/Manuscript/Code Files/Risk Estimates/Manuscript Cumulative Data.xlsx", 
           row.names = TRUE,
           overwrite = TRUE)
