Quantitative Microbial Risk Assessment Models to Quantify SARS-CoV-2 Infection Risk Among Indoor & Outdoor Food Workers

D. Kane Cooper and Collaborators
June 2, 2023
Rollins School of Public Health, Emory University 
Atlanta, GA 30322

Code developed in R.

Copyright: These materials are distributed under the GPL-3 license with the following copyright and attribution requirements listed in the License document. If utilizing this code for academic or research purposes, please cite our manuscript below. 

Manuscript Citation: Cooper DK, Sobolik JS, Kovacevic J, Rock CM, Sajewski ET, Guest JL, Lopman BA, Jaykus LA, Leon JS. Combined Infection Control Interventions Protect Essential Food Workers from Occupational Exposures to SARS-CoV-2 in the Agricultural Environment. Appl Environ Microbiol. 2023 Jun 13:e0012823. doi: 10.1128/aem.00128-23. Epub ahead of print. PMID: 37310232.

Project Information: The code provided in this repository may be used to estimate SARS-CoV-2 viral transmission risk across aerosol, droplet, and fomite-mediated pathways within environmental scenarios pertinent to an indoor and outdoor food worker. The scenarios assessed include: car and bus transportation, indoor food packaging facility and breakroom, outdoor harvesting / packaging operation, and an employer-provided shared housing space. Finally, we also assessed the impact of infection control interventions (handwashing, surface disinfection, masking, physical distancing, increased ventilation) and vaccinations on reducing the risk of SARS-CoV-2 infection for a worker. For more information, please refer to and cite this publication: Cooper DK, Sobolik JS, Kovacevic J, Rock CM, Sajewski ET, Guest JL, Lopman BA, Jaykus LA, Leon JS. Combined Infection Control Interventions Protect Essential Food Workers from Occupational Exposures to SARS-CoV-2 in the Agricultural Environment. Appl Environ Microbiol. 2023 Jun 13:e0012823. doi: 10.1128/aem.00128-23. Epub ahead of print. PMID: 37310232.

Using the Models: In the repository you will find three numbered folders. The first folder (1. Scenario Functions) contains the R functions parameterized for each viral transmission pathway across all environmental scenarios (termed “modules” in the code) assessed, without any infection control interventions applied. This code should be saved such that the pathname for the function files can be called from the files in the next folder. The second folder (2. Viral Dose Estimates) utilizes the functions from the prior folder to quantify the combined viral dose for the specified intervention being assessed. For example, in the “DoseCalc_20210903_Baseline.R” file, there are no interventions active, so the output of running this code will be .csv files for each of the environmental scenarios that contain the baseline viral dose across each transmission pathway. Finally, the third folder (3. Infection Risk Estimates) utilizes the .csv output from the “DoseCalc” files from the second folder to quantify the risk of infection for each individual environmental scenario, as well as the combined environmental scenarios pertinent to either an indoor or an outdoor food worker (as specified in our manuscript). The code used for our sensitivity analyses are also included in this final folder. 

Note: The set.seed() function was used such that our output remained consistent throughout our data analysis; however, removing this from the code would provide additional variability in the output. 
