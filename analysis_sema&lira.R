setwd("~/GLPs")

library(readxl)
###here extract data from sheet "Drugs"
VigiLyze1 <- read_excel("VigiLyze line listing_GLP1 RA_HLGT-suicidal self-injurious.xlsx", sheet = "Drugs")

names(VigiLyze1)[1]<-paste("PrimaryId")
names(VigiLyze1)[2]<-paste("SafetyId")

#create the array for semaglutide
VigiLyze1$temp.s<-array(0,length(VigiLyze1$`WHODrug active ingredient variant`))
VigiLyze1$temp.s[VigiLyze1$`WHODrug active ingredient variant`=="Semaglutide"]<-1


VigiLyze1.1<-transform(VigiLyze1, totaltemps= ave(VigiLyze1$temp.s, 
                                                    VigiLyze1$PrimaryId, FUN=sum))[-2]
table(VigiLyze1.1$totaltemps)

##here identify only cases medicated with Semaglutide
VigiLyze1.1.s<-VigiLyze1.1[VigiLyze1.1$totaltemps>0,]
length(VigiLyze1.1.s$PrimaryId)

##now ensure information on indication before removing duplicates 

library(tidyverse)
VigiLyze1.2.s<- VigiLyze1.1.s %>% 
  group_by(PrimaryId,WHODrug.active.ingredient.variant) %>% 
  fill(Indication) 

View(VigiLyze1.2.s)   ###works

##Remove duplicates
ncol(VigiLyze1.2.s)
require(dplyr)
VigiLyze1.2.s$temp2<-apply(VigiLyze1.2.s[1:17], 1, function(x) paste(sort(x), collapse = "_"))

VigiLyze1.3.s<-distinct(VigiLyze1.2.s,VigiLyze1.2.s$temp2,.keep_all = TRUE)
length(VigiLyze1.3.s$PrimaryId)
length(unique(VigiLyze1.3.s$PrimaryId))

View(VigiLyze1.3.s)

###assess co-medication
table(VigiLyze1.3.s$WHODrug.active.ingredient.variant)
medications.s <- sort(table(VigiLyze1.3.s$WHODrug.active.ingredient.variant),decreasing=T)

###Co-medication with antidiabetic agents
VigiLyze1.3.s$antidiabetic<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Metformin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Insulin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Empagliflozin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Dapagliflozin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Glibenclamide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Gliclazide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Glipizide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Sitagliptin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Pioglitazone", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Voglibose", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Acarbose", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Canagliflozin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}
  if(length(i <- grep("Repaglinide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidiabetic[i]<-1}}

##Comedication with PPIs
VigiLyze1.3.s$ppi<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Omeprazole", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$ppi[i]<-1}
  if(length(i <- grep("Lansoprazole", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$ppi[i]<-1}
  if(length(i <- grep("Esomeprazole", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$ppi[i]<-1}
  if(length(i <- grep("Pantoprazole", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$ppi[i]<-1}}
 
###Co-medication with statins/lipid lowering agents
VigiLyze1.3.s$statin<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Simvastatin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$statin[i]<-1}
  if(length(i <- grep("Atorvastatin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$statin[i]<-1}
  if(length(i <- grep("Rosuvastatin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$statin[i]<-1}
  if(length(i <- grep("Pravastatin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$statin[i]<-1}
  if(length(i <- grep("Fenofibrate", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$statin[i]<-1}}

###Co-medication with painkilers
VigiLyze1.3.s$analg<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Paracetamol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Acetylsalicylic acid", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Tramadol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Carisoprodol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Celecoxib", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Hydrocodon", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Diclofenac", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Meloxicam", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Naproxen", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}
  if(length(i <- grep("Cyclobenzaprine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$analg[i]<-1}}

###Co-medication with antidepressants
VigiLyze1.3.s$antidepressant<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("italopram", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Amitriptyline", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Mirtazapine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Fluvoxamine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Bupropion", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Sertraline", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Buspirone", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Fluoxetine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("enlafaxine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Duloxetine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Clomipramine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Vortioxetine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}
  if(length(i <- grep("Trazodone", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antidepressant[i]<-1}}
  
###Co-medication with antipsychotics
VigiLyze1.3.s$antipsychotic<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Cariprazine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Haloperidol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Promethazine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Quetiapine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Risperidone", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Aripiprazole", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Olanzapine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Pipamperone", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Sulpiride", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}
  if(length(i <- grep("Ziprasidone", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antipsychotic[i]<-1}}

###Co-medication with benzodiapines
VigiLyze1.3.s$benzos<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Alprazolam", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}
  if(length(i <- grep("azepam", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}
  if(length(i <- grep("Mexazolam", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}
  if(length(i <- grep("Etizolam", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}
  if(length(i <- grep("Triazolam", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}
  if(length(i <- grep("Gabapentin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}
  if(length(i <- grep("Pregabalin", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}
  if(length(i <- grep("Brotizolam", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$benzos[i]<-1}}

###Co-medication with antiseizure medications
VigiLyze1.3.s$anticonvulsant<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Lamotrigine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$anticonvulsant[i]<-1}
  if(length(i <- grep("Topiramate", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$anticonvulsant[i]<-1}
  if(length(i <- grep("Valproate", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$anticonvulsant[i]<-1}}

###Co-medication with antihypertensive agents
VigiLyze1.3.s$antihypertensive<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Lisinopril", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Amlodipine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("lapril", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Atenolol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Bisoprolol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Propanolol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Metoprolol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Candesartan", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Hydrochlorothiazide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Lercanidipine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Furosemide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("sartan", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Ramipril", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Torasemide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}
  if(length(i <- grep("Spironolactone", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$antihypertensive[i]<-1}}

###Co-medication with antiasthmatics
VigiLyze1.3.s$asthma<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("uticasone ", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$asthma[i]<-1}
  if(length(i <- grep("Salmeterol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$asthma[i]<-1}
  if(length(i <- grep("Salbutamol", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$asthma[i]<-1}
  if(length(i <- grep("Desloratadine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$asthma[i]<-1}
  if(length(i <- grep("Budesonide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$asthma[i]<-1}
  if(length(i <- grep("Loratadine", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$asthma[i]<-1}
  if(length(i <- grep("Montelukast", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$asthma[i]<-1}}

###Co-medication with other GLP-1 RAs
VigiLyze1.3.s$glps<-array(0,length(VigiLyze1.3.s$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.s$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Exenatide ", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$glps[i]<-1}
  if(length(i <- grep("Liraglutide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$glps[i]<-1}
  if(length(i <- grep("Tirzepatide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$glps[i]<-1}
  if(length(i <- grep("Dulaglutide", VigiLyze1.3.s$WHODrug.active.ingredient.variant))){VigiLyze1.3.s$glps[i]<-1}}

VigiLyze1.4.s<-transform(VigiLyze1.3.s, anticonv= ave(VigiLyze1.3.s$anticonvulsant, 
                                                      VigiLyze1.3.s$PrimaryId, FUN=sum),
                      antihypertensives= ave(VigiLyze1.3.s$antihypertensive, 
                                             VigiLyze1.3.s$PrimaryId, FUN=sum), 
                      benzodiazepines= ave(VigiLyze1.3.s$benzos, 
                                   VigiLyze1.3.s$PrimaryId, FUN=sum),
                      benzodiazepines= ave(VigiLyze1.3.s$benzos, 
                                           VigiLyze1.3.s$PrimaryId, FUN=sum),
                      antipsych= ave(VigiLyze1.3.s$antipsychotic, 
                                           VigiLyze1.3.s$PrimaryId, FUN=sum),
                      antidep= ave(VigiLyze1.3.s$antidepressant, 
                                     VigiLyze1.3.s$PrimaryId, FUN=sum),
                      antidiabetics= ave(VigiLyze1.3.s$antidiabetic, 
                                   VigiLyze1.3.s$PrimaryId, FUN=sum),
                      asthm= ave(VigiLyze1.3.s$asthma, 
                                  VigiLyze1.3.s$PrimaryId, FUN=sum), 
                      analgetics= ave(VigiLyze1.3.s$analg, 
                                 VigiLyze1.3.s$PrimaryId, FUN=sum),
                      ppis= ave(VigiLyze1.3.s$ppi, 
                                 VigiLyze1.3.s$PrimaryId, FUN=sum),
                      statins= ave(VigiLyze1.3.s$statin, 
                                 VigiLyze1.3.s$PrimaryId, FUN=sum),
                       glp= ave(VigiLyze1.3.s$glps, 
                                VigiLyze1.3.s$PrimaryId, FUN=sum))[-2]

###Assess indication
VigiLyze1.5.s<-VigiLyze1.4.s[VigiLyze1.4.s$WHODrug.active.ingredient.variant=="Semaglutide",]
length(VigiLyze1.5.s$PrimaryId)
length(unique(VigiLyze1.5.s$PrimaryId))
table(VigiLyze1.5.s$WHODrug.active.ingredient.variant)
##create a new file
library(tidyverse)
VigiLyze1.6.s<-distinct(VigiLyze1.5.s,VigiLyze1.5.s$PrimaryId,.keep_all = TRUE)

indications.s <- sort(table(VigiLyze1.6.s$Indication),decreasing=T)

VigiLyze1.6.s$ind<-array(,length(VigiLyze1.6.s$Indication))
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Weight control"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Obesity"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Overweight"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Weight decreased"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Weight loss"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Adipositas"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Inability to lose weight"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Metabolic syndrome"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Morbid obesity"]<-"Weight"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Body mass index high"]<-"Weight"

VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Product used for unknown indication"]<-"off-label"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Drug use for unknown indication"]<-"off-label"

VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Type 2 diabetes mellitus"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Diabetes mellitus"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Type 2 diabetes mellitus"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Diabetes"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Type II diabetes mellitus"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Type II diabetes mellitus inadequate control"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Blood glucose increased"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Glucose tolerance impaired"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Glycosylated haemoglobin abnormal"]<-"diabetes"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Blood glucose increased"]<-"diabetes"

VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Polycystic ovaries"]<-"PCOS"
VigiLyze1.6.s$ind[VigiLyze1.6.s$Indication=="Polycystic ovarian syndrome"]<-"PCOS"

table(VigiLyze1.6.s$ind)

###here extract comedications
table(VigiLyze1.6.s$antidiabetics)
table(VigiLyze1.6.s$antidep)
table(VigiLyze1.6.s$benzodiazepines)
table(VigiLyze1.6.s$ppis)
table(VigiLyze1.6.s$statins)
table(VigiLyze1.6.s$analgetics)
table(VigiLyze1.6.s$antipsych)
table(VigiLyze1.6.s$anticonv)
table(VigiLyze1.6.s$antihypertensives)
table(VigiLyze1.6.s$asthm)
table(VigiLyze1.6.s$glp)

###overlapping medications
table(VigiLyze1.6.s$antidep[VigiLyze1.6.s$antidiabetics>0])
table(VigiLyze1.6.s$antidiabetics[VigiLyze1.6.s$antidep>0])

##Extract dose data
by(VigiLyze1.6.s$Dose, VigiLyze1.6.s$Dose.unit ,summary)
VigiLyze1.6.s$Dose[VigiLyze1.6.s$Dose==0.05]<-0.5
VigiLyze1.6.s$Dose[VigiLyze1.6.s$Dose==25 & VigiLyze1.6.s$Dose.unit=="mg"]<-NA

table(VigiLyze1.6.s$Dosage.regimen)
table(VigiLyze1.6.s$Action.taken.with.drug)

##########################
##########################
####reaction sheet########
##########################
library(readxl)
VigiLyze2 <- read_excel("VigiLyze line listing_GLP1 RA_HLGT-suicidal self-injurious.xlsx", 
                        sheet = "Reactions")
names(VigiLyze2)[1]<-paste("PrimaryId")
names(VigiLyze2)[2]<-paste("SafetyId")
names(VigiLyze2)[4]<-paste("ADR")
names(VigiLyze2)[5]<-paste("MedDRA")
names(VigiLyze2)[6]<-paste("ADR_start")
names(VigiLyze2)[7]<-paste("ADR_end")


VigiLyze2$ADR<-NULL
ids <- unique(VigiLyze1.6.s$PrimaryId) #here we select the unique IDs

VigiLyze2$temps<-array(0,length(VigiLyze2$PrimaryId))
for(i in 1:length(VigiLyze2$PrimaryId)){
  for(j in 1:length(ids)){
    if(VigiLyze2$PrimaryId[i]==ids[j]){VigiLyze2$temps[i]<-1}}}
table(VigiLyze2$temps)

VigiLyze2.1.s<-VigiLyze2[VigiLyze2$temps==1,]
length(VigiLyze2.1.s$PrimaryId)

###merge reactions and drugs data frames 
reac_drug.s<-merge(VigiLyze2.1.s,VigiLyze1.6.s,by.x = "PrimaryId", by.y = "PrimaryId")
length(reac_drug.s$PrimaryId)
View(reac_drug.s)


###Adverse drug reactions by Indication
table(reac_drug.s$MedDRA, reac_drug.s$ind)

####suicidal ideation
si.s<-reac_drug.s[reac_drug.s$MedDRA=="Suicidal ideation",]
length(si.s$PrimaryId)
length(unique(si.s$PrimaryId))
table(si.s$antidep)
table(si.s$antidiabetics)
table(si.s$antidep[si.s$antidiabetics>0])

si.s1<-subset(si.s,!is.na(si.s$ADR_start))
si.s2<-subset(si.s1,!is.na(si.s1$Start.date))
require("anytime")
si.s2$ADRstart<-anytime(si.s2$ADR_start)
si.s2$ADRstartdate<-as.Date(si.s2$ADRstart, format = "%Y-%m-%d")

si.s2$treatmentstart1<-anytime(si.s2$Start.date)
si.s2$treatmentstart<-as.Date(si.s2$treatmentstart1, format = "%Y-%m-%d")

si.s2$ADRduration<- si.s2$ADRstartdate-si.s2$treatmentstart
si.s2$ADRduration[si.s2$ADRduration<0]<-NA

si.s2$ADRduration2<-as.numeric(si.s2$ADRduration)
summary(si.s2$ADRduration2)



######################
#####################
######Cases##########
####################
library(readxl)
VigiLyze3 <- read_excel("VigiLyze line listing_GLP1 RA_HLGT-suicidal self-injurious.xlsx", 
                        sheet = "Cases")
colnames(VigiLyze3)
names(VigiLyze3)[1]<-paste("PrimaryId")
names(VigiLyze3)[3]<-paste("SafetyId")

VigiLyze3$temps<-array(0,length(VigiLyze3$PrimaryId))
for(i in 1:length(VigiLyze3$PrimaryId)){
  for(j in 1:length(ids)){
    if(VigiLyze3$PrimaryId[i]==ids[j]){VigiLyze3$temps[i]<-1}}}
table(VigiLyze3$temps)

VigiLyze3.1.s<-VigiLyze3[VigiLyze3$temps==1,]
length(VigiLyze3.1.s$PrimaryId)

###merge reactions/drugs and cases data frames 
reac_drug_case.s<-merge(reac_drug.s,VigiLyze3.1.s,by.x = c("PrimaryId","SafetyId"), by.y = c("PrimaryId","SafetyId"))
length(reac_drug_case.s$PrimaryId)
View(reac_drug_case.s)

##Extract demographic information 
####Age (all in years)
table(reac_drug_case.s$`Age unit`)
reac_drug_case.s1<-distinct(reac_drug_case.s,reac_drug_case.s$PrimaryId,.keep_all = TRUE)
summary(reac_drug_case.s1$Age,na.rm =T)
###Sex
table(reac_drug_case.s1$Sex, useNA = "always")
table(reac_drug_case.s1$Sex[reac_drug_case.s1$MedDRA=="Suicidal ideation"])

###Body weight 
summary(reac_drug_case.s1$`Weight (kg)`)
###Height
summary(reac_drug_case.s1$`Height (cm)`)

reac_drug_case.s1$BMI<-reac_drug_case.s1$`Weight (kg)`/((reac_drug_case.s1$`Height (cm)`/100)^2)
summary(reac_drug_case.s1$BMI)

####Extract information on time between onset of suicidal ideation and start of treatment with semaglutide
si.s<-reac_drug.s[reac_drug.s$MedDRA=="Suicidal ideation",]
length(si.s$PrimaryId)
length(unique(si.s$PrimaryId))
table(si.s$antidep)
table(si.s$antidiabetics)
table(si.s$antidep[si.s$antidiabetics>0])

si.s1<-subset(si.s,!is.na(si.s$ADR_start))
si.s2<-subset(si.s1,!is.na(si.s1$Start.date))
require("anytime")
si.s2$ADRstart<-anytime(si.s2$ADR_start)
si.s2$ADRstartdate<-as.Date(si.s2$ADRstart, format = "%Y-%m-%d")

si.s2$treatmentstart1<-anytime(si.s2$Start.date)
si.s2$treatmentstart<-as.Date(si.s2$treatmentstart1, format = "%Y-%m-%d")

si.s2$ADRduration<- si.s2$ADRstartdate-si.s2$treatmentstart
si.s2$ADRduration[si.s2$ADRduration<0]<-NA

si.s2$ADRduration2<-as.numeric(si.s2$ADRduration)
summary(si.s2$ADRduration2)


###Extract information on seriousness
###Fatal -Death
table(reac_drug_case.s1$Fatal)

####Countries where ADRs were reported
countries.s <- sort(table(reac_drug_case.s1$`Country of primary source`),decreasing=T)

require("anytime")
reac_drug_case.s$ADRenddate.1<-anytime(reac_drug_case.s$ADR_end)
reac_drug_case.s$ADRenddate<-as.Date(reac_drug_case.s$ADRenddate.1, format = "%Y-%m-%d")
reac_drug_case.s$ADRstartdate.1<-anytime(reac_drug_case.s$ADR_start)
reac_drug_case.s$ADRstartdate<-as.Date(reac_drug_case.s$ADRstartdate.1, format = "%Y-%m-%d")

reac_drug_case.s$ADRduration<- reac_drug_case.s$ADRenddate-reac_drug_case.s$ADRstartdate
class(reac_drug_case.s$ADRduration)
reac_drug_case.s$ADRduration<-as.numeric(reac_drug_case.s$ADRduration)

summary(reac_drug_case.s$ADRduration)
###kick out non-plausible outliers, e.g. negative duration values (most likely due to reporting issues)
reac_drug_case.s$ADRduration[reac_drug_case.s$ADRduration<=0]<-NA

###extract % of indications for suicidal ideation
table(reac_drug_case.s$ind[reac_drug_case.s$MedDRA=="Suicidal ideation"], reac_drug_case.s$Sex[reac_drug_case.s$MedDRA=="Suicidal ideation"])
###Remove PCOS indication
withoutpcos.s<-reac_drug_case.s[!reac_drug_case.s$ind=="PCOS",]
table(withoutpcos.s$ind[withoutpcos.s$MedDRA=="Suicidal ideation"], withoutpcos.s$Sex[withoutpcos.s$MedDRA=="Suicidal ideation"])

###Comparisons of sex distribution, dose and age for suicidal ideation
fisher.test(withoutpcos.s$ind[withoutpcos.s$MedDRA=="Suicidal ideation"], withoutpcos.s$Sex[withoutpcos.s$MedDRA=="Suicidal ideation"])

by(reac_drug_case.s$Dose.x[reac_drug_case.s$MedDRA=="Suicidal ideation" & reac_drug_case.s$Dose.unit=="mg"],reac_drug_case.s$ind[reac_drug_case.s$MedDRA=="Suicidal ideation"&reac_drug_case.s$Dose.unit=="mg"],summary)
kruskal.test(withoutpcos.s$Dose.x[withoutpcos.s$MedDRA=="Suicidal ideation"& withoutpcos.s$Dose.unit=="mg"] ~ withoutpcos.s$ind[withoutpcos.s$MedDRA=="Suicidal ideation"& withoutpcos.s$Dose.unit=="mg"])

by(reac_drug_case.s$Age[reac_drug_case.s$MedDRA=="Suicidal ideation"],reac_drug_case.s$ind[reac_drug_case.s$MedDRA=="Suicidal ideation"],summary)
kruskal.test(withoutpcos.s$Age[withoutpcos.s$MedDRA=="Suicidal ideation"] ~ withoutpcos.s$ind[withoutpcos.s$MedDRA=="Suicidal ideation"])


####################################
####################################
####Drug-reaction link sheet #######
####################################
####################################
library(readxl)
VigiLyze4 <- read_excel("VigiLyze line listing_GLP1 RA_HLGT-suicidal self-injurious.xlsx", 
                        sheet = "Drug - reaction link")
colnames(VigiLyze4)
names(VigiLyze4)[1]<-paste("PrimaryId")
names(VigiLyze4)[2]<-paste("SafetyId")
names(VigiLyze4)[4]<-paste("MedDRA")
names(VigiLyze4)[5]<-paste("Time-to-onset")


library(tidyr)
VigiLyze4.1<-separate_wider_delim(VigiLyze4, cols = "Dechallenge performed? / Reaction resolved/resolving?", delim = "/", names = c("Dechallenge performed?", "DE_Reaction resolved/resolving?"))
VigiLyze4.2<-separate_wider_delim(VigiLyze4.1, cols = "Rechallenge performed? / Reaction recurred?", delim = "/", names = c("Rechallenge performed", "RE_Reaction resolved/resolving?"))
VigiLyze4.2<-separate_wider_delim(VigiLyze4.2, cols = `Time-to-onset`, delim = " ", names = c("time", "time_unit"))
VigiLyze4.2$time<-as.numeric(VigiLyze4.2$time)
table(VigiLyze4.2$time_unit)

####semaglutide
VigiLyze4.2$temps<-array(0,length(VigiLyze4.2$PrimaryId))
for(i in 1:length(VigiLyze4.2$PrimaryId)){
  for(j in 1:length(ids)){
    if(VigiLyze4.2$PrimaryId[i]==ids[j]){VigiLyze4.2$temps[i]<-1}}}
table(VigiLyze4.2$temps)

VigiLyze4.2.s<-VigiLyze4.2[VigiLyze4.2$temps==1,]
length(VigiLyze4.2.s$PrimaryId)
table(VigiLyze4.2.s$`WHODrug active ingredient variant`)

###merge reactions & drugs WITH drug-reaction link
reac_drug_case.sd<-merge(reac_drug.s,VigiLyze4.2.s,by.x = c("PrimaryId","MedDRA"), by.y = c("PrimaryId","MedDRA"), all = TRUE)

###Eliminate duplicates
require(dplyr)
ncol(reac_drug_case.sd)
reac_drug_case.sd$temp2<-apply(reac_drug_case.sd[1:44], 1, function(x) paste(sort(x), collapse = "_"))

reac_drug_case.sdf<-distinct(reac_drug_case.sd,reac_drug_case.sd$temp2,.keep_all = TRUE)
length(reac_drug_case.sdf$PrimaryId)

###Extract information on dechallenge per type of adverse reaction
####Suicidal ideation
table(reac_drug_case.sdf$`Dechallenge performed?`[reac_drug_case.sdf$MedDRA=="Suicidal ideation"])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal ideation" & reac_drug_case.sdf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal ideation" & reac_drug_case.sdf$`Dechallenge performed?`=="No "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal ideation" & reac_drug_case.sdf$`Dechallenge performed?`=="Unknown "])
#####Intentional overdose
table(reac_drug_case.sdf$`Dechallenge performed?`[reac_drug_case.sdf$MedDRA=="Intentional overdose"])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional overdose" & reac_drug_case.sdf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional overdose" & reac_drug_case.sdf$`Dechallenge performed?`=="No "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional overdose" & reac_drug_case.sdf$`Dechallenge performed?`=="Unknown "])
#####Suicidal behaviour
table(reac_drug_case.sdf$`Dechallenge performed?`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour"])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour" & reac_drug_case.sdf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour" & reac_drug_case.sdf$`Dechallenge performed?`=="No "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour" & reac_drug_case.sdf$`Dechallenge performed?`=="Unknown "])
#####Suicide attempt
table(reac_drug_case.sdf$`Dechallenge performed?`[reac_drug_case.sdf$MedDRA=="Suicide attempt"])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicide attempt" & reac_drug_case.sdf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicide attempt" & reac_drug_case.sdf$`Dechallenge performed?`=="No "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicide attempt" & reac_drug_case.sdf$`Dechallenge performed?`=="Unknown "])
#####Intentional self-injury
table(reac_drug_case.sdf$`Dechallenge performed?`[reac_drug_case.sdf$MedDRA=="Intentional self-injury"])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional self-injury" & reac_drug_case.sdf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional self-injury" & reac_drug_case.sdf$`Dechallenge performed?`=="No "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional self-injury" & reac_drug_case.sdf$`Dechallenge performed?`=="Unknown "])
#####Self-injurious ideation
table(reac_drug_case.sdf$`Dechallenge performed?`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation"])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation" & reac_drug_case.sdf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation" & reac_drug_case.sdf$`Dechallenge performed?`=="No "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation" & reac_drug_case.sdf$`Dechallenge performed?`=="Unknown "])
#####Depression suicidal
table(reac_drug_case.sdf$`Dechallenge performed?`[reac_drug_case.sdf$MedDRA=="Depression suicidal"])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Depression suicidal" & reac_drug_case.sdf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Depression suicidal" & reac_drug_case.sdf$`Dechallenge performed?`=="No "])
table(reac_drug_case.sdf$`DE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Depression suicidal" & reac_drug_case.sdf$`Dechallenge performed?`=="Unknown "])


###Extract information on rechallenge per type of adverse reaction
####Suicidal ideation
table(reac_drug_case.sdf$`Rechallenge performed`[reac_drug_case.sdf$MedDRA=="Suicidal ideation"])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal ideation" & reac_drug_case.sdf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal ideation" & reac_drug_case.sdf$`Rechallenge performed`=="No "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal ideation" & reac_drug_case.sdf$`Rechallenge performed`=="Unknown "])
#####Intentional overdose
table(reac_drug_case.sdf$`Rechallenge performed`[reac_drug_case.sdf$MedDRA=="Intentional overdose"])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional overdose" & reac_drug_case.sdf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional overdose" & reac_drug_case.sdf$`Rechallenge performed`=="No "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional overdose" & reac_drug_case.sdf$`Rechallenge performed`=="Unknown "])
#####Suicidal behaviour
table(reac_drug_case.sdf$`Rechallenge performed`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour"])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour" & reac_drug_case.sdf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour" & reac_drug_case.sdf$`Rechallenge performed`=="No "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicidal behaviour" & reac_drug_case.sdf$`Rechallenge performed`=="Unknown "])
#####Suicide attempt
table(reac_drug_case.sdf$`Rechallenge performed`[reac_drug_case.sdf$MedDRA=="Suicide attempt"])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicide attempt" & reac_drug_case.sdf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicide attempt" & reac_drug_case.sdf$`Rechallenge performed`=="No "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Suicide attempt" & reac_drug_case.sdf$`Rechallenge performed`=="Unknown "])
#####Intentional self-injury
table(reac_drug_case.sdf$`Rechallenge performed`[reac_drug_case.sdf$MedDRA=="Intentional self-injury"])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional self-injury" & reac_drug_case.sdf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional self-injury" & reac_drug_case.sdf$`Rechallenge performed`=="No "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Intentional self-injury" & reac_drug_case.sdf$`Rechallenge performed`=="Unknown "])
#####Self-injurious ideation
table(reac_drug_case.sdf$`Rechallenge performed`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation"])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation" & reac_drug_case.sdf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation" & reac_drug_case.sdf$`Rechallenge performed`=="No "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Self-injurious ideation" & reac_drug_case.sdf$`Rechallenge performed`=="Unknown "])
#####Depression suicidal
table(reac_drug_case.sdf$`Rechallenge performed`[reac_drug_case.sdf$MedDRA=="Depression suicidal"])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Depression suicidal" & reac_drug_case.sdf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Depression suicidal" & reac_drug_case.sdf$`Rechallenge performed`=="No "])
table(reac_drug_case.sdf$`RE_Reaction resolved/resolving?`[reac_drug_case.sdf$MedDRA=="Depression suicidal" & reac_drug_case.sdf$`Rechallenge performed`=="Unknown "])


###Extract information on time to onset
timetoonset.s<-subset(reac_drug_case.sdf,!is.na(reac_drug_case.sdf$time_unit))
###unify units of time
timetoonset.s$time1<-timetoonset.s$time
for(i in 1:length(timetoonset.s$time_unit)){
      if(timetoonset.s$time_unit[i]=="months"){timetoonset.s$time1[i]<-timetoonset.s$time[i]*30}
  if(timetoonset.s$time_unit[i]=="month"){timetoonset.s$time1[i]<-30}
  if(timetoonset.s$time_unit[i]=="hours"){timetoonset.s$time1[i]<-0}}

by(timetoonset.s$time1,timetoonset.s$MedDRA,summary)
length(unique(timetoonset.s$PrimaryId))
timetoonset.s$time1[timetoonset.s$MedDRA=="Completed suicide"]
timetoonset.s$time1[timetoonset.s$MedDRA=="Suicide attempt"]
timetoonset.s$time1[timetoonset.s$MedDRA=="Suicidal ideation"]
timetoonset.s$time1[timetoonset.s$MedDRA=="Intentional overdose"]
timetoonset.s$time1[timetoonset.s$MedDRA=="Intentional self-injury"]
timetoonset.s$time1[timetoonset.s$MedDRA=="Self-injurious ideation"]


###############################################
##############################################
####        liraglutide           ############
##############################################
##############################################      
####Extract data on liraglutide
VigiLyze1$temp.l<-array(0,length(VigiLyze1$`WHODrug active ingredient variant`))
VigiLyze1$temp.l[VigiLyze1$`WHODrug active ingredient variant`=="Liraglutide"]<-1


VigiLyze1.l<-transform(VigiLyze1, totaltempl= ave(VigiLyze1$temp.l, 
                                                  VigiLyze1$PrimaryId, FUN=sum))[-2]
table(VigiLyze1.l$totaltempl)
##here include only cases medicated with liraglutide
VigiLyze1.1.l<-VigiLyze1.l[VigiLyze1.l$totaltempl>0,]
length(VigiLyze1.1.l$PrimaryId)

##now ensure information on indications before removing duplicates 
library(tidyverse)
VigiLyze1.2.l<- VigiLyze1.1.l %>% 
  group_by(PrimaryId,WHODrug.active.ingredient.variant) %>% 
  fill(Indication) 

View(VigiLyze1.2.l)  

##remove duplicates
ncol(VigiLyze1.2.l)
require(dplyr)
VigiLyze1.2.l$temp2<-apply(VigiLyze1.2.l[1:18], 1, function(x) paste(sort(x), collapse = "_"))

VigiLyze1.3.l<-distinct(VigiLyze1.2.l,VigiLyze1.2.l$temp2,.keep_all = TRUE)
length(VigiLyze1.3.l$PrimaryId)
length(unique(VigiLyze1.3.l$PrimaryId))
##in total 162 patients with lira
View(VigiLyze1.3.l)

###Assess co-medication
table(VigiLyze1.3.l$WHODrug.active.ingredient.variant)
medications.l <- sort(table(VigiLyze1.3.l$WHODrug.active.ingredient.variant),decreasing=T)

###Co-medication with antidiabetic agents
VigiLyze1.3.l$antidiabetic<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Metformin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Insulin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Empagliflozin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Dapagliflozin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Glibenclamide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Gliclazide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Glipizide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Sitagliptin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Pioglitazone", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Voglibose", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Acarbose", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Canagliflozin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}
  if(length(i <- grep("Repaglinide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidiabetic[i]<-1}}

##Co-medication with PPIs
VigiLyze1.3.l$ppi<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Omeprazole", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$ppi[i]<-1}
  if(length(i <- grep("Lansoprazole", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$ppi[i]<-1}
  if(length(i <- grep("Esomeprazole", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$ppi[i]<-1}
  if(length(i <- grep("Pantoprazole", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$ppi[i]<-1}}

###Co-medication with statins/lipid-lowering agents
VigiLyze1.3.l$statin<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Simvastatin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$statin[i]<-1}
  if(length(i <- grep("Atorvastatin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$statin[i]<-1}
  if(length(i <- grep("Rosuvastatin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$statin[i]<-1}
  if(length(i <- grep("Pravastatin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$statin[i]<-1}
  if(length(i <- grep("Fenofibrate", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$statin[i]<-1}}

###Co-medication with painkillers
VigiLyze1.3.l$analg<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Paracetamol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Acetylsalicylic acid", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Tramadol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Carisoprodol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Celecoxib", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Hydrocodon", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Diclofenac", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Meloxicam", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Naproxen", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}
  if(length(i <- grep("Cyclobenzaprine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$analg[i]<-1}}

###Co-medication with antidepressants
VigiLyze1.3.l$antidepressant<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("italopram", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Amitriptyline", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Mirtazapine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Fluvoxamine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Bupropion", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Sertraline", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Buspirone", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Fluoxetine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("enlafaxine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Duloxetine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Clomipramine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Vortioxetine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}
  if(length(i <- grep("Trazodone", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antidepressant[i]<-1}}

###Co-medication with antipsychotics
VigiLyze1.3.l$antipsychotic<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Cariprazine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Haloperidol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Promethazine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Quetiapine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Risperidone", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Aripiprazole", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Olanzapine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Pipamperone", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Sulpiride", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}
  if(length(i <- grep("Ziprasidone", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antipsychotic[i]<-1}}

###Co-medication with benzoziapines
VigiLyze1.3.l$benzos<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Alprazolam", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}
  if(length(i <- grep("azepam", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}
  if(length(i <- grep("Mexazolam", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}
  if(length(i <- grep("Etizolam", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}
  if(length(i <- grep("Triazolam", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}
  if(length(i <- grep("Gabapentin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}
  if(length(i <- grep("Pregabalin", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}
  if(length(i <- grep("Brotizolam", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$benzos[i]<-1}}

###Co-medication with antiseizure medications
VigiLyze1.3.l$anticonvulsant<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Lamotrigine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$anticonvulsant[i]<-1}
  if(length(i <- grep("Topiramate", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$anticonvulsant[i]<-1}
  if(length(i <- grep("Valproate", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$anticonvulsant[i]<-1}}

###Co-medications with antihypertensive agents
VigiLyze1.3.l$antihypertensive<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Lisinopril", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Amlodipine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("lapril", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Atenolol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Bisoprolol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Propanolol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Metoprolol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Candesartan", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Hydrochlorothiazide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Lercanidipine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Furosemide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("sartan", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Ramipril", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Torasemide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}
  if(length(i <- grep("Spironolactone", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$antihypertensive[i]<-1}}

###Co-medications with antiasthmatics
VigiLyze1.3.l$asthma<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("uticasone ", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$asthma[i]<-1}
  if(length(i <- grep("Salmeterol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$asthma[i]<-1}
  if(length(i <- grep("Salbutamol", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$asthma[i]<-1}
  if(length(i <- grep("Desloratadine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$asthma[i]<-1}
  if(length(i <- grep("Budesonide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$asthma[i]<-1}
  if(length(i <- grep("Loratadine", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$asthma[i]<-1}
  if(length(i <- grep("Montelukast", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$asthma[i]<-1}}

###Co-medications with other GLPS
VigiLyze1.3.l$glps<-array(0,length(VigiLyze1.3.l$WHODrug.active.ingredient.variant))
for(i in 1:length(VigiLyze1.3.l$WHODrug.active.ingredient.variant)){
  if(length(i <- grep("Exenatide ", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$glps[i]<-1}
  if(length(i <- grep("Semaglutide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$glps[i]<-1}
  if(length(i <- grep("Tirzepatide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$glps[i]<-1}
  if(length(i <- grep("Dulaglutide", VigiLyze1.3.l$WHODrug.active.ingredient.variant))){VigiLyze1.3.l$glps[i]<-1}}

VigiLyze1.4.l<-transform(VigiLyze1.3.l, anticonv= ave(VigiLyze1.3.l$anticonvulsant, 
                                                      VigiLyze1.3.l$PrimaryId, FUN=sum),
                         antihypertensives= ave(VigiLyze1.3.l$antihypertensive, 
                                                VigiLyze1.3.l$PrimaryId, FUN=sum), 
                         benzodiazepines= ave(VigiLyze1.3.l$benzos, 
                                              VigiLyze1.3.l$PrimaryId, FUN=sum),
                         benzodiazepines= ave(VigiLyze1.3.l$benzos, 
                                              VigiLyze1.3.l$PrimaryId, FUN=sum),
                         antipsych= ave(VigiLyze1.3.l$antipsychotic, 
                                        VigiLyze1.3.l$PrimaryId, FUN=sum),
                         antidep= ave(VigiLyze1.3.l$antidepressant, 
                                      VigiLyze1.3.l$PrimaryId, FUN=sum),
                         antidiabetics= ave(VigiLyze1.3.l$antidiabetic, 
                                            VigiLyze1.3.l$PrimaryId, FUN=sum),
                         asthm= ave(VigiLyze1.3.l$asthma, 
                                    VigiLyze1.3.l$PrimaryId, FUN=sum), 
                         analgetics= ave(VigiLyze1.3.l$analg, 
                                         VigiLyze1.3.l$PrimaryId, FUN=sum),
                         ppis= ave(VigiLyze1.3.l$ppi, 
                                   VigiLyze1.3.l$PrimaryId, FUN=sum),
                         statins= ave(VigiLyze1.3.l$statin, 
                                      VigiLyze1.3.l$PrimaryId, FUN=sum),
                         glp= ave(VigiLyze1.3.l$glps, 
                                  VigiLyze1.3.l$PrimaryId, FUN=sum))[-2]

###Assess indication
VigiLyze1.5.l<-VigiLyze1.4.l[VigiLyze1.4.l$WHODrug.active.ingredient.variant=="Liraglutide",]
length(VigiLyze1.5.l$PrimaryId)
length(unique(VigiLyze1.5.l$PrimaryId))
table(VigiLyze1.5.l$WHODrug.active.ingredient.variant)
##create a new file
library(tidyverse)
VigiLyze1.6.l<-distinct(VigiLyze1.5.l,VigiLyze1.5.l$PrimaryId,.keep_all = TRUE)

indications.l <- sort(table(VigiLyze1.6.l$Indication),decreasing=T)

VigiLyze1.6.l$ind<-array(,length(VigiLyze1.6.l$Indication))
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Weight control"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Obesity"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Overweight"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Weight decreased"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Weight loss"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Adipositas"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Inability to lose weight"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Metabolic syndrome"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Morbid obesity"]<-"Weight"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Body mass index high"]<-"Weight"

VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Product used for unknown indication"]<-"off-label"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Drug use for unknown indicationn"]<-"off-label"

VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Type 2 diabetes mellitus"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Diabetes mellitus"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Type 2 diabetes mellitus"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Diabetes"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Type II diabetes mellitus"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Type II diabetes mellitus inadequate control"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Blood glucose increased"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Glucose tolerance impaired"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Glycosylated haemoglobin abnormal"]<-"diabetes"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Blood glucose increased"]<-"diabetes"

VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Polycystic ovaries"]<-"PCOS"
VigiLyze1.6.l$ind[VigiLyze1.6.l$Indication=="Polycystic ovarian syndrome"]<-"PCOS"


table(VigiLyze1.6.l$ind)

###here extract information on comedications
length(VigiLyze1.6.l$antidiabetics[VigiLyze1.6.l$antidiabetics>0])
length(VigiLyze1.6.l$antidep[VigiLyze1.6.l$antidep>0])
length(VigiLyze1.6.l$benzodiazepines[VigiLyze1.6.l$benzodiazepines>0])
length(VigiLyze1.6.l$ppis[VigiLyze1.6.l$ppis>0])
length(VigiLyze1.6.l$statins[VigiLyze1.6.l$statins>0])
length(VigiLyze1.6.l$analgetics[VigiLyze1.6.l$analgetics>0])
length(VigiLyze1.6.l$antipsych[VigiLyze1.6.l$antipsych>0])
length(VigiLyze1.6.l$anticonv[VigiLyze1.6.l$anticonv>0])
length(VigiLyze1.6.l$antihypertensives[VigiLyze1.6.l$antihypertensives>0])
length(VigiLyze1.6.l$asthm[VigiLyze1.6.l$asthm>0])

###overlapping medications
table(VigiLyze1.6.l$antidep[VigiLyze1.6.l$antidiabetics>0])
table(VigiLyze1.6.l$antidiabetics[VigiLyze1.6.l$antidep>0])


##Extract information on dosing
by(VigiLyze1.6.l$Dose, VigiLyze1.6.l$Dose.unit ,summary)
VigiLyze1.6.l$Dose[VigiLyze1.6.l$Dose.unit=="mg"] 
VigiLyze1.6.l$Dose[VigiLyze1.6.l$Dose==36 & VigiLyze1.6.l$Dose.unit=="mg"]<-3.6 
VigiLyze1.6.l$Dose[VigiLyze1.6.l$Dose==27 & VigiLyze1.6.l$Dose.unit=="mg"]<-NA 

table(VigiLyze1.6.l$Dosage.regimen)
table(VigiLyze1.6.l$Action.taken.with.drug)

####################
####################
####Reactions#######
####################
#####################
idl <- unique(VigiLyze1.6.l$PrimaryId) #here we select the unique IDs

VigiLyze2$templ<-array(0,length(VigiLyze2$PrimaryId))
for(i in 1:length(VigiLyze2$PrimaryId)){
  for(j in 1:length(idl)){
    if(VigiLyze2$PrimaryId[i]==idl[j]){VigiLyze2$templ[i]<-1}}}
table(VigiLyze2$templ)

VigiLyze2.1.l<-VigiLyze2[VigiLyze2$templ==1,]
length(VigiLyze2.1.l$PrimaryId)

###merge reactions and drugs sheets
reac_drug.l<-merge(VigiLyze2.1.l,VigiLyze1.6.l,by.x = "PrimaryId", by.y = "PrimaryId")
length(reac_drug.l$PrimaryId)
View(reac_drug.l)

medra.l <- sort(table(reac_drug.l$MedDRA),decreasing=T)

table(reac_drug.l$MedDRA, reac_drug.l$ind)

VigiLyze3$templ<-array(0,length(VigiLyze3$PrimaryId))
for(i in 1:length(VigiLyze3$PrimaryId)){
  for(j in 1:length(idl)){
    if(VigiLyze3$PrimaryId[i]==idl[j]){VigiLyze3$templ[i]<-1}}}
table(VigiLyze3$templ)

VigiLyze3.1.l<-VigiLyze3[VigiLyze3$templ==1,]
length(VigiLyze3.1.l$PrimaryId)

###merge reactions and drugs containing liraglutide
reac_drug_case.l<-merge(reac_drug.l,VigiLyze3.1.l,by.x = c("PrimaryId","SafetyId"), by.y = c("PrimaryId","SafetyId"))

###Extract information on demographic characteristics
##Age (all in years)
table(reac_drug_case.l$`Age unit`)
reac_drug_case.l1<-distinct(reac_drug_case.l,reac_drug_case.l$PrimaryId,.keep_all = TRUE)
summary(reac_drug_case.l1$Age,na.rm =T)

###Sex
table(reac_drug_case.l1$Sex)

###Body Weight 
summary(reac_drug_case.l1$`Weight (kg)`)

###Height
summary(reac_drug_case.l1$`Height (cm)`)

reac_drug_case.l1$BMI<-reac_drug_case.l1$`Weight (kg)`/((reac_drug_case.l1$`Height (cm)`/100)^2)
summary(reac_drug_case.l1$BMI)

###extract information on Seriousness
###Fatal -Death
table(reac_drug_case.l1$Fatal)

####extract information on countries of reporting
countries.l <- sort(table(reac_drug_case.l1$`Country of primary source`),decreasing=T)

require("anytime")
reac_drug_case.l$ADRenddate.1<-anytime(reac_drug_case.l$ADR_end)
reac_drug_case.l$ADRenddate<-as.Date(reac_drug_case.l$ADRenddate.1, format = "%Y-%m-%d")
reac_drug_case.l$ADRstartdate.1<-anytime(reac_drug_case.l$ADR_start)
reac_drug_case.l$ADRstartdate<-as.Date(reac_drug_case.l$ADRstartdate.1, format = "%Y-%m-%d")

reac_drug_case.l$ADRduration<- reac_drug_case.l$ADRenddate-reac_drug_case.l$ADRstartdate
class(reac_drug_case.l$ADRduration)
reac_drug_case.l$ADRduration<-as.numeric(reac_drug_case.l$ADRduration)

summary(reac_drug_case.l$ADRduration)
###kick out non-plausible outliers
reac_drug_case.l$ADRduration[reac_drug_case.l$ADRduration<=0]<-NA

###assess % of indications for suicidal ideation
table(reac_drug_case.l$ind[reac_drug_case.l$MedDRA=="Suicidal ideation"], reac_drug_case.l$Sex[reac_drug_case.l$MedDRA=="Suicidal ideation"])
###remove pcos out indication PCOS cases
withoutpcos.l<-reac_drug_case.l[!reac_drug_case.l$ind=="PCOS",]
table(withoutpcos.l$ind[withoutpcos.l$MedDRA=="Suicidal ideation"], withoutpcos.l$Sex[withoutpcos.l$MedDRA=="Suicidal ideation"])

###Comparisons of sex, age and doses between indications for suicidal ideation
fisher.test(withoutpcos.l$ind[withoutpcos.l$MedDRA=="Suicidal ideation"], withoutpcos.l$Sex[withoutpcos.l$MedDRA=="Suicidal ideation"])

by(reac_drug_case.l$Dose.x[reac_drug_case.l$MedDRA=="Suicidal ideation" & reac_drug.l$Dose.unit=="mg"],reac_drug_case.l$ind[reac_drug_case.l$MedDRA=="Suicidal ideation" & reac_drug.l$Dose.unit=="mg"],summary)
kruskal.test(withoutpcos.l$Dose.x[withoutpcos.l$MedDRA=="Suicidal ideation"& withoutpcos.l$Dose.unit=="mg"] ~ withoutpcos.l$ind[withoutpcos.l$MedDRA=="Suicidal ideation"& withoutpcos.l$Dose.unit=="mg"])

by(reac_drug_case.l$Age[reac_drug_case.s$MedDRA=="Suicidal ideation"],reac_drug_case.l$ind[reac_drug_case.s$MedDRA=="Suicidal ideation"],summary)
kruskal.test(withoutpcos.l$Age[withoutpcos.l$MedDRA=="Suicidal ideation"] ~ withoutpcos.l$ind[withoutpcos.l$MedDRA=="Suicidal ideation"])

#############################
#############################
####Drug-reaction link ######
#############################
############################
####liraglutide
VigiLyze4.2$templ<-array(0,length(VigiLyze4.2$PrimaryId))
for(i in 1:length(VigiLyze4.2$PrimaryId)){
  for(j in 1:length(idl)){
    if(VigiLyze4.2$PrimaryId[i]==idl[j]){VigiLyze4.2$templ[i]<-1}}}
table(VigiLyze4.2$templ)

VigiLyze4.2.l<-VigiLyze4.2[VigiLyze4.2$templ==1,]
length(VigiLyze4.2.l$PrimaryId)
table(VigiLyze4.2.l$`WHODrug active ingredient variant`)

###merge reactions & drugs WITH drug-reaction link
reac_drug_case.ld<-merge(reac_drug_case.l,VigiLyze4.2.l,by.x = c("PrimaryId","MedDRA"), by.y = c("PrimaryId","MedDRA"))
length(reac_drug_case.ld$PrimaryId)
View(reac_drug_case.ld)

###remove duplicates
require(dplyr)
ncol(reac_drug_case.ld)
reac_drug_case.ld$temp2<-apply(reac_drug_case.ld[1:45], 1, function(x) paste(sort(x), collapse = "_"))

reac_drug_case.ldf<-distinct(reac_drug_case.ld,reac_drug_case.ld$temp2,.keep_all = TRUE)
length(reac_drug_case.ldf$PrimaryId)

###Extract information on dechallenge per type of adverse reaction
####Suicidal ideation
table(reac_drug_case.ldf$`Dechallenge performed?`[reac_drug_case.ldf$MedDRA=="Suicidal ideation"])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal ideation" & reac_drug_case.ldf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal ideation" & reac_drug_case.ldf$`Dechallenge performed?`=="No "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal ideation" & reac_drug_case.ldf$`Dechallenge performed?`=="Unknown "])
#####Intentional overdose
table(reac_drug_case.ldf$`Dechallenge performed?`[reac_drug_case.ldf$MedDRA=="Intentional overdose"])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional overdose" & reac_drug_case.ldf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional overdose" & reac_drug_case.ldf$`Dechallenge performed?`=="No "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional overdose" & reac_drug_case.ldf$`Dechallenge performed?`=="Unknown "])
#####Suicidal behaviour
table(reac_drug_case.ldf$`Dechallenge performed?`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour"])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour" & reac_drug_case.ldf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour" & reac_drug_case.ldf$`Dechallenge performed?`=="No "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour" & reac_drug_case.ldf$`Dechallenge performed?`=="Unknown "])
#####Suicide attempt
table(reac_drug_case.ldf$`Dechallenge performed?`[reac_drug_case.ldf$MedDRA=="Suicide attempt"])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicide attempt" & reac_drug_case.ldf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicide attempt" & reac_drug_case.ldf$`Dechallenge performed?`=="No "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicide attempt" & reac_drug_case.ldf$`Dechallenge performed?`=="Unknown "])
#####Intentional self-injury
table(reac_drug_case.ldf$`Dechallenge performed?`[reac_drug_case.ldf$MedDRA=="Intentional self-injury"])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional self-injury" & reac_drug_case.ldf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional self-injury" & reac_drug_case.ldf$`Dechallenge performed?`=="No "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional self-injury" & reac_drug_case.ldf$`Dechallenge performed?`=="Unknown "])
#####Self-injurious ideation
table(reac_drug_case.ldf$`Dechallenge performed?`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation"])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation" & reac_drug_case.ldf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation" & reac_drug_case.ldf$`Dechallenge performed?`=="No "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation" & reac_drug_case.ldf$`Dechallenge performed?`=="Unknown "])
#####Depression suicidal
table(reac_drug_case.ldf$`Dechallenge performed?`[reac_drug_case.ldf$MedDRA=="Depression suicidal"])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Depression suicidal" & reac_drug_case.ldf$`Dechallenge performed?`=="Yes "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Depression suicidal" & reac_drug_case.ldf$`Dechallenge performed?`=="No "])
table(reac_drug_case.ldf$`DE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Depression suicidal" & reac_drug_case.ldf$`Dechallenge performed?`=="Unknown "])

###Extract information on rechallenge per type of adverse reaction
####Suicidal ideation
table(reac_drug_case.ldf$`Rechallenge performed`[reac_drug_case.ldf$MedDRA=="Suicidal ideation"])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal ideation" & reac_drug_case.ldf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal ideation" & reac_drug_case.ldf$`Rechallenge performed`=="No "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal ideation" & reac_drug_case.ldf$`Rechallenge performed`=="Unknown "])
#####Intentional overdose
table(reac_drug_case.ldf$`Rechallenge performed`[reac_drug_case.ldf$MedDRA=="Intentional overdose"])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional overdose" & reac_drug_case.ldf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional overdose" & reac_drug_case.ldf$`Rechallenge performed`=="No "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional overdose" & reac_drug_case.ldf$`Rechallenge performed`=="Unknown "])
#####Suicidal behaviour
table(reac_drug_case.ldf$`Rechallenge performed`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour"])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour" & reac_drug_case.ldf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour" & reac_drug_case.ldf$`Rechallenge performed`=="No "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicidal behaviour" & reac_drug_case.ldf$`Rechallenge performed`=="Unknown "])
#####Suicide attempt
table(reac_drug_case.ldf$`Rechallenge performed`[reac_drug_case.ldf$MedDRA=="Suicide attempt"])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicide attempt" & reac_drug_case.ldf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicide attempt" & reac_drug_case.ldf$`Rechallenge performed`=="No "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Suicide attempt" & reac_drug_case.ldf$`Rechallenge performed`=="Unknown "])
#####Intentional self-injury
table(reac_drug_case.ldf$`Rechallenge performed`[reac_drug_case.ldf$MedDRA=="Intentional self-injury"])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional self-injury" & reac_drug_case.ldf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional self-injury" & reac_drug_case.ldf$`Rechallenge performed`=="No "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Intentional self-injury" & reac_drug_case.ldf$`Rechallenge performed`=="Unknown "])
#####Self-injurious ideation
table(reac_drug_case.ldf$`Rechallenge performed`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation"])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation" & reac_drug_case.ldf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation" & reac_drug_case.ldf$`Rechallenge performed`=="No "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Self-injurious ideation" & reac_drug_case.ldf$`Rechallenge performed`=="Unknown "])
#####Depression suicidal
table(reac_drug_case.ldf$`Rechallenge performed`[reac_drug_case.ldf$MedDRA=="Depression suicidal"])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Depression suicidal" & reac_drug_case.ldf$`Rechallenge performed`=="Yes "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Depression suicidal" & reac_drug_case.ldf$`Rechallenge performed`=="No "])
table(reac_drug_case.ldf$`RE_Reaction resolved/resolving?`[reac_drug_case.ldf$MedDRA=="Depression suicidal" & reac_drug_case.ldf$`Rechallenge performed`=="Unknown "])

###Extract information on time to onset
timetoonset.l<-subset(reac_drug_case.ldf,!is.na(reac_drug_case.ldf$time_unit))
###unify units of time
timetoonset.l$time1<-timetoonset.l$time
for(i in 1:length(timetoonset.l$time_unit)){
  if(timetoonset.l$time_unit[i]=="months"){timetoonset.l$time1[i]<-timetoonset.l$time[i]*30}
  if(timetoonset.l$time_unit[i]=="month"){timetoonset.l$time1[i]<-30}
  if(timetoonset.l$time_unit[i]=="hours"){timetoonset.l$time1[i]<-0}}

by(timetoonset.l$time1,timetoonset.l$MedDRA,summary)
length(unique(timetoonset.l$PrimaryId))
timetoonset.l$time1[timetoonset.l$MedDRA=="Completed suicide"]
timetoonset.l$time1[timetoonset.l$MedDRA=="Suicide attempt"]
timetoonset.l$time1[timetoonset.l$MedDRA=="Suicidal ideation"]
timetoonset.l$time1[timetoonset.l$MedDRA=="Intentional overdose"]
timetoonset.l$time1[timetoonset.l$MedDRA=="Intentional self-injury"]
timetoonset.l$time1[timetoonset.l$MedDRA=="Self-injurious ideation"]
timetoonset.l$time1[timetoonset.l$MedDRA=="Depression suicidal"]
timetoonset.l$time1[timetoonset.l$MedDRA=="Suspected suicide"]

###########################################
############################################
#### MAIN DISPROPORTIONALITY ANALYSIS#######
###########################################
############################################

library(readxl)
glp_dis <- read_excel("glp_dis_cg.xlsx")
##Liraglutide 
glp_disl<-glp_dis[!glp_dis$`Drug lab`=="Semaglutide",]

##Suicidal ideation
ADR1<-glp_disl[glp_disl$`AE lab`=="Suicidal ideation"|glp_disl$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Liraglutide"&ADR1$`AE lab`=="Rest"]<-52131-ADR1$n11[ADR1$`Drug lab`=="Liraglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Rest"]<-36172078-ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Liraglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],36172078-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

##Intentional overdose
ADR2<-glp_disl[glp_disl$`AE lab`=="Intentional overdose"|glp_disl$`AE lab`=="Rest",]
ADR2$n11[ADR2$`Drug lab`=="Liraglutide"&ADR2$`AE lab`=="Rest"]<-52131-ADR2$n11[ADR2$`Drug lab`=="Liraglutide"&ADR2$`AE lab`=="Intentional overdose"]
ADR2$n11[ADR2$`Drug lab`=="Rest"&ADR2$`AE lab`=="Rest"]<-36172078-ADR2$n11[ADR2$`Drug lab`=="Rest"&ADR2$`AE lab`=="Intentional overdose"]
require("PhViD")
ADR2.1<-as.PhViD(data.frame(ADR2),MARGIN.THRES = 1)
ADR2.2<-ROR(ADR2.1,MIN.n11 = 3)

ADR2.3<-ADR2.2$ALLSIGNALS
ADR2.4<-ADR2.3[ADR2.3$`drug code`=="Liraglutide" & ADR2.3$`event effect`=="Intentional overdose",]
ADR2.4$`event effect`<-as.character(ADR2.4$`event effect`)

OADR[2,]<-c(ADR2.4[2],ADR2.4[3],ADR2.4[7]-ADR2.4[3],ADR2.4[8]-ADR2.4[3],36172078-ADR2.4[8]+ADR2.4[3],ADR2.4[6])

##Suicide attempt
ADR3<-glp_disl[glp_disl$`AE lab`=="Suicide attempt"|glp_disl$`AE lab`=="Rest",]
ADR3$n11[ADR3$`Drug lab`=="Liraglutide"&ADR3$`AE lab`=="Rest"]<-52131-ADR3$n11[ADR3$`Drug lab`=="Liraglutide"&ADR3$`AE lab`=="Suicide attempt"]
ADR3$n11[ADR3$`Drug lab`=="Rest"&ADR3$`AE lab`=="Rest"]<-36172078-ADR3$n11[ADR3$`Drug lab`=="Rest"&ADR3$`AE lab`=="Suicide attempt"]
require("PhViD")
ADR3.1<-as.PhViD(data.frame(ADR3),MARGIN.THRES = 1)
ADR3.2<-ROR(ADR3.1,MIN.n11 = 3)

ADR3.3<-ADR3.2$ALLSIGNALS
ADR3.4<-ADR3.3[ADR3.3$`drug code`=="Liraglutide" & ADR3.3$`event effect`=="Suicide attempt",]
ADR3.4$`event effect`<-as.character(ADR3.4$`event effect`)

OADR[3,]<-c(ADR3.4[2],ADR3.4[3],ADR3.4[7]-ADR3.4[3],ADR3.4[8]-ADR3.4[3],36172078-ADR3.4[8]+ADR3.4[3],ADR3.4[6])

###Completed Suicide
ADR4<-glp_disl[glp_disl$`AE lab`=="Completed suicide"|glp_disl$`AE lab`=="Rest",]
ADR4$n11[ADR4$`Drug lab`=="Liraglutide"&ADR4$`AE lab`=="Rest"]<-52131-ADR4$n11[ADR4$`Drug lab`=="Liraglutide"&ADR4$`AE lab`=="Completed suicide"]
ADR4$n11[ADR4$`Drug lab`=="Rest"&ADR4$`AE lab`=="Rest"]<-36172078-ADR4$n11[ADR4$`Drug lab`=="Rest"&ADR4$`AE lab`=="Completed suicide"]
require("PhViD")
ADR4.1<-as.PhViD(data.frame(ADR4),MARGIN.THRES = 1)
ADR4.2<-ROR(ADR4.1,MIN.n11 = 3)

ADR4.3<-ADR4.2$ALLSIGNALS
ADR4.4<-ADR4.3[ADR4.3$`drug code`=="Liraglutide" & ADR4.3$`event effect`=="Completed suicide",]
ADR4.4$`event effect`<-as.character(ADR4.4$`event effect`)

OADR[4,]<-c(ADR4.4[2],ADR4.4[3],ADR4.4[7]-ADR4.4[3],ADR4.4[8]-ADR4.4[3],36172078-ADR4.4[8]+ADR4.4[3],ADR4.4[6])

##Suicidal behaviour
ADR5<-glp_disl[glp_disl$`AE lab`=="Suicidal behaviour"|glp_disl$`AE lab`=="Rest",]
ADR5$n11[ADR5$`Drug lab`=="Liraglutide"&ADR5$`AE lab`=="Rest"]<-52131-ADR5$n11[ADR5$`Drug lab`=="Liraglutide"&ADR5$`AE lab`=="Suicidal behaviour"]
ADR5$n11[ADR5$`Drug lab`=="Rest"&ADR5$`AE lab`=="Rest"]<-36172078-ADR5$n11[ADR5$`Drug lab`=="Rest"&ADR5$`AE lab`=="Suicidal behaviour"]
require("PhViD")
ADR5.1<-as.PhViD(data.frame(ADR5),MARGIN.THRES = 1)
ADR5.2<-ROR(ADR5.1,MIN.n11 = 3)

ADR5.3<-ADR5.2$ALLSIGNALS
ADR5.4<-ADR5.3[ADR5.3$`drug code`=="Liraglutide" & ADR5.3$`event effect`=="Suicidal behaviour",]
ADR5.4$`event effect`<-as.character(ADR5.4$`event effect`)

OADR[5,]<-c(ADR5.4[2],ADR5.4[3],ADR5.4[7]-ADR5.4[3],ADR5.4[8]-ADR5.4[3],36172078-ADR5.4[8]+ADR5.4[3],ADR5.4[6])

##Intentional self-injury
ADR6<-glp_disl[glp_disl$`AE lab`=="Intentional self-injury"|glp_disl$`AE lab`=="Rest",]
ADR6$n11[ADR6$`Drug lab`=="Liraglutide"&ADR6$`AE lab`=="Rest"]<-52131-ADR6$n11[ADR6$`Drug lab`=="Liraglutide"&ADR6$`AE lab`=="Intentional self-injury"]
ADR6$n11[ADR6$`Drug lab`=="Rest"&ADR6$`AE lab`=="Rest"]<-36172078-ADR6$n11[ADR6$`Drug lab`=="Rest"&ADR6$`AE lab`=="Intentional self-injury"]
require("PhViD")
ADR6.1<-as.PhViD(data.frame(ADR6),MARGIN.THRES = 1)
ADR6.2<-ROR(ADR6.1,MIN.n11 = 3)

ADR6.3<-ADR6.2$ALLSIGNALS
ADR6.4<-ADR6.3[ADR6.3$`drug code`=="Liraglutide" & ADR6.3$`event effect`=="Intentional self-injury",]
ADR6.4$`event effect`<-as.character(ADR6.4$`event effect`)

OADR[6,]<-c(ADR6.4[2],ADR6.4[3],ADR6.4[7]-ADR6.4[3],ADR6.4[8]-ADR6.4[3],36172078-ADR6.4[8]+ADR6.4[3],ADR6.4[6])

##Self-injurious ideation
ADR7<-glp_disl[glp_disl$`AE lab`=="Self-injurious ideation"|glp_disl$`AE lab`=="Rest",]
ADR7$n11[ADR7$`Drug lab`=="Liraglutide"&ADR7$`AE lab`=="Rest"]<-52131-ADR7$n11[ADR7$`Drug lab`=="Liraglutide"&ADR7$`AE lab`=="Self-injurious ideation"]
ADR7$n11[ADR7$`Drug lab`=="Rest"&ADR7$`AE lab`=="Rest"]<-36172078-ADR7$n11[ADR7$`Drug lab`=="Rest"&ADR7$`AE lab`=="Self-injurious ideation"]
require("PhViD")
ADR7.1<-as.PhViD(data.frame(ADR7),MARGIN.THRES = 1)
ADR7.2<-ROR(ADR7.1,MIN.n11 = 3)

ADR7.3<-ADR7.2$ALLSIGNALS
ADR7.4<-ADR7.3[ADR7.3$`drug code`=="Liraglutide" & ADR7.3$`event effect`=="Self-injurious ideation",]
ADR7.4$`event effect`<-as.character(ADR7.4$`event effect`)

OADR[7,]<-c(ADR7.4[2],ADR7.4[3],ADR7.4[7]-ADR7.4[3],ADR7.4[8]-ADR7.4[3],36172078-ADR7.4[8]+ADR7.4[3],ADR7.4[6])

##Depression suicidal
ADR8<-glp_disl[glp_disl$`AE lab`=="Depression suicidal"|glp_disl$`AE lab`=="Rest",]
ADR8$n11[ADR8$`Drug lab`=="Liraglutide"&ADR8$`AE lab`=="Rest"]<-52131-ADR8$n11[ADR8$`Drug lab`=="Liraglutide"&ADR8$`AE lab`=="Depression suicidal"]
ADR8$n11[ADR8$`Drug lab`=="Rest"&ADR8$`AE lab`=="Rest"]<-36172078-ADR8$n11[ADR8$`Drug lab`=="Rest"&ADR8$`AE lab`=="Depression suicidal"]
require("PhViD")
ADR8.1<-as.PhViD(data.frame(ADR8),MARGIN.THRES = 1)
ADR8.2<-ROR(ADR8.1,MIN.n11 = 3)

ADR8.3<-ADR8.2$ALLSIGNALS
ADR8.4<-ADR8.3[ADR8.3$`drug code`=="Liraglutide" & ADR8.3$`event effect`=="Depression suicidal",]
ADR8.4$`event effect`<-as.character(ADR8.4$`event effect`)

OADR[8,]<-c(ADR8.4[2],ADR8.4[3],ADR8.4[7]-ADR8.4[3],ADR8.4[8]-ADR8.4[3],36172078-ADR8.4[8]+ADR8.4[3],ADR8.4[6])

##Suspected suicide
ADR9<-glp_disl[glp_disl$`AE lab`=="Suspected suicide"|glp_disl$`AE lab`=="Rest",]
ADR9$n11[ADR9$`Drug lab`=="Liraglutide"&ADR9$`AE lab`=="Rest"]<-52131-ADR9$n11[ADR9$`Drug lab`=="Liraglutide"&ADR9$`AE lab`=="Suspected suicide"]
ADR9$n11[ADR9$`Drug lab`=="Rest"&ADR9$`AE lab`=="Rest"]<-36172078-ADR9$n11[ADR9$`Drug lab`=="Rest"&ADR9$`AE lab`=="Suspected suicide"]
require("PhViD")
ADR9.1<-as.PhViD(data.frame(ADR9),MARGIN.THRES = 1)
ADR9.2<-ROR(ADR9.1,MIN.n11 = 3)

ADR9.3<-ADR9.2$ALLSIGNALS
ADR9.4<-ADR9.3[ADR9.3$`drug code`=="Liraglutide" & ADR9.3$`event effect`=="Suspected suicide",]
ADR9.4$`event effect`<-as.character(ADR9.4$`event effect`)

OADR[9,]<-c(ADR9.4[2],ADR9.4[3],ADR9.4[7]-ADR9.4[3],ADR9.4[8]-ADR9.4[3],36172078-ADR9.4[8]+ADR9.4[3],ADR9.4[6])

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)
            
###Estimate information component (IC)
E.1<-(52131*(OADR$a+OADR$b))/36254736
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)
            
OADR$ROR<-round(OADR$ROR, digits = 2)
            
OADRL<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRL, file="OADRL.txt", row.names = F, sep = "\t")

########################################################
########################################################
####   Disproportionality analysis for semaglutide   ###
########################################################
########################################################

glp_diss<-glp_dis[!glp_dis$`Drug lab`=="Liraglutide",]

##Suicidal ideatio
ADR1<-glp_diss[glp_diss$`AE lab`=="Suicidal ideation"|glp_diss$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-30527-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Rest"]<-36172078-ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],36172078-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")
                 
#####Intentional overdose
ADR2<-glp_diss[glp_diss$`AE lab`=="Intentional overdose"|glp_diss$`AE lab`=="Rest",]
ADR2$n11[ADR2$`Drug lab`=="Semaglutide"&ADR2$`AE lab`=="Rest"]<-30527-ADR2$n11[ADR2$`Drug lab`=="Semaglutide"&ADR2$`AE lab`=="Intentional overdose"]
ADR2$n11[ADR2$`Drug lab`=="Rest"&ADR2$`AE lab`=="Rest"]<-36172078-ADR2$n11[ADR2$`Drug lab`=="Rest"&ADR2$`AE lab`=="Intentional overdose"]
require("PhViD")
ADR2.1<-as.PhViD(data.frame(ADR2),MARGIN.THRES = 1)
ADR2.2<-ROR(ADR2.1,MIN.n11 = 3)
                 
ADR2.3<-ADR2.2$ALLSIGNALS
ADR2.4<-ADR2.3[ADR2.3$`drug code`=="Semaglutide" & ADR2.3$`event effect`=="Intentional overdose",]
ADR2.4$`event effect`<-as.character(ADR2.4$`event effect`)
                 
OADR[2,]<-c(ADR2.4[2],ADR2.4[3],ADR2.4[7]-ADR2.4[3],ADR2.4[8]-ADR2.4[3],36172078-ADR2.4[8]+ADR2.4[3],ADR2.4[6])
                             
##Suicide attempt
ADR3<-glp_diss[glp_diss$`AE lab`=="Suicide attempt"|glp_diss$`AE lab`=="Rest",]
ADR3$n11[ADR3$`Drug lab`=="Semaglutide"&ADR3$`AE lab`=="Rest"]<-30527-ADR3$n11[ADR3$`Drug lab`=="Semaglutide"&ADR3$`AE lab`=="Suicide attempt"]
ADR3$n11[ADR3$`Drug lab`=="Rest"&ADR3$`AE lab`=="Rest"]<-36172078-ADR3$n11[ADR3$`Drug lab`=="Rest"&ADR3$`AE lab`=="Suicide attempt"]
require("PhViD")
ADR3.1<-as.PhViD(data.frame(ADR3),MARGIN.THRES = 1)
ADR3.2<-ROR(ADR3.1,MIN.n11 = 3)
                             
ADR3.3<-ADR3.2$ALLSIGNALS
ADR3.4<-ADR3.3[ADR3.3$`drug code`=="Semaglutide" & ADR3.3$`event effect`=="Suicide attempt",]
ADR3.4$`event effect`<-as.character(ADR3.4$`event effect`)
                             
OADR[3,]<-c(ADR3.4[2],ADR3.4[3],ADR3.4[7]-ADR3.4[3],ADR3.4[8]-ADR3.4[3],36172078-ADR3.4[8]+ADR3.4[3],ADR3.4[6])
                                         
##Completed Suicide
ADR4<-glp_diss[glp_diss$`AE lab`=="Completed suicide"|glp_diss$`AE lab`=="Rest",]
ADR4$n11[ADR4$`Drug lab`=="Semaglutide"&ADR4$`AE lab`=="Rest"]<-30527-ADR4$n11[ADR4$`Drug lab`=="Semaglutide"&ADR4$`AE lab`=="Completed suicide"]
ADR4$n11[ADR4$`Drug lab`=="Rest"&ADR4$`AE lab`=="Rest"]<-36172078-ADR4$n11[ADR4$`Drug lab`=="Rest"&ADR4$`AE lab`=="Completed suicide"]
require("PhViD")
ADR4.1<-as.PhViD(data.frame(ADR4),MARGIN.THRES = 1)
ADR4.2<-ROR(ADR4.1,MIN.n11 = 3)
                                         
ADR4.3<-ADR4.2$ALLSIGNALS
ADR4.4<-ADR4.3[ADR4.3$`drug code`=="Semaglutide" & ADR4.3$`event effect`=="Completed suicide",]
ADR4.4$`event effect`<-as.character(ADR4.4$`event effect`)
                                         
OADR[4,]<-c(ADR4.4[2],ADR4.4[3],ADR4.4[7]-ADR4.4[3],ADR4.4[8]-ADR4.4[3],36172078-ADR4.4[8]+ADR4.4[3],ADR4.4[6])
                                                     
###Suicidal behaviour
ADR5<-glp_diss[glp_diss$`AE lab`=="Suicidal behaviour"|glp_diss$`AE lab`=="Rest",]
ADR5$n11[ADR5$`Drug lab`=="Semaglutide"&ADR5$`AE lab`=="Rest"]<-30527-ADR5$n11[ADR5$`Drug lab`=="Semaglutide"&ADR5$`AE lab`=="Suicidal behaviour"]
ADR5$n11[ADR5$`Drug lab`=="Rest"&ADR5$`AE lab`=="Rest"]<-36172078-ADR5$n11[ADR5$`Drug lab`=="Rest"&ADR5$`AE lab`=="Suicidal behaviour"]
require("PhViD")
ADR5.1<-as.PhViD(data.frame(ADR5),MARGIN.THRES = 1)
ADR5.2<-ROR(ADR5.1,MIN.n11 = 3)
                                                     
ADR5.3<-ADR5.2$ALLSIGNALS
ADR5.4<-ADR5.3[ADR5.3$`drug code`=="Semaglutide" & ADR5.3$`event effect`=="Suicidal behaviour",]
ADR5.4$`event effect`<-as.character(ADR5.4$`event effect`)
                                                     
OADR[5,]<-c(ADR5.4[2],ADR5.4[3],ADR5.4[7]-ADR5.4[3],ADR5.4[8]-ADR5.4[3],36172078-ADR5.4[8]+ADR5.4[3],ADR5.4[6])
                                                                 
##Intentional self-injury
ADR6<-glp_diss[glp_diss$`AE lab`=="Intentional self-injury"|glp_diss$`AE lab`=="Rest",]
ADR6$n11[ADR6$`Drug lab`=="Semaglutide"&ADR6$`AE lab`=="Rest"]<-30527-ADR6$n11[ADR6$`Drug lab`=="Semaglutide"&ADR6$`AE lab`=="Intentional self-injury"]
ADR6$n11[ADR6$`Drug lab`=="Rest"&ADR6$`AE lab`=="Rest"]<-36172078-ADR6$n11[ADR6$`Drug lab`=="Rest"&ADR6$`AE lab`=="Intentional self-injury"]
require("PhViD")
ADR6.1<-as.PhViD(data.frame(ADR6),MARGIN.THRES = 1)
ADR6.2<-ROR(ADR6.1,MIN.n11 = 3)
                                                                 
ADR6.3<-ADR6.2$ALLSIGNALS
ADR6.4<-ADR6.3[ADR6.3$`drug code`=="Semaglutide" & ADR6.3$`event effect`=="Intentional self-injury",]
ADR6.4$`event effect`<-as.character(ADR6.4$`event effect`)
                                                                 
OADR[6,]<-c(ADR6.4[2],ADR6.4[3],ADR6.4[7]-ADR6.4[3],ADR6.4[8]-ADR6.4[3],36172078-ADR6.4[8]+ADR6.4[3],ADR6.4[6])
                                                                             
##Self-injurious ideation
ADR7<-glp_diss[glp_diss$`AE lab`=="Self-injurious ideation"|glp_diss$`AE lab`=="Rest",]
ADR7$n11[ADR7$`Drug lab`=="Semaglutide"&ADR7$`AE lab`=="Rest"]<-30527-ADR7$n11[ADR7$`Drug lab`=="Semaglutide"&ADR7$`AE lab`=="Self-injurious ideation"]
ADR7$n11[ADR7$`Drug lab`=="Rest"&ADR7$`AE lab`=="Rest"]<-36172078-ADR7$n11[ADR7$`Drug lab`=="Rest"&ADR7$`AE lab`=="Self-injurious ideation"]
require("PhViD")
ADR7.1<-as.PhViD(data.frame(ADR7),MARGIN.THRES = 1)
ADR7.2<-ROR(ADR7.1,MIN.n11 = 3)
                                                                             
ADR7.3<-ADR7.2$ALLSIGNALS
ADR7.4<-ADR7.3[ADR7.3$`drug code`=="Semaglutide" & ADR7.3$`event effect`=="Self-injurious ideation",]
ADR7.4$`event effect`<-as.character(ADR7.4$`event effect`)
                                                                             
OADR[7,]<-c(ADR7.4[2],ADR7.4[3],ADR7.4[7]-ADR7.4[3],ADR7.4[8]-ADR7.4[3],36172078-ADR7.4[8]+ADR7.4[3],ADR7.4[6])
                                                                                         
##Depression suicidal
ADR8<-glp_diss[glp_diss$`AE lab`=="Depression suicidal"|glp_diss$`AE lab`=="Rest",]
ADR8$n11[ADR8$`Drug lab`=="Semaglutide"&ADR8$`AE lab`=="Rest"]<-30527-ADR8$n11[ADR8$`Drug lab`=="Semaglutide"&ADR8$`AE lab`=="Depression suicidal"]
ADR8$n11[ADR8$`Drug lab`=="Rest"&ADR8$`AE lab`=="Rest"]<-36172078-ADR8$n11[ADR8$`Drug lab`=="Rest"&ADR8$`AE lab`=="Depression suicidal"]
require("PhViD")
ADR8.1<-as.PhViD(data.frame(ADR8),MARGIN.THRES = 1)
ADR8.2<-ROR(ADR8.1,MIN.n11 = 3)
                                                                                         
ADR8.3<-ADR8.2$ALLSIGNALS
ADR8.4<-ADR8.3[ADR8.3$`drug code`=="Semaglutide" & ADR8.3$`event effect`=="Depression suicidal",]
ADR8.4$`event effect`<-as.character(ADR8.4$`event effect`)
                                                                                         
OADR[8,]<-c(ADR8.4[2],ADR8.4[3],ADR8.4[7]-ADR8.4[3],ADR8.4[8]-ADR8.4[3],36172078-ADR8.4[8]+ADR8.4[3],ADR8.4[6])


####no suspected suicides for semaglutide                                                                                                     

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)
                                                                                                                 
###Estimate information component (ICs)
E.1<-(30527*(OADR$a+OADR$b))/36254736
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)
                                                                                                                 
OADR$ROR<-round(OADR$ROR, digits = 2)

OADRS<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRS, file="OADRS.txt", row.names = F, sep = "\t")

#################################################
###################  ###########################
###############        ########################
#############             ######################
###########                 #################### 
#########                      ################
########                          #############
#####                               ###########
####    SEMAGLUTIDE NON-CASES       ############
#####                              ###########
#######                           ################
########                        ##################  
###########                   ###################    
############                ####################
#############             #####################
################       ##########################
##################   #########################
############################################

setwd("~/GLPs")

library(readxl)
###here extract data from sheet "Drugs"
VigiLyze1 <- read_excel("VigiLyze line listing_semaglutide_overall_time.xlsx", sheet = "Drugs")

names(VigiLyze1)[1]<-paste("PrimaryId")
names(VigiLyze1)[2]<-paste("SafetyId")

length(VigiLyze1$PrimaryId)
##now move on to remove duplicates

colnames(VigiLyze1)
require(dplyr)
VigiLyze1$temp2<-apply(VigiLyze1[1:7], 1, function(x) paste(sort(x), collapse = "_"))

VigiLyze1.2<-distinct(VigiLyze1,VigiLyze1$temp2,.keep_all = TRUE)
length(VigiLyze1.2$PrimaryId)

########
#create the array for semaglutide
VigiLyze1.2$temp.s<-array(0,length(VigiLyze1.2$`WHODrug active ingredient variant`))
VigiLyze1.2$temp.s[VigiLyze1.2$`WHODrug active ingredient variant`=="Semaglutide"]<-1

VigiLyze1.3<-VigiLyze1.2[VigiLyze1.2$temp.s==0,]
length(VigiLyze1.3$PrimaryId)

###assess co-medication
table(VigiLyze1.3$`WHODrug active ingredient variant`)
medications.s <- sort(table(VigiLyze1.3$`WHODrug active ingredient variant`),decreasing=T)
length(medications.s)
write.table(medications.s,"medications.s.csv",sep="$",row.names = FALSE)

###antidiabetic agents
VigiLyze1.2$antidiabetic<-array(0,length(VigiLyze1.2$`WHODrug active ingredient variant`))
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Albiglutide"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Alogliptin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Alogliptin benzoate"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Alogliptin benzoate;Metformin hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Alogliptin benzoate;Pioglitazone hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Canagliflozin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Canagliflozin hemihydrate"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Canagliflozin hemihydrate;Metformin hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Canagliflozin;Metformin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Canagliflozin;Metformin hydrochloride"){<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Dapagliflozin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Dapagliflozin propanediol monohydrate"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Dapagliflozin propanediol monohydrate;Metformin hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Dapagliflozin propanediol monohydrate;Saxagliptin hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Dapagliflozin;Metformin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Dapagliflozin;Saxagliptin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Dulaglutide"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Empagliflozin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Empagliflozin;Linagliptin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Empagliflozin;Linagliptin;Metformin hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Empagliflozin;Metformin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Empagliflozin;Metformin hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Ertugliflozin"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Ertugliflozin pidolate"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Ertugliflozin pidolate;Metformin hydrochloride"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Ertugliflozin pidolate;Sitagliptin phosphate monohydrate"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Exenatide"]<-1
  VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Glibenclamide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Glibenclamide;Metformin hydrochloride"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Gliclazide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Gliclazide;Metformin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Glimepiride"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Glipizide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Gliquidone"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Glucagon"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin aspart"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin aspart protamine"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin aspart;Insulin aspart protamine"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin aspart;Insulin aspart protamine (crystalline)"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin aspart;Insulin degludec"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin degludec"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin degludec;Liraglutide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin detemir"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin glargine"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin glargine;Lixisenatide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin glulisine"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin human"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin human injection, isophane"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin human;Insulin human injection, isophane"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin isophane bovine"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin isophane porcine;Insulin porcine"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin lispro"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin lispro aabc"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin lispro;Insulin lispro protamine suspension"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulin porcine"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Insulins and analogues"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Linagliptin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Linagliptin;Metformin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Linagliptin;Metformin hydrochloride"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Liraglutide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Nateglinide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin embonate"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin hydrochloride"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin hydrochloride;Pioglitazone hydrochloride"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin hydrochloride;Saxagliptin hydrochloride"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin hydrochloride;Sitagliptin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin hydrochloride;Sitagliptin Phosphate"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin hydrochloride;Sitagliptin phosphate monohydrate"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin hydrochloride;Vildagliptin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin;Pioglitazone"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Metformin;Sitagliptin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Remogliflozin etabonate"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Rosiglitazone"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Voglibose"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Acarbose"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Saxagliptin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Saroglitazar"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Saxagliptin hydrochloride"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Sitagliptin"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Sitagliptin Phosphate"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Sitagliptin phosphate monohydrate"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Teneligliptin hydrobromide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Tirzepatide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Tolbutamide"]<-1
VigiLyze1.2$antidiabetic[VigiLyze1.2$`WHODrug active ingredient variant`=="Vildagliptin"]<-1

table(VigiLyze1.2$antidiabetic)

###Co-medication with Antidepressants
VigiLyze1.2$antidep<-array(0,length(VigiLyze1.2$`WHODrug active ingredient variant`))
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Agomelatine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Alprazolam;Sertraline"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Amitriptyline"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Amitriptyline hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Amitriptyline hydrochloride;Perphenazine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Bupropion"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Bupropion hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Bupropion hydrochloride;Naltrexone hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Bupropion;Naltrexone"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Buspirone"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Buspirone hydrochloride"){<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Citalopram"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Citalopram hydrobromide"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Citalopram hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Clomipramine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Clomipramine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Cyclobenzaprine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Cyclobenzaprine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Desvenlafaxine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Desvenlafaxine succinate"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Desvenlafaxine succinate monohydrate"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Dosulepin"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Dosulepin hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Doxepin"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Duloxetine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Duloxetine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Escitalopram"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Escitalopram oxalate"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Esketamine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Fluoxetine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Fluoxetine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Fluvoxamine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Imipramine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Levomilnacipran hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Lofepramine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Mianserin"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Mianserin hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Milnacipran hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Mirtazapine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Nortriptyline"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Nortriptyline hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Opipramol"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Paroxetine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Paroxetine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Paroxetine hydrochloride hemihydrate"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Reboxetine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Sertraline"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Sertraline hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Trazodone"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Trazodone hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Trimipramine maleate"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Venlafaxine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Venlafaxine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Vilazodone"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Vilazodone hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Viloxazine hydrochloride"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Vortioxetine"]<-1
VigiLyze1.2$antidep[VigiLyze1.2$`WHODrug active ingredient variant`=="Vortioxetine hydrobromide"]<-1
table(VigiLyze1.2$antidep)

ids<-VigiLyze1.2$PrimaryId

####new disproportionality stratifying for antidepressants
library(readxl)
glp_diss1 <- read_excel("glp_diss1.xlsx")

####SUICIDAL IDEATION stratifying for antidepressants
ADR1<-glp_diss1[glp_diss1$`AE lab`=="Suicidal ideation"|glp_diss1$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-1275-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Rest"]<-36172078-ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],36172078-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)

###Estimation of information component (ICs)
E.1<-(1275*(OADR$a+OADR$b))/36254736
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)

OADR$ROR<-round(OADR$ROR, digits = 2)

OADRad<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRad, file="OADRSad.txt", row.names = F, sep = "\t")


####SUICIDAL IDEATION stratifying for antidiabetics
ADR1<-glp_diss1[glp_diss1$`AE lab`=="Suicidal ideation"|glp_diss1$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-11919-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Rest"]<-36172078-ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],36172078-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)

###Estimate information component (ICs)
E.1<-(11919*(OADR$a+OADR$b))/36254736
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)

OADR$ROR<-round(OADR$ROR, digits = 2)

OADRdb<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRad, file="OADRSad.txt", row.names = F, sep = "\t")

####Disproportionality analysis for suicidal ideation using metformin as control
####metformin
library(readxl)
glp_diss2 <- read_excel("glp_diss2.xlsx")
####SUICIDAL IDEATION
ADR1<-glp_diss2[glp_diss2$`AE lab`=="Suicidal ideation"|glp_diss2$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-30527-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Metformin"&ADR1$`AE lab`=="Rest"]<-122459-ADR1$n11[ADR1$`Drug lab`=="Metformin"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],122459-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)

###Estimate information component (ICs)
E.1<-(30527*(OADR$a+OADR$b))/152986
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)

OADR$ROR<-round(OADR$ROR, digits = 2)

OADRmet<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRad, file="OADRSad.txt", row.names = F, sep = "\t")

#####Disproportionality analysis for suicidal ideation only for females
glp_diss3 <- read_excel("glp_diss3.xlsx")
####View(glp_dis1)

ADR1<-glp_diss3[glp_diss3$`AE lab`=="Suicidal ideation"|glp_diss3$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-17907-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Rest"]<-20710586-17907-ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],20710586-17907-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)

##Estimate information component (ICs)
E.1<-(17907*(OADR$a+OADR$b))/20710586
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)

OADR$ROR<-round(OADR$ROR, digits = 2)

OADRfem<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRad, file="OADRSad.txt", row.names = F, sep = "\t")

#####Disproportionality analysis for suicidal ideation only for males
glp_diss4 <- read_excel("glp_diss4.xlsx")
####View(glp_dis1)

ADR1<-glp_diss4[glp_diss4$`AE lab`=="Suicidal ideation"|glp_diss4$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-11415-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Rest"]<-13388140-11415-ADR1$n11[ADR1$`Drug lab`=="Rest"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],13388140-11415-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)

###Estimate information component (ICs)
E.1<-(11415*(OADR$a+OADR$b))/13388140
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)

OADR$ROR<-round(OADR$ROR, digits = 2)

OADRmale<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRad, file="OADRSad.txt", row.names = F, sep = "\t")


#######disproportionality analysis for suicidal ideation using dapagliflozin as control
library(readxl)
glp_diss5 <- read_excel("glp_diss5.xlsx")

ADR1<-glp_diss5[glp_diss5$`AE lab`=="Suicidal ideation"|glp_diss5$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-30527-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Dapagliflozin"&ADR1$`AE lab`=="Rest"]<-27039-ADR1$n11[ADR1$`Drug lab`=="Dapagliflozin"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],27039-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)

###Estimate information component (ICs)
E.1<-(30527*(OADR$a+OADR$b))/57566
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)

OADR$ROR<-round(OADR$ROR, digits = 2)

OADRdat<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADRad, file="OADRdat.txt", row.names = F, sep = "\t")


#######disproportionality analysis for suicidal ideation using orlistat as control
library(readxl)
glp_diss6 <- read_excel("glp_diss6.xlsx")

ADR1<-glp_diss6[glp_diss6$`AE lab`=="Suicidal ideation"|glp_diss5$`AE lab`=="Rest",]
ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Rest"]<-30527-ADR1$n11[ADR1$`Drug lab`=="Semaglutide"&ADR1$`AE lab`=="Suicidal ideation"]
ADR1$n11[ADR1$`Drug lab`=="Orlistat"&ADR1$`AE lab`=="Rest"]<-31576-ADR1$n11[ADR1$`Drug lab`=="Orlistat"&ADR1$`AE lab`=="Suicidal ideation"]
require("PhViD")
ADR1.1<-as.PhViD(data.frame(ADR1),MARGIN.THRES = 1)
ADR1.2<-ROR(ADR1.1,MIN.n11 = 3)

ADR1.3<-ADR1.2$ALLSIGNALS
ADR1.4<-ADR1.3[ADR1.3$`drug code`=="Semaglutide" & ADR1.3$`event effect`=="Suicidal ideation",]
ADR1.4[2]<-as.character(ADR1.4$`event effect`)

##Create table
OADR<-data.frame(ADR1.4[2],ADR1.4[3],ADR1.4[7]-ADR1.4[3],ADR1.4[8]-ADR1.4[3],31576-ADR1.4[8]+ADR1.4[3],ADR1.4[6])
colnames(OADR)<-c("event effect","a","c","b","d","ROR")

###Estimate confidence intervals (CIs)
s<-sqrt((1/OADR$a)+(1/OADR$b)+(1/OADR$c)+(1/OADR$d))
super_low<-log(OADR$ROR)-(1.96*s)
ROR_low<-round(exp(super_low), digits = 2)
super_high<-log(OADR$ROR)+(1.96*s)
ROR_high<-round(exp(super_high), digits = 2)

###Estimate information component (ICs)
E.1<-(30527*(OADR$a+OADR$b))/(31576+30527)
IC<-round((log2((OADR$a+0.5)/(E.1+0.5))), digits = 2)
IC_LOW.1<-IC-3.3*(OADR$a+0.5)^(-0.5)
IC_LOW<-round((IC_LOW.1-2*(OADR$a+0.5)^(-1.5)), digits = 2)
IC_HIGH.1<-IC+2.4*(OADR$a+0.5)^(-0.5)
IC_HIGH<-round((IC_HIGH.1-0.5*(OADR$a+0.5)^(-1.5)), digits = 2)

OADR$ROR<-round(OADR$ROR, digits = 2)
OADR<-cbind(OADR,ROR_low,ROR_high,IC,IC_LOW,IC_HIGH)
write.table(OADR, file="OADRorl.txt", row.names = F, sep = "\t")

