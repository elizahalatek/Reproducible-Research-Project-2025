library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")
library("ucminf")
library("ordinal")
library("reshape")
library("generalhoslem")
library("oglmx")
library("aod")
library("brant")
library(tidyverse)
library(stargazer)
library(dplyr)
library(naniar)
library(DescTools)
library(corrplot)
library("performance")
library(car)

Sys.setenv(LANG = "en")
options(scipen = 5)


# --- data preparation ---

job_sat<-read.csv("ESS10SC-subset.csv")

clean_job_sat<-job_sat 
clean_job_sat<-clean_job_sat %>% select( #columns to delete and reasons of delete
  -c("idno", #id number 
     "dweight", #we use other type of weight
     "pweight", #we use other type of weight
     "pspwght",#we use other type of weight
     "name", #title of dataset
     "essround", #ESS round - version of dataset
     "edition", #edition
     "proddate", #prod date
     "edlvhpl", #highest level of education in poland - we have other column with this info 
     "edlvdse", #highest level of education in sweden - we have other column with this info 
     "prob", #Sampling probability
     "stratum", #Sampling stratum
     "psu", #Primary sampling unit
     "yrbrn" #we already have age variable
     ))
clean_job_sat<-clean_job_sat %>% select(-c("crpdwk","pdjobyr","njbspv","emplno")) #too few observations 

# --- filtering data ---
clean_job_sat <- clean_job_sat %>% filter(stfmjob < 66) #ordinal
clean_job_sat <- clean_job_sat %>% filter(happy < 66) #ordinal
clean_job_sat <- clean_job_sat %>% filter(inprdsc < 66) #count
clean_job_sat <- clean_job_sat %>% filter(health < 7) #ordinal
clean_job_sat <- clean_job_sat %>% filter(hlthhmp < 7) #ordinal 
clean_job_sat <- clean_job_sat %>% filter(rlgdgr < 66) #ordinal
clean_job_sat <- clean_job_sat %>% filter(brncntr < 3) #binary
clean_job_sat <- clean_job_sat %>% filter(gndr < 3) #binary
clean_job_sat <- clean_job_sat %>% filter(agea < 666) #discrete
clean_job_sat <- clean_job_sat %>% filter(rshpsts < 66) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(domicil < 7) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(edulvlb < 5555) #ordered/qualitive 
clean_job_sat <- clean_job_sat %>% filter(eduyrs < 66) #discrete
clean_job_sat <- clean_job_sat %>% filter(emplrel < 6) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(wrkctra < 6) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(estsz < 6) #ordinal
clean_job_sat <- clean_job_sat %>% filter(wkdcorga < 66) #ordinal
clean_job_sat <- clean_job_sat %>% filter(wkhtot < 666) #count
clean_job_sat <- clean_job_sat %>% filter(nacer2 < 666) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(tporgwk < 66) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(uemp3m < 6) #binary
clean_job_sat <- clean_job_sat %>% filter(hincsrca < 66) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(hinctnta < 66) #ordinal 
clean_job_sat <- clean_job_sat %>% filter(emprelp < 6) #qualitive - need to be converted to dummy
clean_job_sat <- clean_job_sat %>% filter(atncrse < 7) #binary
clean_job_sat <- clean_job_sat %>% filter(trdawrk < 6) #ordinal
clean_job_sat <- clean_job_sat %>% filter(jbprtfp < 6) #ordinal
clean_job_sat <- clean_job_sat %>% filter(pfmfdjba < 6) #ordinal
clean_job_sat <- clean_job_sat %>% filter(dcsfwrka < 6) #ordinal

clean_job_sat$cntry <- as.factor(clean_job_sat$cntry)

clean_job_sat$rshpsts <- ifelse(clean_job_sat$rshpsts == 1, "Legally married",
                                ifelse(clean_job_sat$rshpsts == 2, "In a legally registered civil union",
                                       ifelse(clean_job_sat$rshpsts == 3, "Living with my partner - not legally recognised",
                                              ifelse(clean_job_sat$rshpsts == 4, "Living with my partner - legally recognised",
                                                     ifelse(clean_job_sat$rshpsts == 5, "Legally separated",
                                                            ifelse(clean_job_sat$rshpsts == 6, "Legally divorced/Civil union dissolved", "N/A"))))))
clean_job_sat$rshpsts <- as.factor(clean_job_sat$rshpsts)

clean_job_sat$domicil <- ifelse(clean_job_sat$domicil == 1, "A big city",
                                ifelse(clean_job_sat$domicil == 2, "Suburbs or outskirts of big city",
                                       ifelse(clean_job_sat$domicil == 3, "Town or small city",
                                              ifelse(clean_job_sat$domicil == 4, "Country village",
                                                     ifelse(clean_job_sat$domicil == 5, "Farm or home in countryside", "N/A")))))

clean_job_sat$domicil <- as.factor(clean_job_sat$domicil)

clean_job_sat$wrkctra <- ifelse(clean_job_sat$wrkctra == 1, "Unlimited",
                                ifelse(clean_job_sat$wrkctra == 2, "Limited",
                                       ifelse(clean_job_sat$wrkctra == 3, "No contract", "N/A")))

clean_job_sat$wrkctra <- as.factor(clean_job_sat$wrkctra)

clean_job_sat$emplrel <- ifelse(clean_job_sat$emplrel == 1, "Employee",
                                ifelse(clean_job_sat$emplrel == 2, "Self-employed",
                                       ifelse(clean_job_sat$emplrel == 3, "Working for own family business", "N/A")))

clean_job_sat$emplrel <- as.factor(clean_job_sat$emplrel)

clean_job_sat <- clean_job_sat %>%
  mutate(
    nacer2 = case_when(
      nacer2 %in% c(1, 2, 3, 5, 6, 7, 8, 9, 41, 42, 43, 49, 50, 51, 52, 53, 80, 81) ~ "Physical Work",
      nacer2 %in% c(10:33) ~ "Manufacturing",
      nacer2 %in% c(58:75, 85:88) ~ "Intellectual Work",
      nacer2 %in% c(35:39, 45:47, 55:57, 77:79, 82, 84, 90:99) ~ "Service & Administration",
      TRUE ~ "Missing/Other"
    )
  )

clean_job_sat$nacer2 <- as.factor(clean_job_sat$nacer2)

clean_job_sat <- clean_job_sat %>%
  mutate(edulvlb = case_when(
    edulvlb %in% c(0, 113) ~ "Primary Education",
    edulvlb %in% c(129, 212, 213, 221, 222, 223) ~ "Lower Secondary Education",
    edulvlb %in% c(229, 311, 312, 313, 321, 322, 323) ~ "Upper Secondary Education",
    edulvlb %in% c(412, 413, 421, 422, 423) ~ "Post-Secondary Non-Tertiary Education",
    edulvlb %in% c(510, 520, 610, 620, 710, 720, 800) ~ "Tertiary Education",
    edulvlb %in% c(5555, 7777, 8888, 9999) ~ "Other/Missing",
    TRUE ~ "Unknown"
  ))

clean_job_sat$edulvlb <- factor(clean_job_sat$edulvlb, 
                                levels = c("Primary Education", 
                                           "Lower Secondary Education", 
                                           "Upper Secondary Education", 
                                           "Post-Secondary Non-Tertiary Education", 
                                           "Tertiary Education", 
                                           "Other/Missing", 
                                           "Unknown"), 
                                ordered = FALSE)

clean_job_sat$tporgwk <- as.factor(clean_job_sat$tporgwk)

clean_job_sat$tporgwk <- ifelse(clean_job_sat$tporgwk == 1, "Central or local government",
                                ifelse(clean_job_sat$tporgwk == 2, "Other public sector (ex. education and health)",
                                       ifelse(clean_job_sat$tporgwk == 3, "A state owned enterprise",
                                              ifelse(clean_job_sat$tporgwk == 4, "A private firm",
                                                     ifelse(clean_job_sat$tporgwk == 5, "Self employed", "N/A")))))

clean_job_sat$tporgwk <- as.factor(clean_job_sat$tporgwk)

clean_job_sat$hincsrca<- ifelse(clean_job_sat$hincsrca == 1, "Wages or salaries",
                                ifelse(clean_job_sat$hincsrca == 2, "Income from self-employment (excluding farming)",
                                       ifelse(clean_job_sat$hincsrca == 3, "Income from farming",
                                              ifelse(clean_job_sat$hincsrca == 4, "Pensions",
                                                     ifelse(clean_job_sat$hincsrca == 5, "Unemployment/redundancy benefit",
                                                            ifelse(clean_job_sat$hincsrca == 6, "Any other social benefits or grants",
                                                                   ifelse(clean_job_sat$hincsrca == 7, "Income from investments, savings etc.",
                                                                          ifelse(clean_job_sat$hincsrca == 8, "Income from other sources", "N/A"))))))))
clean_job_sat$hincsrca <- as.factor(clean_job_sat$hincsrca)

clean_job_sat$emprelp <- ifelse(clean_job_sat$emprelp == 1, "Employee",
                                ifelse(clean_job_sat$emprelp == 2, "Self-employed",
                                       ifelse(clean_job_sat$emprelp == 3, "Working for own family business", "N/A")))

clean_job_sat$emprelp <- as.factor(clean_job_sat$emprelp)

clean_job_sat$health <- factor(clean_job_sat$health, 
                               levels = 1:5,
                               labels = c("Very good", "Good", "Fair", "Bad", "Very Bad"), 
                               ordered = FALSE)

clean_job_sat$trdawrk <- factor(clean_job_sat$trdawrk,
                                levels = 1:5,
                                labels = c("Never", "Hardly ever", "Sometimes", "Often", "Always"), 
                                ordered = FALSE)

clean_job_sat$jbprtfp <- factor(clean_job_sat$jbprtfp,
                                levels = 1:5,
                                labels = c("Never", "Hardly ever", "Sometimes", "Often", "Always"), 
                                ordered = FALSE)

clean_job_sat$pfmfdjba <- factor(clean_job_sat$pfmfdjba,
                                 levels = 1:5,
                                 labels = c("Never", "Hardly ever", "Sometimes", "Often", "Always"), 
                                 ordered = FALSE)

clean_job_sat$dcsfwrka <- factor(clean_job_sat$dcsfwrka,
                                 levels = 1:5,
                                 labels = c("Never", "Hardly ever", "Sometimes", "Often", "Always"), 
                                 ordered = FALSE)


#recoding into dummies
clean_job_sat <- clean_job_sat %>%
  mutate(hlthhmp = case_when(
    hlthhmp %in% c(1, 2) ~ TRUE,
    hlthhmp == 3 ~ FALSE,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(brncntr = case_when(
    brncntr %in% 1 ~ TRUE,
    brncntr == 2 ~ FALSE,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(gndr = case_when(
    gndr %in% 1 ~ 0,
    gndr == 2 ~ 1,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(uemp3m = case_when(
    uemp3m %in% 1 ~ TRUE,
    uemp3m == 2 ~ FALSE,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(atncrse = case_when(
    atncrse %in% 1 ~ TRUE,
    atncrse == 2 ~ FALSE,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(uempla = case_when(
    uempla %in% 0 ~ FALSE,
    uempla == 1 ~ TRUE,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(uempli = case_when(
    uempli %in% 0 ~ FALSE,
    uempli == 1 ~ TRUE,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(rtrd = case_when(
    rtrd %in% 0 ~ FALSE,
    rtrd == 1 ~ TRUE,
  ))

clean_job_sat <- clean_job_sat %>%
  mutate(hswrk = case_when(
    hswrk %in% 0 ~ FALSE,
    hswrk == 1 ~ TRUE,
  ))

# satisfaction levels

categorize_stfmjob <- function(value) {
  if (value %in% 66:99) {
    return(NA) # Exclude missing values
  } else if (value >= 0 & value <= 2) {
    return(1)
  } else if (value >= 3 & value <= 4) {
    return(2)
  } else if (value >= 5 & value <= 6) {
    return(3)
  } else if (value >= 7 & value <= 8) {
    return(4)
  } else if (value >= 9 & value <= 10) {
    return(5)
  }
}

# Apply the function to the stfmjob column
clean_job_sat$stfmjob_grouped <- sapply(clean_job_sat$stfmjob, categorize_stfmjob)

# Convert the numeric groups to descriptive names
group_names <- c("Very Dissatisfied", "Somewhat Dissatisfied", "Neutral", "Somewhat Satisfied", "Very Satisfied")
clean_job_sat$stfmjob_named <- factor(clean_job_sat$stfmjob_grouped, levels = 1:5, labels = group_names)
clean_job_sat$stfmjob_grouped = factor(clean_job_sat$stfmjob_grouped, levels=1:5, ordered=T)

clean_job_sat<-na.omit(clean_job_sat)

# --- end of data preparation ---

# data structure

for (col in 2:ncol(clean_job_sat)) {
  hist(clean_job_sat[,col], main=colnames(clean_job_sat)[col],  xlab=colnames(clean_job_sat)[col])
}

#Countries

ggplot(clean_job_sat, aes(x = "", fill = cntry)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Distribution of Countries")

country_proportions <- clean_job_sat %>%
  group_by(cntry, tporgwk) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(country_proportions, aes(x = tporgwk, y = proportion, fill = cntry)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Proportion", title = "Proportion of Categories by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# happiness

ggplot(clean_job_sat, aes(x = happy)) +
  geom_bar()

# Proportions
country_proportions <- clean_job_sat %>%
  group_by(cntry, happy) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(country_proportions, aes(x = happy, y = proportion, fill = cntry)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Proportion", title = "Happiness by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(clean_job_sat, aes(x = cntry, fill = happy, group = happy)) +
  geom_bar(position = "fill") +
  labs(x = "Country", y = "Percentage", fill = "Happiness") +
  ggtitle("Happiness by Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#--- Proportions charts ---

job_proportions <- clean_job_sat %>%
  group_by(nacer2, stfmjob_named) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(job_proportions, aes(x = nacer2 , y = proportion, fill = stfmjob_named)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Job category", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill=guide_legend(title="Satisifaction level"))

cntry_proportions <- clean_job_sat %>%
  group_by(cntry, stfmjob_named) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(cntry_proportions, aes(x = cntry , y = proportion, fill = stfmjob_named)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill=guide_legend(title="Satisifaction level"))

edu_proportions <- clean_job_sat %>%
  group_by(edulvlb, stfmjob_named) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(edu_proportions, aes(x = edulvlb , y = proportion, fill = stfmjob_named)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Education level", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill=guide_legend(title="Satisifaction level"))

ggplot(clean_job_sat, aes(x = nacer2, fill = stfmjob, group = stfmjob)) +
  geom_bar(position = "fill") +
  labs(x = "Country", y = "Percentage", fill = "Job Satisfaction") +
  ggtitle("Job Satisfaction by Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



#                   -------- initial models --------


# --- model formulas ---

formula_2 <-
  stfmjob_grouped ~  happy + inprdsc + health + hlthhmp + rlgdgr + 
  brncntr + gndr + agea + rshpsts + edulvlb +
  eduyrs + uempla + uempli + rtrd + hswrk + emplrel +
  wrkctra + estsz + wkdcorga + wkhtot + tporgwk +
  uemp3m + hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

# - linear regression -

lmmodel<-lm(formula_2,data=clean_job_sat, weights = anweight)
summary(lmmodel)

# --- ordered logit models ---

ologit <- polr(formula_2, data=clean_job_sat)
ologitr <- ologit.reg(formula_2, data=clean_job_sat)
summary(ologit)
summary(ologitr)

pR2(ologit)
PseudoR2(ologit,which = "all")
# both functions works fine 

# --- ordered probit models --- 

oprobit<-polr(formula_2, data=clean_job_sat,method = "probit")
oprobitr<-oprobit.reg(formula_2, data=clean_job_sat)

summary(oprobit)
summary(oprobitr)

pR2(oprobit)
PseudoR2(oprobit,which = "all")

# --- ordered logit model has lover AIC value than probit model (6467.171 < 6516.821)
# so we will continue with logit model and use function ologit.reg ---

formula_a <-
  stfmjob ~  happy + inprdsc + health + hlthhmp + rlgdgr +
  brncntr + gndr + agea + I(agea^2) + rshpsts + edulvlb +
  eduyrs + uempla + uempli + rtrd + hswrk + emplrel +
  wrkctra + estsz + wkdcorga + wkhtot + tporgwk +
  uemp3m + hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

ologita <- ologit.reg(formula_a, data=clean_job_sat)
summary(ologita) #age^2 is insignificant so we wont continue with this idea


#general-to-specific approach 

# 1 step

formula_2 <-
  stfmjob_grouped ~  happy + inprdsc + health + hlthhmp + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + edulvlb +
  eduyrs  + uempla + uempli + hswrk + emplrel +
  wrkctra + estsz + wkdcorga + wkhtot + tporgwk +
  uemp3m + hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

ologit_2 <- ologit.reg(formula_2, data=clean_job_sat)
summary(ologit_2)
lrtest(ologitr,ologit_2) #p-value = 0.8537 - we can't reject h0: rtrd is insignificant

ologit_lr<-polr(formula_2, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#2 step

formula_3 <-
  stfmjob_grouped ~  happy + inprdsc + health + hlthhmp + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + edulvlb +
  eduyrs + uempla + uempli + emplrel +
  wrkctra + estsz + wkdcorga + wkhtot + tporgwk +
  uemp3m + hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

ologit_3 <- ologit.reg(formula_3, data=clean_job_sat)
summary(ologit_3)
lrtest(ologitr,ologit_3) # p-value = 0.841 - we can't reject h0: hswrk is insignificant

ologit_lr<-polr(formula_3, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#3 step

formula_4 <-
  stfmjob_grouped ~  happy + inprdsc + health + hlthhmp + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + edulvlb +
  eduyrs + uempla + uempli + emplrel +
  wrkctra + estsz + wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

ologit_4 <- ologit.reg(formula_4, data=clean_job_sat)
summary(ologit_4)
lrtest(ologitr,ologit_4) # p-value = 0.6053 - we can't reject h0: uemp3m is insignificant

ologit_lr<-polr(formula_4, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#4 step

formula_5 <-
  stfmjob_grouped ~  happy + inprdsc + health + hlthhmp + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + edulvlb +
  eduyrs + uempla + uempli + emplrel +
  estsz + wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

ologit_5 <- ologit.reg(formula_5, data=clean_job_sat)
summary(ologit_5)
lrtest(ologitr,ologit_5) # p-value = 0.7641 - we can't reject h0: wrkctra is insignificant

ologit_5_lt<-polr(formula_5, data=clean_job_sat)
lipsitz.test(ologit_5_lt)
brant(ologit_5_lt)

#5 step

formula_6 <-
  stfmjob_grouped ~  happy + inprdsc + health + hlthhmp + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  estsz + wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

ologit_6 <- ologit.reg(formula_6, data=clean_job_sat)
summary(ologit_6)
lrtest(ologitr,ologit_6) # p-value = 0.7574 - we can't reject h0: edulvlb is insignificant

ologit_lr<-polr(formula_6, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#6 step

formula_7 <-
  stfmjob_grouped ~  happy + inprdsc + health  + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  estsz + wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil + hincsrca + emprelp

ologit_7 <- ologit.reg(formula_7, data=clean_job_sat)
summary(ologit_7)
lrtest(ologitr,ologit_7) # p-value = 0.8245 - we can't reject h0: hlthhmp insignificant

ologit_lr<-polr(formula_7, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#7 step

formula_8 <-
  stfmjob_grouped ~  happy + inprdsc + health  + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  estsz + wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + nacer2 + domicil  + emprelp

ologit_8 <- ologit.reg(formula_8, data=clean_job_sat)
summary(ologit_8)
lrtest(ologitr,ologit_8) # p-value = 0.441- we can't reject h0: hincsrca insignificant

ologit_lr<-polr(formula_8, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#8 step

formula_9 <-
  stfmjob_grouped ~ happy + inprdsc + health + rlgdgr + 
  brncntr + gndr + agea + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  estsz + wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + domicil + emprelp

ologit_9 <- ologit.reg(formula_9, data=clean_job_sat)
summary(ologit_9)

lrtest(ologitr,ologit_9) # p-value = 0.5599 - we can't reject h0: nacer2  insignificant
ologit_lr<-polr(formula_9, data=clean_job_sat) 
lipsitz.test(ologit_lr)

#9 step

formula_10 <-
  stfmjob_grouped ~  happy + inprdsc + health  + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba + dcsfwrka + domicil  + emprelp

ologit_10 <- ologit.reg(formula_10, data=clean_job_sat)
summary(ologit_10)
lrtest(ologitr,ologit_10) # p-value = 0.5855 - we can't reject h0:   estsz  insignificant

ologit_lr<-polr(formula_10, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#10 step

formula_11 <-
  stfmjob_grouped ~  happy + inprdsc + health  + rlgdgr + 
  brncntr + gndr  + agea + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_11 <- ologit.reg(formula_11, data=clean_job_sat)
summary(ologit_11)
lrtest(ologitr,ologit_11) # p-value =  0.53 - we can't reject h0: dcsfwrka  insignificant

ologit_lr<-polr(formula_11, data=clean_job_sat) 
lipsitz.test(ologit_lr)
brant(ologit_lr)

#11 step

formula_12 <-
  stfmjob_grouped ~  happy + inprdsc + health  + rlgdgr + 
  brncntr + gndr  + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp


ologit_12 <- ologit.reg(formula_12, data=clean_job_sat)
summary(ologit_12)
lrtest(ologitr,ologit_12) # p-value = 0.5238 - we can't reject h0:  agea insignificant

#12 step

formula_13 <-
  stfmjob_grouped ~  happy  + health  + rlgdgr + 
  brncntr + gndr  + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_13 <- ologit.reg(formula_13, data=clean_job_sat)
summary(ologit_13)
lrtest(ologitr,ologit_13) # p-value = 0.5548 - we can't reject h0: inprdsc is insignificant

#13 step

formula_14 <-
  stfmjob_grouped ~  happy  + rlgdgr + 
  brncntr + gndr  + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_14 <- ologit.reg(formula_14, data=clean_job_sat)
summary(ologit_14)
lrtest(ologitr,ologit_14) # p-value = 0.6365 - we can't reject h0:  health insignificant

#14 step

formula_15 <-
  stfmjob_grouped ~  happy  + rlgdgr + gndr  + rshpsts + 
  eduyrs + uempla + uempli + emplrel +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_15 <- ologit.reg(formula_15, data=clean_job_sat)
summary(ologit_15)
lrtest(ologitr,ologit_15) # p-value = 0.583 - we can't reject h0: brncntr is insignificant

#15 step

formula_16 <-
  stfmjob_grouped ~  happy  + rlgdgr + gndr  + rshpsts + 
  eduyrs + uempla  + emplrel +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_16 <- ologit.reg(formula_16, data=clean_job_sat)
summary(ologit_16)
lrtest(ologitr,ologit_16) # p-value = 0.6168- we can't reject h0: uempli insignificant

#16 step

formula_17 <-
  stfmjob_grouped ~  happy  + rlgdgr + gndr  + rshpsts + 
  eduyrs + uempla  +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_17 <- ologit.reg(formula_17, data=clean_job_sat)
summary(ologit_17)
lrtest(ologitr,ologit_17) # p-value = 0.6636 - we can't reject h0: emplrel insignificant 

#17 step

formula_18 <-
  stfmjob_grouped ~  happy  + rlgdgr   + rshpsts + 
  eduyrs + uempla  +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_18 <- ologit.reg(formula_18, data=clean_job_sat)
summary(ologit_18)
lrtest(ologitr,ologit_18) # p-value = 0.542 - we can't reject h0: gndr insignificant 

ologit_lr<-polr(formula_18, data=clean_job_sat) 
lipsitz.test(ologit_lr) # p-value = 0.09171 > 0.05 - fit is okay
brant(ologit_lr)

#18 step

formula_19 <-
  stfmjob_grouped ~  happy  + rlgdgr    + 
  eduyrs + uempla  +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_19 <- ologit.reg(formula_19, data=clean_job_sat)
summary(ologit_19)
lrtest(ologitr,ologit_19) #p-value = 0.5018 - we can't reject h0: rshpsts insignificant 

ologit_lr<-polr(formula_19, data=clean_job_sat) 
lipsitz.test(ologit_lr) #p-value = 0.09171 > 0.05 - fit is okay
brant(ologit_lr)

#19 step 

formula_20 <-
  stfmjob_grouped ~  happy  + rlgdgr    +  uempla  +
  wkdcorga + wkhtot + tporgwk +
  hinctnta + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp

ologit_20 <- ologit.reg(formula_20, data=clean_job_sat)
summary(ologit_20)
lrtest(ologitr,ologit_20) #p-value =  0.4625 - we cant't reject h0: eduyrs insignificant  

ologit_20<-polr(formula_20, data=clean_job_sat) 
lipsitz.test(ologit_20) 
brant(ologit_20) 

#20 step

formula_21 <-
  stfmjob_grouped ~  happy  + rlgdgr + uempla+
  wkdcorga + wkhtot + tporgwk +
  atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp 

ologit_21 <- ologit.reg(formula_21, data=clean_job_sat)
summary(ologit_21)
lrtest(ologitr,ologit_21) #p-value = 0.3975 - we cant't reject h0: hinctnta insignificant  

ologit_21<-polr(formula_21, data=clean_job_sat)
lipsitz.test(ologit_21)
brant(ologit_21)

#21 step

formula_20_int <-
  stfmjob_grouped ~  happy  + rlgdgr +  uempla  +
  wkdcorga + wkhtot + tporgwk + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp + happy:gndr


ologit_20_int <- ologit.reg(formula_20_int, data=clean_job_sat)
summary(ologit_20_int)
lrtest(ologitr,ologit_20_int) #p-value =  0.4625 - we cant't reject h0: eduyrs insignificant  

ologit_20_int<-polr(formula_20_int, data=clean_job_sat) 
lipsitz.test(ologit_20_int) 
brant(ologit_20_int) #omnibus p-value = 1 > 0.05 - Parallel Regression Assumption holds

#probit for compare
probit_20_int <- probit.reg(formula_20_int, data=clean_job_sat)
summary(probit_20_int) # logit is still better

pulkrob.chisq(ologit_20_int, c("pfmfdjba")) 

as.integer(clean_job_sat$stfmjob_grouped)
fitted(ologit_20_int)
logitgof(as.integer(clean_job_sat$stfmjob_grouped),fitted(ologit_20_int))

# --- marginal effects ---

margins.oglmx(ologit_20_int)
marg<-margins.oglmx(ologit_20_int)
marg_df_1<-as.data.frame(marg[1])
marg_df_2<-as.data.frame(marg[2])
marg_df_3<-as.data.frame(marg[3])
marg_df_4<-as.data.frame(marg[4])
marg_df_5<-as.data.frame(marg[5])
round(marg_df_1[1],3)
round(marg_df_2[1],3)
round(marg_df_3[1],3)
round(marg_df_4[1],3)
round(marg_df_5[1],3)
marg_fin<-cbind(round(marg_df_1[1],3),round(marg_df_2[1],3),round(marg_df_3[1],3),round(marg_df_4[1],3),round(marg_df_5[1],3))
marg_fin
colnames(marg_fin)<-c(1:5)
stargazer(marg_fin,type='text',summary = F)

# ---VIF---

formula_20_reg <-
  as.numeric(stfmjob_grouped) ~  happy  + rlgdgr +  uempla  +
  wkdcorga + wkhtot + tporgwk + atncrse  + trdawrk + jbprtfp +
  pfmfdjba  + domicil  + emprelp + happy:gndr

lm_vif<-lm(formula_20_reg,data=clean_job_sat)
summary(lm_vif)
vif(lm_vif)

## --- fianl output ---

stargazer(ologit_20,ologit_21,ologit_20_int,type='text')
summary(ologit_20)
summary(ologit_21)
summary(ologit_20_int)

#END