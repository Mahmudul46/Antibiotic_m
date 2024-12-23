#install package
install.packages("ggstats")
# Load all package
library(tidyverse)
library(gtsummary)
library(gt)
library(readxl)
library(dplyr)
library(likert)

#load data
read_excel("raw_data/AMR_KAP_Data.xlsx")
data <- read_excel("raw_data/AMR_KAP_Data.xlsx")

#load package
library(tidyverse)
library(likert)
library(sjPlot)
library(ISLR)
library(ggplot2)
library(dplyr)
library(ggstats)
library(sjmisc)


#figure-1 data processing

## data convert as a factor
data $`Antibiotic kills the bacteria(Yes)` <- 
  as.factor(data $`Antibiotic kills the bacteria(Yes)`)
data$` Amoxicillin is an antibiotic (Yes)`<- 
  as.factor(data$` Amoxicillin is an antibiotic (Yes)`)
data $` Azithromycin is an antibiotic(Yes)` <-
  as.factor(data $` Azithromycin is an antibiotic(Yes)`)
data$` Paracetamol is an antibiotic(No)` <- 
  as.factor(data$` Paracetamol is an antibiotic(No)`)
data$` Antibiotic kills the virus(No)`<-
  as.factor(data$` Antibiotic kills the virus(No)`)
data$` Antibiotics used to treat diarrhoea(Yes)` <- 
  as.factor(data$` Antibiotics used to treat diarrhoea(Yes)`)
data$` Antibiotics are useful for flu and cough(No)` <-
  as.factor(data$` Antibiotics are useful for flu and cough(No)`)
data $` Antibiotic resistant bacteria are difficult to treat(Yes)` <-
  as.factor(data $` Antibiotic resistant bacteria are difficult to treat(Yes)`)
data$` Antibiotics can cause allergic reactions(Yes)` <- 
  as.factor(data$` Antibiotics can cause allergic reactions(Yes)`)
data $` Antibiotics can kill normal flora(Yes)` <- 
  as.factor(data $` Antibiotics can kill normal flora(Yes)`)
data $` Infectious disease are becoming difficult to treat with antibiotics(Yes)`<-
  as.factor(data $` Infectious disease are becoming difficult to treat with antibiotics(Yes)`)

### Figure-1
select(data,12 :23)
responses1 <- select(data,12 :23)
gglikert(responses1) + labs(x="Percentage",fill= "Response") +
          theme(legend.position = "top")
          ggsave("figure/Figure-1.png", dpi = 300)

          
# Figure-2 
## data convert as a factor
data $ ` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)` <- 
  as.factor(data $ ` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)`)
data $ ` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)` <-
  as.factor(data $ ` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)`)
data $` Antibiotics are safe and hence can be used commonly(Disagree)` <-
  as.factor(data $` Antibiotics are safe and hence can be used commonly(Disagree)`)
data$ ` Sick child is given antibiotics, even there is no indication(Disagree)` <-
  as.factor(data$ ` Sick child is given antibiotics, even there is no indication(Disagree)`)
data $` Antibiotics can improve fever in children(Disagree)` <-
  as.factor(data $` Antibiotics can improve fever in children(Disagree)`)
data $`A child with cold is given antibiotics(Disagree)`<- 
  as.factor(data $`A child with cold is given antibiotics(Disagree)`)
data $ `I stop antibiotics when my child condition improves(Disagree)` <-
  as.factor(data $ `I stop antibiotics when my child condition improves(Disagree)`)
data $  `I reusing the same antibiotics for similar symptoms(Disagree)` <-
  as.factor(data $  `I reusing the same antibiotics for similar symptoms(Disagree)`)
data $ `Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)` <-
  as.factor(data $ `Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)`)
data $ `Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)` <-
  as.factor(data $ `Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)`)

## Figure-2
select(data,24:33)
responses2 <- select(data,24 :33)
gglikert(responses2) + labs(x= "Percentage",fill= "Response")+ theme(legend.position = "top")
ggsave("figure/Figure-2.png", dpi = 300)



# Figure-3 processing
## data convert as a factor
data $` My child should complete a given dose, even he improve after 2 dose(Yes)` <- 
  as.factor(data $` My child should complete a given dose, even he improve after 2 dose(Yes)`)
data $ ` I seek medical advice before giving antibiotic to my children(Yes)` <- as.factor(data $ ` I seek medical advice before giving antibiotic to my children(Yes)`)
data $ ` I like to take antibiotic from pharmacy instead of taking from doctor(No)` <-as.factor(data $ ` I like to take antibiotic from pharmacy instead of taking from doctor(No)`)
data $` I give my children antibiotics(No)` <-as.factor(data $` I give my children antibiotics(No)`)
data $ ` I give my children antibiotics when they get cough(No)` <- as.factor(data $ ` I give my children antibiotics when they get cough(No)`)
data $ ` I check expiring date of antibiotic before giving to children(Yes)` <- as.factor(data $ ` I check expiring date of antibiotic before giving to children(Yes)`)

# figure 3
responses3 <-select(data,34:39)
gglikert(responses3) + labs(x= "Percentage", fill= "Response") + 
  theme(legend.position = "top" )
ggsave("figure/Figure-3.png", dpi = 300)



