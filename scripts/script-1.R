install.packages("easystats")

                 
# Load package
library(tidyverse)
library(gtsummary)
library(readxl)
library(gt)
library(easystats)

# Load data
read_excel("raw_data/AMR_KAP_Data.xlsx")
data <- read_excel("raw_data/AMR_KAP_Data.xlsx")

# Cheaking missing value
is.na(data)
#cheaking total missing value
sum(is.na(data))
# Remove missing value 
data <- na.omit(data)

# export clean data
write.csv(data,"clean_data/Antibiotic_clean.csv",row.names = FALSE)

# Data applied for tables
data1 <- read_excel("raw_data/AMR_KAP_Data.xlsx")


#Table -1 + Export table
data1 |> select(1:11) |> tbl_summary(statistic = 
                   list(all_continuous()~
                    "{mean} ± {sd}")) |> 
  as_gt() |> gtsave("tables/table1.docx")

# table-2 + Export

data1 |>  select(41:49) |>tbl_summary(statistic = 
                      list(all_continuous()~
                      "{mean} ± {sd}")) |> 
  as_gt() |> gtsave("tables/table2.docx")


# data processing for table -3
#knowlwdge

library(dplyr)

knowledge <- data1 |> 
  select(12:23) |> 
  mutate(across(everything(), ~
                  case_when(
                    . == "Don't Know" ~ 0,
                    . == "No" ~ 0,
                    . == "Yes" ~ 1,
                    TRUE ~ NA_real_
                  ))) |> 
  mutate(
    knowledge__M = apply(as.matrix(across(everything())), 1, function(x) median(x, na.rm = TRUE)) * 100
  ) |> 
  mutate(
    knowledge__M = paste0(knowledge__M, "%"),
    knowledge_level = case_when(
      as.numeric(gsub("%", "", knowledge__M)) < 50 ~ "Poor",
      as.numeric(gsub("%", "", knowledge__M)) >= 50 & as.numeric(gsub("%", "", knowledge__M)) < 80 ~ "Moderate",
      as.numeric(gsub("%", "", knowledge__M)) >= 80 ~ "Good",
      TRUE ~ NA_character_
    )
  )
# Create a summary table for the knowledge levels
knowledge_summary <- table(knowledge$knowledge_level)

# Display the summary table
print(knowledge_summary)
























library(dplyr)
knowledge <-data1 |> select( 12:23) |> mutate(across( everything(), ~
                                                      case_when(
  .== "Don't Know" ~ 0,
  .== "No"~0,	
  .=="Yes"~1,
  TRUE ~ NA_real_
))) |>  mutate(knowledge__M = rowMeans(across(everything()), na.rm = TRUE) * 100) |> 
  mutate(knowledge__M = paste0(knowledge__M, "%"), 
  knowledge_level = case_when( 
    knowledge__M < 50 ~ "Poor",
    knowledge__M >= 50 & knowledge__M < 80 ~ "Modarate",
    knowledge__M >= 80 ~ "Good",
    TRUE ~ NA_character_
  ))


#Attitude
attitute <-data1 |> select(24:33) |> mutate(across( everything(), ~
                          case_when(
                          .== "Neutral" ~ 0,
                          .== "Disagree"~1,	
                          .=="Agree"~0,
                          TRUE ~ NA_real_
                           ))) |>  mutate(attitute__M = rowMeans(across(everything()), na.rm = TRUE) * 100) |> 
  mutate(attitute__M = paste0(attitute__M, "%"),
attitute_level = case_when( 
  attitute__M < 50 ~ "Negative",
  attitute__M >= 50 & attitute__M < 80 ~ "Uncertain",
  attitute__M >= 80 ~ "Positive",
  TRUE ~ NA_character_
))
#Practice
practice <-data1 |>  select(34:39) |> mutate(across(everything(), ~
                                case_when(
                                .== "Don't Know" ~ 0,
                                .== "No"~1,	
                                .=="Yes"~0,
                                TRUE ~ NA_real_
                              ))) |>  
  mutate(practice__M = rowMeans(across(everything()), na.rm = TRUE) * 100) |> 
  mutate(practice__M = paste0(practice__M, "%"),
         practice_level = case_when( 
           practice__M < 80 ~ "Misuse",
           
           practice__M >= 80 ~ "Good",
           TRUE ~ NA_character_
         )) 


#comboine the data
parents_cha <- cbind(knowledge,attitute,practice) 

library(gtsummary)

summary_table <- parents_cha |> 
  select(knowledge_level, attitute_level, practice_level) |> 
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",  
    label = list(  
      knowledge_level ~ "Knowledge Level",
      attitute_level ~ "Attitude Level",
      practice_level ~ "Practice Level"
    ),
    missing = "no"  
  ) 


summary_table |> as_gt()






















##Table-4 try

#table 4
install.packages("broom.helpers")
library(broom.helpers)
knowledge
data1 |> select(1:9, knowledge_M ) |>
  tbl_uvregression(
    method = glm,
    method.args = list(family = binomial),
    y = Knowledge_of_antibiotics,
    exponentiate = T) |> 
  bold_p( t= 0.05)
















# table-4 + export
str(data1)
data1 $`Parent’s age (years)`<-as.factor(data1 $`Parent’s age (years)`)
data1 $`Parent’s sex`<- as.factor(data1 $`Parent’s sex`)
data1 $`Parent’s education level` <- as.factor(data1 $`Parent’s education level`)
data1 $`Employment status`<- as.factor(data1 $`Employment status`)
data1$`Family type` <- as.factor(data1$`Family type`)
data1 $`Your average household income per month (BDT)` <- as.factor(data1 $`Your average household income per month (BDT)`)
data $`Child’s sex`<- as.factor(data $`Child’s sex`)
data1$`Child’s age (years)` <- as.factor(data1$`Child’s age (years)`)
data1$`Number of children`<- as.factor(data1$`Number of children`)


data1 |>  select(1:9,`Knowledge of antibiotics`) |> 
  tbl_uvregression(method = glm,
                   method.args = list(family= binomial),
                   y=`Knowledge of antibiotics`,exponentiate = TRUE)
