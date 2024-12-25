
library(tidyverse)
library(gtsummary)
library(gt)
library(readxl)
data1 <- read_excel("raw_data/AMR_KAP_Data.xlsx")
#table_1
data1 |> 
  select(1:11) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/table1.docx")


#table_2
data1 |> 
  select(41:49) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/table2.docx")

#table-3

#knowledge
library(dplyr)
library(gtsummary)
knowledge <- data1 |>
  select(12:23) |>
  mutate(across(everything(), ~ case_when(
    . == "Don't Know" ~ 0,
    . == "No" ~ 6,	
    . == "Yes" ~ 12,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(
    knowledge_M = mean(c_across(1:12), na.rm = TRUE), 
    knowledge_percent = (knowledge_M / 12) * 100,
    knowledge_level = case_when(
      knowledge_percent < 50 ~ "Poor",
      knowledge_percent >= 50 & knowledge_percent < 80 ~ "Moderate", 
      knowledge_percent >= 80 ~ "Good", 
      TRUE ~ NA_character_
    ) )





#Attitude
attitude <- data1 |> 
  select(24:33) |> 
  mutate(across(everything(), ~ case_when(
    . == "Neutral" ~ 0,
    . == "Disagree" ~ 10,	
    . == "Agree" ~ 5,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(
    attitude_M = mean(c_across(1:10), na.rm = TRUE),            
    attitude_percent = (attitude_M / 10) * 100,                   
    attitude_level = case_when(                                    
      attitude_percent < 50 ~ "Uncertain",
      attitude_percent >= 50 & attitude_percent < 80 ~ "Negative", 
      attitude_percent >= 80 ~ "positive",
      TRUE ~ NA_character_
    )
  )


#Practice
practice <- data1 |>  
  select(34:39) |>  
  mutate(across(everything(), ~ case_when(
    . == "Don't Know" ~ 3,
    . == "No" ~ 6,	
    . == "Yes" ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(
    practice_M = mean(c_across(1:6), na.rm = TRUE),             
    practice_percent = (practice_M / 6) * 100,                     
    practice_level = case_when(                                    
      practice_percent < 80 ~ "Misuse",                            
      practice_percent >= 80 ~ "Good",
      TRUE ~ NA_character_
    )
  )


practice <-data1 |>  select(34:39) |> mutate(across(everything(), ~
                                   case_when(
                                  .== "Don't Know" ~ 3,
                                  .== "No"~6,	
                                  .=="Yes"~0,
                                  TRUE ~ NA_real_
                                   ))) |> 
  rowwise() |> mutate(practice_M = mean(c_across(1:6),na.rm = TRUE),
                      practice_percent = (practice_M / 6) * 100,                   
                      practice_level = case_when(                                    
                        practice_percent < 80 ~ "Misuse",
                        
                        practice_percent >= 80 ~ "Good",
                        TRUE ~ NA_character_
                      ))
#combine data
parents_cha <-cbind(knowledge,attitude,practice)


#table -3 (summary)

# Correct the typo by using 'practice_percent' instead of 'practicee_percent'
practice <- data1 |>  
  select(34:39) |>  
  mutate(across(everything(), ~ case_when(
    . == "Don't Know" ~ 3,
    . == "No" ~ 6,	
    . == "Yes" ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(
    practice_M = mean(c_across(1:6), na.rm = TRUE),             
    practice_percent = (practice_M / 6) * 100,                     
    practice_level = case_when(                                    
      practice_percent < 80 ~ "Misuse",  
      practice_percent >= 80 ~ "Good",
      TRUE ~ NA_character_
    )
  )


# Combine the data
parents_cha <- cbind(knowledge, attitude, practice) 

# Generate a summary table for categorical variables (knowledge, attitude, practice levels)
summary_table <- parents_cha |> 
  select(knowledge_level, attitude_level, practice_level) |> 
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",  
    label = list(  
      knowledge_level ~ "Knowledge Level", 
      attitude_level ~ "Attitude Level",
      practice_level ~ "Practice Level"
    ),
    missing = "ifany"  # Show missing data if any
  )

# Print the summary table
print(summary_table)

summary_table |> 
  as_gt() |> 
  gtsave("tables/table3.docx")







#table-4

library(gtsummary)
library(dplyr)

# Ensure binary transformation for the dependent variable
AM_data <- AM_data |> 
  mutate(
    knowledge_binary = if_else(knowledge_M >= 6, 1, 0, missing = NA_real_)  # Create a binary variable
  )

# Perform univariate logistic regression 
AM_data |> 
  select(1:9, knowledge_binary) |>  
  tbl_uvregression(
    y = knowledge_binary,           
    method = glm,                   
    method.args = list(family = binomial),  
    exponentiate = TRUE             
  ) |> 
  bold_p(t = 0.05) |>               
  as_gt() |> 
  gtsave("tables/table4.docx")

  




##table-5 

# Exclude problematic variables (e.g., Child’s age)
AM_data_cleaned <- AM_data |> 
  select(-`Child’s age (years)`)

# Perform univariate logistic regression
AM_data_cleaned |> 
  select(1:9, attitude_binary) |>  
  tbl_uvregression(
    y = attitude_binary,            
    method = glm,                 
    method.args = list(family = binomial),  
    exponentiate = TRUE             
  ) |> 
  bold_p(t = 0.05) |>             
  as_gt() |> 
  gtsave("tables/table-5.docx")







##table-6

# Load necessary libraries
library(gtsummary)
library(dplyr)


AM_data $`Parent’s age (years)` <- as.factor(AM_data $`Parent’s age (years)`)
AM_data $`Parent’s sex` <-as.factor(AM_data $`Parent’s sex`) 
AM_data $`Parent’s education level` <- as.factor(AM_data $`Parent’s education level`)
AM_data $`Employment status` <- as.factor(AM_data $`Employment status`)
AM_data $`Family type` <- as.factor(AM_data $`Family type`)
AM_data $`Your average household income per month (BDT)` <- as.factor(AM_data $`Your average household income per month (BDT)`)
AM_data $`Child’s sex` <- as.factor(AM_data $`Child’s sex`)
AM_data $`Child’s age (years)` <- as.factor(AM_data $`Child’s age (years)`)
AM_data $`Number of children` <- as.factor(AM_data $`Number of children`)

AM_data |>  select( 1:9,practice_M) |> 
  tbl_uvregression(method = glm,
                   method.args = list(family= binomial,
                                
                                      y= practice_M,
                                      exponentiate= TRUE)) |> 
  bold_p(t=0.05) |> as_gt()



  

