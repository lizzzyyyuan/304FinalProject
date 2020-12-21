#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(janitor)
setwd("/Users/lizyuan/Desktop/final project sta304")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("2019 Canadian Election Study - Online Survey v1.0.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(cps19_yob,
         cps19_gender,
         cps19_province,
         cps19_education,
         cps19_rel_imp,
         cps19_children,
         cps19_marital,
         cps19_income_cat,
         cps19_income_number,
         cps19_bornin_canada,
         cps19_citizenship,
         cps19_votechoice,
         cps19_vote_unlikely,
         cps19_v_advance,
         cps19_vote_lean,
         cps19_2nd_choice)


agebin<-function(r_age){
  a<-0
  while (r_age >=18+a*5){
    a<-a+1
  }
    cat("between",18+(a-1)*5,"and",18+a*5-1)
  }
  


reduced_data<-reduced_data %>% filter(cps19_citizenship=="Canadian citizen")
reduced_data<-reduced_data %>% mutate(age=2020-as.numeric(substr(cps19_yob,1, 4))) %>% filter(age>=18)
reduced_data<-reduced_data %>% mutate(sex_2cats=ifelse((cps19_gender=="A man"),"male","female")) #anyone who is not a male is a female
reduced_data<-reduced_data %>% filter(cps19_province!="Yukon" & cps19_province!="Northwest Territories" & cps19_province!="Nunavut") %>% 
  mutate(province=cps19_province)
reduced_data<-reduced_data %>% mutate(education=ifelse(cps19_education=="No schooling"|cps19_education=="Some elementary school"|cps19_education=="Completed elementary school"
                                                       |cps19_education=="Some secondary/ high school"
                                                       ,"below high school",
                                                       ifelse(cps19_education=="Completed secondary/ high school","high school",
                                                              ifelse(cps19_education=="Some technical, community college, CEGEP, College Classique"
                                                                     |cps19_education=="Completed technical, community college, CEGEP, College Classique","certificate or college",
                                                                     ifelse(cps19_education=="Some university","some university",
                                                                            ifelse(cps19_education=="Bachelor's degree","bachelor's degree",
                                                                                   ifelse(cps19_education=="Master's degree"
                                                                                          |cps19_education=="Professional degree or doctorate","above bachelor's degree",
                                                                                          "others")
                                                                            ) )   )
                                                       )) )
reduced_data<-reduced_data %>% mutate(religion=ifelse(cps19_rel_imp=="Very important","very important",
                                                      ifelse(cps19_rel_imp=="Somewhat important","somewhat important",
                                                             ifelse(cps19_rel_imp=="Not very important","not very important",
                                                                    ifelse(cps19_rel_imp=="Not important at all","not important at all","others")
                                                                    )
                                                             )
                                                      ))
  

reduced_data<-reduced_data %>% mutate(if_children=ifelse(cps19_children=="Yes","yes",
                                                         ifelse(cps19_children=="No","no","others")
                                                         ))



reduced_data<-reduced_data %>% mutate(marital_status=ifelse(cps19_marital=="Married","married",
                                          ifelse(cps19_marital=="Living with a partner","common-law",
                                                 ifelse(cps19_marital=="Divorced","divorced",
                                                        ifelse(cps19_marital=="Separated","seperated",
                                                               ifelse(cps19_marital=="Widowed","widowed",
                                                                      ifelse(cps19_marital=="Never Married","never married","others")
                                                               )))
                                          )))
reduced_data<-reduced_data %>% mutate(marital_status=ifelse(is.na(marital_status),"others",marital_status))
reduced_data<-reduced_data %>% filtere(is.na(marital_status))


reduced_data<-reduced_data %>% mutate(household_income=ifelse(is.na(cps19_income_cat), cps19_income_number,cps19_income_cat))
reduced_data<-reduced_data %>% mutate(born_canada=ifelse(cps19_bornin_canada=="Yes","yes",
                                                         ifelse(cps19_bornin_canada=="No","no","others") ))

#cps19_votechoice
#cps19_vote_unlikely
#cps19_v_advance
#cps19_vote_lean
#cps19_2nd_choice
reduced_data<-reduced_data %>% mutate(li=ifelse(
  cps19_votechoice=="Liberal Party"|cps19_vote_unlikely=="Liberal Party"|cps19_v_advance=="Liberal Party"|cps19_vote_lean=="Liberal Party"
  ,1,0))

reduced_data<-reduced_data %>% mutate(co=ifelse(
  cps19_votechoice=="Conservative Party"|cps19_vote_unlikely=="Conservative Party"|cps19_v_advance=="Conservative Party"|cps19_vote_lean=="Conservative Party"
  ,1,0))

reduced_data<-reduced_data %>% mutate(nd=ifelse(
  cps19_votechoice=="ndp"|cps19_vote_unlikely=="ndp"|cps19_v_advance=="ndp"|cps19_vote_lean=="ndp"
  ,1,0))


reduced_data<-reduced_data %>% mutate(gr=ifelse(
  cps19_votechoice=="Green Party"|cps19_vote_unlikely=="Green Party"|cps19_v_advance=="Green Party"|cps19_vote_lean=="Green Party"
  ,1,0))

reduced_data<-reduced_data %>% mutate(pe=ifelse(
  cps19_votechoice=="People's Party"|cps19_vote_unlikely=="People's Party"|cps19_v_advance=="People's Party"|cps19_vote_lean=="People's Party"
  ,1,0))

reduced_data<-reduced_data %>% mutate(oc=ifelse(
  cps19_votechoice=="Another party (please specify)"|cps19_vote_unlikely=="Another party (please specify)"|cps19_v_advance=="Another party (please specify)"|cps19_vote_lean=="Another party (please specify)"|
  cps19_votechoice=="Don't know/ Prefer not to answer"|cps19_vote_unlikely=="Don't know/ Prefer not to answer"|cps19_v_advance=="Don't know/ Prefer not to answer"|cps19_vote_lean=="Don't know/ Prefer not to answer"
  ,1,0))




reduced_data<-reduced_data %>% mutate(liberal=ifelse(is.na(li),0,1))
reduced_data<-reduced_data %>% mutate(conservative=ifelse(is.na(co),0,1))
reduced_data<-reduced_data %>% mutate(NDP=ifelse(is.na(nd),0,1))
reduced_data<-reduced_data %>% mutate(Green_party=ifelse(is.na(gr),0,1))
reduced_data<-reduced_data %>% mutate(People=ifelse(is.na(pe),0,1))
reduced_data<-reduced_data %>% mutate(other_choice=ifelse(is.na(oc),0,1))
reduced_data<-reduced_data %>% mutate(BQ=ifelse(liberal!=1&conservative!=1&NDP!=1&Green_party!=1&People!=1&other_choice!=1,1,0))

reduced_data<-reduced_data %>% mutate(agebin=ifelse(age>=18&age<=22,"18-22",
                                                    ifelse(age>=23&age<=27,"23-27",
                                                           ifelse(age>=28&age<=32,"28-32",
                                                                  ifelse(age>=33&age<=37,"33-37",
                                                                         ifelse(age>=38&age<=42,"38-42",
                                                                                ifelse(age>=43&age<=47,"43-47",
                                                                                       ifelse(age>=48&age<=52,"48-52",
                                                                                              ifelse(age>=53&age<=57,"53-57",
                                                                                                     ifelse(age>=58&age<=62,"58-62",
                                                                                                            ifelse(age>=63&age<=67,"63-67",
                                                                                                                   ifelse(age>=68&age<=72,"68-72",
                                                                                                                          ifelse(age>=73&age<=77,"73-77",
                                                                                                                                 ifelse(age>=78&age<=82,"78-82",
                                                                                                                                        "older than 83")        )            ) )
                                                                                                            )      )       ))  )     )   )    )     ))


reduced_data<-reduced_data %>%mutate(province=
                      ifelse((cps19_province=="Newfoundland and Labrador"),"Newfoundland and Labrador",
                             
                             ifelse(cps19_province=="Prince Edward Island","Prince Edward Island",
                                    ifelse(cps19_province=="Nova Scotia","Nova Scotia",
                                           ifelse(cps19_province=="New Brunswick","New Brunswick",
                                                  ifelse(cps19_province=="Quebec","Quebec",
                                                         ifelse(cps19_province=="Ontario","Ontario",
                                                                ifelse(cps19_province=="Manitoba","Manitoba",
                                                                       ifelse(cps19_province=="Saskatchewan","Saskatchewan",
                                                                              ifelse(cps19_province=="Alberta","Alberta",
                                                                                     "British Columbia"
                                                                              )) )  )
                                                  ) )))                  ))

reduced_data<-reduced_data %>%select(liberal,
                                     conservative,
                                     NDP,
                                     Green_party,
                                     People,
                                     other_choice,
                                     BQ,
                                     agebin,
                                     sex_2cats,
                                     province,
                                     education,
                                     religion,
                                     if_children,
                                     marital_status,
                                     born_canada) 
 


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/survey_data.csv")

