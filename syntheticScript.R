library(synthpop)
library(tidyverse)
library(readr)
library(cgwtools) #to resave RData files

mycols <- c("darkmagenta", "turquoise")

# Note that for very small numbers the mean averages can differ wildly from the original data. Particularly notable for the much older
# age group of 70-79


# Survey full questions ---------------------------------------------------------------
# Original data is in long form, transposed for copying and used datapasta package to produce tribble

tibble::tribble(
                                                                                                ~RespondentID,
                                                                                                  "StartDate",
                                                                                                    "EndDate",
                                                     "Overall how satisfied are you with your life nowadays?",
                     "Overall to what extent do you feel that the things you do in your life are worthwhile?",
                                                                  "Overall how happy did you feel yesterday?",
                                                                "Overall how anxious did you feel yesterday?",
  "How likely are you to recommend this organisation to friends and family if they needed care or treatment?",
                "How likely are you to recommend this organisation to friends and family as a place to work?",
  "How likely are you to recommend this service/team to friends and family if they needed care or treatment?",
                "How likely are you to recommend this service/team to friends and family as a place to work?",
                                                                       "Are you aware that the Trust offers:",
                                        "Please state which staff occupational group best fits your job role",
                                                                                                 "Job Role 2",
                                                        "Please state which pay band best fits your job role",
                                                                                                 "Pay Band 2",
                                                                             "Which division do you work in?",
                                                                         "Which directorate do you work in?1",
                                                                         "Which directorate do you work in?2",
                                                                                                "Directorate",
                                                                             "Which locality do you work in?",
                                                                                 "Which area do you work in?",
                                                                                       "What is your gender?",
                                                                           "What is your race/ Ethnic group?",
                                                                                                    "Recoded",
                                                                                        "race/Ethnic Group 2",
         "Do you have a disability or a long-term health condition which affects your day to day activities?",
                                                                             "What is your religion/ belief?",
                                                                           "What is your sexual orientation?",
                                                                                          "What is your age?",
                                                                                                "Recoded Age",
                                                                          "What is your relationship status?",
                                                                             "Are you pregnant at this time?",
                                                                  "Have you had a baby in the last 26 weeks?"
  )


# Category groups for ESR

esrCategory <- tibble::tribble(
               ~Category,                             ~Type,
                "Gender",                            "Male",
                "Gender",                          "Female",
                "Gender",                           "Other",
                   "Age",                           "16-29",
                   "Age",                           "30-39",
                   "Age",                           "40-49",
                   "Age",                           "50-59",
                   "Age",                           "60-69",
                   "Age",                           "70-79",
           "Staff Group",                "Admin & Clerical",
           "Staff Group",            "Estates & Ancilliary",
           "Staff Group",                             "AHP",
           "Staff Group",                "Medical & Dental",
           "Staff Group",               "Registered Nurses",
           "Staff Group", "Healthcare Assistants & Support",
               "Banding",                        "Band 1-4",
               "Banding",                        "Band 5-7",
               "Banding",                         "Band 8+",
               "Banding",                         "Medical",
           "Directorate",                             "AMH",
           "Directorate",                            "IAPT",
           "Directorate",                           "MHSOP",
           "Directorate",                   "Spec Services",
           "Directorate",                    "Arnold Lodge",
           "Directorate",                      "Low Secure",
           "Directorate",                         "Rampton",
           "Directorate",                        "Wathwood",
           "Directorate",                  "Offender Heath",
           "Directorate",         "North Notts & Bassetlaw",
           "Directorate",                     "South Notts",
           "Directorate",         "Community Spec Services",
           "Directorate",        "Childrens & Young People",
             "Ethnicity",                           "White",
             "Ethnicity",                             "BME",
             "Ethnicity",                      "Not Stated",
  "Long Term Conditions",             "Long Term Condition",
  "Long Term Conditions",          "No Long Term Condition"
  )



# Import csv files (HIDDEN from GitHub)--------------------------------------------------------
# originally from Survey Monkey but due to GDPR another surveying programme has been procured.

esr <- read_csv("data-raw/ESR.csv")

survey <- read_csv("data-raw/Survey.csv")


# Recode ------------------------------------------------------------------

surveyRecode <- survey %>% 
  rename(Satisfied = 'Overall how satisfied are you with your life nowadays?',
         Worthwhile = 'Overall to what extent do you feel that the things you do in your life are worthwhile?',
         Happy = 'Overall how happy did you feel yesterday?',
         Anxious = 'Overall how anxious did you feel yesterday?',
         Pay = 'Please state which pay band best fits your job role',
         Gender = 'What is your gender?',
         Ethnic = 'What is your race/ Ethnic group?',
         "FandFOrgCare"  = `How likely are you to recommend this organisation to friends and family if they needed care or treatment?`,
         "FandFOrgWork"  = `How likely are you to recommend this organisation to friends and family as a place to work?`,
         "FandFTeamCare" = `How likely are you to recommend this service/team to friends and family if they needed care or treatment?`,
         "FandFTeamWork" = `How likely are you to recommend this service/team to friends and family as a place to work?`,
         "Division"      = `Which division do you work in?`,
         "Gender"        = `What is your gender?`,
         "Ethnic"        = `What is your race/ Ethnic group?`) %>% 
  select(Satisfied,
         Worthwhile,
         Happy,
         Anxious,
         Pay,
         Gender,
         Ethnic,
         FandFOrgCare,
         FandFOrgWork,
         FandFTeamCare,
         FandFTeamWork,
         Division, 
         Gender,    
         Ethnic,
         Recoded,
         `Recoded Age`
         ) %>% 
  mutate(across(where(is.factor), as.character)) 

# Data is in long form, make wide

esrRecode <- esr %>% 
  select(-Category,
         -Age,
         -Directorate) 



# Synthesise --------------------------------------------------------------

synSurvey <- synthpop::syn(surveyRecode)
synESR <- synthpop::syn(esrRecode)

compare(synSurvey, surveyRecode, nrow = 3, ncol = 4, cols = mycols)$plot
compare(synESR, esrRecode, nrow = 3, ncol = 4, cols = mycols)$plot

synthSurvey <- as.data.frame(synSurvey$syn)
synthESRNoCategory <- as.data.frame(synESR$syn)

# Keep categories separate as the synthpop will synthesise if included in the original dataframe

synthESR <- synthESRNoCategory %>% 
  left_join(esrCategory)


# Save to output ----------------------------------------------------------

resave(synthSurvey,
     synthESR,
     file = "data/syntheticSurvey.RData")
