# ------------------------------------------------------------------------------
# CPES Dashboard development
# Denise Jennings, April 2024
# Global file
# ------------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(janitor)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(stringr)
library(phsstyles)
library(DT)
library(Epi)
library(shinycssloaders)

# 1. Read in data and make adjustments for use in dashboard --------------------
# Read in area data
data_by_area <- readRDS("/conf/bss/CPES/2024/Output/analysis_output/draft_dashboard_output2.rds") %>%
  clean_names() %>%
  mutate(report_area_name = factor(report_area_name, levels = c("Aberdeen Royal Infirmary", "Beatson West of Scotland Cancer Centre",
                                                                "Edinburgh Cancer Centre", "NHS Ayrshire & Arran",
                                                                "NHS Borders", "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley",
                                                                "NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Highland", "NHS Lanarkshire",
                                                                "NHS Lothian", "NHS Orkney", "NHS Shetland", "NHS Tayside", "NHS Western Isles",
                                                                "National Facility", "Ninewells Hospital", "Raigmore Hospital", "NOSCAN", "SCAN", "WOSCAN", "Scotland",
                                                                "No network", "Not allocated"))) %>%
  arrange(report_area_name)


# Create separate Scotland variable for use in comparative plots in dashboard
# First create Scotland subset
scotland_totals <- data_by_area %>%
  filter(report_area_name == "Scotland") %>%
  select(question, response_text_analysis, response_option, wgt_percent) %>%
  rename(scotland = "wgt_percent")

# Join subset with full dataset
data_by_area <- data_by_area %>%
  left_join(scotland_totals, by = c("question", "response_option", "response_text_analysis")) %>%
  mutate(across(c("wgt_percent", "upp", "low", "wgt_percent_2018", "upp_2018", "low_2018",
                  "wgt_percent_2015", "upp_2015", "low_2015", "scotland"), round, 2))

# Read in data for cancer groups
data_by_cancer_group <- readRDS("/conf/bss/CPES/2024/Output/analysis_output/cancer_group_output.rds") %>%
  clean_names() %>%
  mutate(report_area = str_replace_all(report_area," / ", "/"),
         report_area = case_when(report_area == "Scotland" ~ "All cancers",
                                 T ~ report_area))


# Create separate all cancers group variable for use in comparative plots in dashboard
# First create all cancer group subset
all_cancer_groups <- data_by_cancer_group %>%
  filter(report_area == "All cancers") %>%
  select(question, response_text_analysis, response_option, wgt_percent) %>%
  rename(all_cancers = "wgt_percent")

# Combine with full dataset
data_by_cancer_group <- data_by_cancer_group %>%
  left_join(all_cancer_groups, by = c("question", "response_option", "response_text_analysis")) %>%
  mutate(across(c("wgt_percent", "wgt_percent_upp", "wgt_percent_low", "all_cancers"), round, 2))


# 2. Set lists for use in dashboard to ensure they appear in preferred order
# List of sections in Home tab
home_list <- c("About CPES" = "about",
               "Using the dashboard" = "use",
               "Further information" = "info",
               "Accessibility" = "accessibility")

# List of survey sections for drop downs
survey_section_list <- c("Getting diagnosed", "Finding out you had cancer", "Deciding the best treatment for you",
                         "Operations, Radiotherapy and Chemotherapy", "Hospital Care", "Wider Support",
                         "Information and Other Support", "Your Overall Experience", "Other Comments",
                         "About You")


# List of quesitons for drop downs
#question_list <- c("Q1: Before you were told you needed to go to hospital about cancer, how many times did you see a healthcare
                   #professional at your GP Practice about the health problem caused by cancer?", "Q2: How long was it from the time
                   #you first thought something might be wrong with you until you first contacted a healthcare professional at your GP
                   #Practice?", "Q3: How do you feel about the length of time you had to wait before your first appointment with a hospital
                   #doctor?", "Q4: Knowing what you know now, did you have all the information you needed about your test beforehand?",
                   #"Q5: Were the results of your first diagnostic test explained in a way you could understand?", "Q7: When you were first
                   #told that you had cancer, had you been told you could bring a family member or friend with you?", "Q8: How do you feel
                   #about the way you were told you had cancer?", "Q9: Did you understand the explanation of what was wrong with you?", "Q10:
                   #When you were told you had cancer, were you given written information about the type of cancer you had?", "Q12: Before
                   #your treatment started, were your treatment options explained to you?", "Q13: Were you involved in discussions with
                   #healthcare professionals about the right treatment options for you?", "Q14: Were the possible side effects of treatment(s)
                   #explained in a way you could understand?", "Q15: Were you offered practical advice and support in dealing with the side
                   #effects of your treatment(s)?", "Q16: Before you started your treatment(s), were you also told about any side-effects of
                   #the treatment that could affect you in the future rather than straight away?", "Q17: Were you involved as much as you
                   #wanted to be in decisions about your care and treatment?", "Q19: Have you had an operation, such as removal of a tumour
                   #or lump, for your cancer?", "Q20: Knowing what you know now, did you have all the information you needed about your
                   #operation beforehand?", "Q21: After the operation, did a member of staff explain how it had gone in a way you could
                   #understand?", "Q22: Were you given clear written information about what you should or should not do after the operation?",
                   #"Q24: Have you had radiotherapy treatment?", "Q25: Knowing what you know now, did you have all the information you needed
                   #about your radiotherapy treatment beforehand?", "Q26: Have you had chemotherapy treatment?", "Q27: Knowing what you know
                   #now, did you have all the information you needed about your chemotherapy treatment beforehand?", "Q29a: They spent enough
                   #time with me", "Q29b: They listened to me if I had any questions or concerns", "Q29c: They discussed my condition and
                   #treatment with me in a way I could understand", "Q29d: They gave me the opportunity to involve the people that matter to me",
                   #"Q29e: They helped me to feel in control of my treatment/care", "Q29f: They talked in front of me as if I wasn't there",
                   #"Q30: Did the healthcare professionals caring for you call you by your preferred name?", "Q31: Were you given enough privacy
                   #when discussing your condition or treatment?", "Q32: Did a healthcare professional tell you who to contact if you were worried
                   #about your condition or treatment after you left hospital?", "Q35: Were you given the name of a Clinical Nurse Specialist
                   #who would support you through your treatment?", "Q36: How easy or difficult has it been for you to contact your Clinical
                   #Nurse Specialist?", "Q37: When you have had important questions to ask your Clinical Nurse Specialist, how often have you
                   #got answers you could understand?", "Q38: Do you think your GP Practice did everything they could to support you while you
                   #were having cancer treatment?", "Q39: During your cancer treatment, have you been given enough care and support from health
                   #or social services?", "Q40: Once your cancer treatment finished were you given enough care and support from health or social
                   #services?", "Q41: Did healthcare professionals discuss with you or give you information about the impact cancer could have on
                   #your day-to-day activities?", "Q42: Did healthcare professionals give you information about support or self-help groups for
                   #people with cancer?", "Q43: Did healthcare professionals give you information about how to get financial help or any benefits
                   #you might be entitled to?", "Q44: Did healthcare professionals give your family or someone close to you all the information
                   #they needed to help care for you at home?", "Q45: Do you feel you have been supported emotionally and psychologically by
                   #healthcare professionals during your cancer treatment?", "Q46: During your cancer treatment, have you been given information
                   #or support from third sector organisations?", "Q47: Do you feel you have been supported emotionally and psychologically by
                   #third sector organisations during your cancer treatment?", "Q48: Once your cancer treatment finished were you given information
                   #or support from third sector organisations?", "Q50: Have you been given a care plan?", "Q51: Have you been given a written note
                   #of the treatments you have received to treat your cancer?", "Q52: Overall, how easy did you find it to travel to appointments
                   #relating to your cancer care?", "Q53: Which of the following difficulties did you experience when travelling to appointments
                   #relating to your cancer care?", "Q54: Were you able to bring a family member, friend or someone else to your appointments to
                   #support you when you wanted to?", "Q55: Overall, how would you rate the communication around how long appointments relating to
                   #your cancer care would take?", "Q56: Overall, do you feel that you have been treated with dignity and respect by the healthcare
                   #professionals treating you for cancer?", "Q57: Overall, how would you rate the administration of your care?", "Q58: Did the
                   #different people treating and caring for you work well together to give you the best possible care?", "Q59: Overall, how would
                   #you rate your care?", "Q60: Since your diagnosis, has anyone discussed with you whether you would like to take part in cancer
                   #research?")



# List of levels for drop downs
level_list <- c("Scotland", "Network of treatment", "Network of residence", "Cancer centre", "NHS board of treatment", "NHS board of residence")

# List of report areas for drop downs
report_list <- c("Aberdeen Royal Infirmary", "Beatson West of Scotland Cancer Centre", "Edinburgh Cancer Centre", "National Facility",
                 "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley", "NHS Grampian",
                 "NHS Greater Glasgow & Clyde", "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Orkney", "NHS Shetland", "NHS Tayside",
                 "NHS Western Isles", "Ninewells Hospital", "Raigmore Hospital", "NOSCAN", "SCAN", "WOSCAN", "Scotland", "No network",
                 "Not allocated")

# List of cancer groups for drop downs
cancer_group <- c("All cancers", "Breast", "Colorectal/Lower Gastrointestinal", "Gynaecological", "Haematological", "Head and Neck", "Lung",
                  "Oesophageal", "Prostate", "Skin", "Urological", "Less Survivable Cancers", "Other")

# 3. Functions for adding download data buttons with appropriate selections ----

# module ui function
download_data_UI <- function(id) {
  ns <- NS(id)

  downloadButton(ns("data_download"), label = "Download Data")
}


# module server function
download_data_server <- function(id, data, filename) {
  moduleServer(
    id,
    function(input, output, session) {
      output$data_download <- downloadHandler(
        filename = function() {
          paste(filename, ".csv")
        },
        content = function(file) {
          write.csv(data(), file)
        })
    }
  )
}

chart_colours <- as.character(phs_colours()[1:8])
