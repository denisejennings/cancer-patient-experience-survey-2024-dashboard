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
library(shinymanager)
library(plotly)
library(stringr)
library(phsstyles)
library(DT)
library(Epi)
library(shinycssloaders)


# 1. Read in data and make adjustments for use in dashboard --------------------
# Read in area data
data_by_area <- readRDS("dashboard_output_2024.rds") %>%
  clean_names() %>%
  select(-question_text) %>%
  rename(question_text = question_text_dashboard) %>%
  mutate(response_text_dashboard = str_replace(response_text_dashboard,"No, social distancing measures prevented this","No (social distancing measures)"),
         response_text_dashboard = str_replace(response_text_dashboard,"I did not need / want to bring someone with me","I did not need / want to"),
         response_text_dashboard = str_replace(response_text_dashboard,"\\(cost, travel, anything else\\)",""),
         response_text_dashboard = str_replace(response_text_dashboard,"Hospital restrictions prevented me from taking someone with me","Hospital restrictions prevented this"),
         response_text_dashboard = str_replace(response_text_dashboard,"Timing of appointment with respect to travel \\(transport unavailable / rush hour etc.\\)","Timing of appointment")) %>%
  mutate(ci_2024 = if_else(!is.na(wgt_percent),paste0("(", round(wgt_percent_low*100, 2)," - ",round(wgt_percent_upp*100, 2),")"),""),
         ci_2018 = if_else(!is.na(wgt_percent_2018),paste0("(", round(wgt_percent_low_2018*100, 2)," - ",round(wgt_percent_upp_2018*100, 2),")"),""),
         ci_2015 = if_else(!is.na(wgt_percent_2015),paste0("(", round(wgt_percent_low_2015*100, 2)," - ",round(wgt_percent_upp_2015*100, 2),")"),"")) %>%


  # order areas alphabetically (by level)
  mutate(report_area_name = factor(report_area_name, levels = c("Aberdeen Royal Infirmary", "Beatson West of Scotland Cancer Centre",
                                                                "Edinburgh Cancer Centre", "NHS Ayrshire & Arran", "NHS Borders",
                                                                "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley", "NHS Golden Jubilee", "NHS Grampian",
                                                                "NHS Greater Glasgow & Clyde", "NHS Highland", "NHS Lanarkshire", "NHS Lothian",
                                                                "NHS Orkney, NHS Shetland & NHS Western Isles", "NHS Tayside", "Ninewells Hospital", "Raigmore Hospital",
                                                                "NCA – North Cancer Alliance", "SCAN – South East Cancer Network",
                                                                "WoSCAN – West of Scotland Cancer Network", "Scotland"))) %>%
  arrange(report_area_name) %>%
  # add treatment or residence identifier
  mutate(report_area_name = case_when(level == "NHS board of treatment" ~ paste(report_area_name, "(Board of treatment)"),
                                      level == "NHS board of residence" ~ paste(report_area_name, "(Board of residence)"),
                                      level == "Network of treatment" ~ paste(report_area_name, "(Network of treatment)"),
                                      level == "Network of residence" ~ paste(report_area_name, "(Network of residence)"),
                                      T ~ report_area_name),
         question = toupper(question),
         question_text = paste(substr(question, 1,3), question_text, sep = ": "),
         response_option = case_when(question == "Q55" & response_text_dashboard == "Positive" ~ "1",
                                     question == "Q55" & response_text_dashboard == "Neutral" ~ "2",
                                     question == "Q55" & response_text_dashboard == "Negative" ~ "3",
                                     T ~ response_option)) %>%
  mutate(question_text = if_else(str_detect(question_type,"tick") == T, paste0(question_text," (tick all that apply)"),question_text)) %>%
  arrange(question,response_option)

# Read in data for cancer groups
data_by_cancer_group <- readRDS("cancer_group_dashboard_output_2024.rds") %>%
  clean_names() %>%
  select(-question_text) %>%
  rename(question_text = question_text_dashboard) %>%
  mutate(response_text_dashboard = str_replace(response_text_dashboard,"No, social distancing measures prevented this","No (social distancing measures)"),
         response_text_dashboard = str_replace(response_text_dashboard,"I did not need / want to bring someone with me","I did not need / want to"),
         response_text_dashboard = str_replace(response_text_dashboard,"\\(cost, travel, anything else\\)",""),
         response_text_dashboard = str_replace(response_text_dashboard,"Hospital restrictions prevented me from taking someone with me","Hospital restrictions prevented this"),
         response_text_dashboard = str_replace(response_text_dashboard,"Timing of appointment with respect to travel \\(transport unavailable / rush hour etc.\\)","Timing of appointment")) %>%
  mutate(ci_2024 = if_else(!is.na(wgt_percent),paste0("(", round(wgt_percent_low*100, 2)," - ",round(wgt_percent_upp*100, 2),")"),""),
         ci_2018 = if_else(!is.na(wgt_percent_2018),paste0("(", round(wgt_percent_low_2018*100, 2)," - ",round(wgt_percent_upp_2018*100, 2),")"),""),
         ci_2015 = if_else(!is.na(wgt_percent_2015),paste0("(", round(wgt_percent_low_2015*100, 2)," - ",round(wgt_percent_upp_2015*100, 2),")"),"")) %>%
  mutate(report_area = case_when(report_area == "Scotland" ~ "All",
                                 T ~ report_area),
         question = toupper(question),
         question_text = paste(substr(question, 1,3), question_text, sep = ": "),
         # fix order of Q55 response options
         response_option = case_when(question == "Q55" & response_text_dashboard == "Positive" ~ "1",
                                     question == "Q55" & response_text_dashboard == "Neutral" ~ "2",
                                     question == "Q55" & response_text_dashboard == "Negative" ~ "3",
                                     T ~ response_option))%>%
  mutate(question_text = if_else(str_detect(question_type,"tick") == T, paste0(question_text," (tick all that apply)"),question_text)) %>%
  arrange(question,response_option)

# Create separate all cancers group variable for use in comparative plots in dashboard
# First create all cancer group subset
all_cancer_groups <- data_by_cancer_group %>%
  filter(report_area == "All") %>%
  select(question, response_text_dashboard, response_option, wgt_percent, wgt_percent_low, wgt_percent_upp) %>%
  rename(wgt_percent_all_cancers = "wgt_percent",
         wgt_percent_upp_all_cancers = "wgt_percent_upp",
         wgt_percent_low_all_cancers = "wgt_percent_low") %>%
  mutate(ci_all_cancers = if_else(!is.na(wgt_percent_all_cancers),paste0("(", round(wgt_percent_low_all_cancers*100, 2)," - ",
                                                                         round(wgt_percent_upp_all_cancers*100, 2),")"),""))

# Combine with full dataset
data_by_cancer_group <- data_by_cancer_group %>%
  left_join(all_cancer_groups, by = c("question", "response_option", "response_text_dashboard"))


# 2. Set lists for use in dashboard to ensure they appear in preferred order----
# List of sections in Home tab
home_list <- c("About SCPES" = "about",
               "Using the dashboard" = "use",
               "Further information" = "info",
               "Accessibility" = "accessibility")

# List of survey sections for dropdowns
survey_section_list <- c("Getting diagnosed", "Finding out you had cancer", "Deciding the best treatment for you",
                         "Operations, Radiotherapy and Chemotherapy", "Hospital Care", "Wider Support",
                         "Information and Other Support", "Your Overall Experience")

# List of levels for dropdowns
level_list <- list(
  "Scotland" = c("Scotland"),
  "Network of residence" = c("NCA – North Cancer Alliance (Network of residence)", "SCAN – South East Cancer Network (Network of residence)",
                             "WoSCAN – West of Scotland Cancer Network (Network of residence)"),
  "Network of treatment" = c("NCA – North Cancer Alliance (Network of treatment)", "SCAN – South East Cancer Network (Network of treatment)",
                             "WoSCAN – West of Scotland Cancer Network (Network of treatment)"),
  "NHS board of residence" = c("NHS Ayrshire & Arran (Board of residence)", "NHS Borders (Board of residence)", "NHS Dumfries & Galloway (Board of residence)",
                               "NHS Fife (Board of residence)", "NHS Forth Valley (Board of residence)", "NHS Grampian (Board of residence)",
                               "NHS Greater Glasgow & Clyde (Board of residence)", "NHS Highland (Board of residence)", "NHS Lanarkshire (Board of residence)",
                               "NHS Lothian (Board of residence)", "NHS Orkney, NHS Shetland & NHS Western Isles (Board of residence)", "NHS Tayside (Board of residence)"),
  "NHS board of treatment" = c("NHS Ayrshire & Arran (Board of treatment)", "NHS Borders (Board of treatment)", "NHS Dumfries & Galloway (Board of treatment)",
                               "NHS Fife (Board of treatment)", "NHS Forth Valley (Board of treatment)", "NHS Golden Jubilee (Board of treatment)", "NHS Grampian (Board of treatment)",
                               "NHS Greater Glasgow & Clyde (Board of treatment)", "NHS Highland (Board of treatment)", "NHS Lanarkshire (Board of treatment)",
                               "NHS Lothian (Board of treatment)", "NHS Tayside (Board of treatment)"),
  "Cancer centre" = c("Aberdeen Royal Infirmary", "Beatson West of Scotland Cancer Centre", "Edinburgh Cancer Centre", "Ninewells Hospital", "Raigmore Hospital"))


# List of cancer groups for dropdowns
cancer_group <- c("All", "Breast", "Colorectal / Lower Gastrointestinal", "Gynaecological", "Haematological", "Head and Neck", "Lung",
                  "Oesophageal", "Prostate", "Skin", "Urological", "Less Survivable Cancers", "Other")

# 3. Function/module for adding download data buttons with appropriate selections ----

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

# Set colours
ci_colour <- "#C73918"
plot1_colours <- c("#3F3685", "#9F9BC2")

################################################################################
# End of script ----------------------------------------------------------------
################################################################################


















