# ##############################################################################
# CPES Dashboard development
# Denise Jennings, April 2024
# UI file
# ##############################################################################


# Create main page with tabs and required CSS settings
ui <- #secure_app( #uncomment if needing password protection
  fluidPage(
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(id = "intabset", # id used for jumping between tabs
             div(
               tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"),
                      href= "https://www.publichealthscotland.scot/",
                      target = "_blank"),
               style = "position: relative; top: -12px;"),
             windowTitle = "Scottish Cancer Patient Experience Survey (SCPES)", #title for browser tab
             header = tags$head(includeCSS("styles.css"), # CSS styles
                                HTML("<html lang='en'>"),
                                tags$link(rel="shortcut icon", href="favicon_phs.ico"), #Icon for browser tab
                                #Including Google analytics
                                HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=G-FST2NWF23R"></script>'),
                                includeScript("gtag.js")
             ),

################################################################################
# Home tab ---------------------------------------------------------------------
################################################################################

# Add text and formatting for Home tab
tabPanel(title = "Home", icon = icon("info-circle"), value = "home",
                      # actionButton("browser", "browser"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     radioGroupButtons("home_select",
                                                       choices = home_list, status = "primary",
                                                       direction = "vertical", justified = T)),
                        mainPanel(width = 9,

                                  # About SCPES
                                  conditionalPanel(
                                    condition= 'input.home_select == "about"',
                                    tagList(h3(tags$b("Scottish Cancer Patient Experience Survey (SCPES) 2024")),
                                            #h4(tags$b(2024)),
                                            h5(tags$b(paste0("Publication date: 10th September 2024"))),
                                            p(paste0("The Scottish Cancer Patient Experience Survey (SCPES) is a national postal
                                                     survey jointly funded by the Scottish Government and Macmillan Cancer Support,
                                                     run in partnership with Public Health Scotland.")),
                               p("The survey asks individuals about their experiences of cancer care, from thinking that something
                                 might be wrong, to the support they received after diagnosis and treatment."),
                               p("The survey results inform health care providers and policy makers about people's experiences of
                                 cancer care. They highlight areas of best practice and areas for improvement in cancer care across
                                 Scotland."),
                               p("More information on the survey, including copies of the questionnaire and Technical Report, is
                                 available on the ",
                                         tags$a(href="https://www.gov.scot/collections/scottish-cancer-patient-experience-survey/#2024survey",
                                                "Scottish Government website (external website).", target = "_blank")), br(),

                               h5(tags$b("General enquiries")),
                               p("If you have an enquiry relating to this publication, please email ", tags$a(href="mailto:patientexperience@gov.scot",
                                                                                                              "patientexperience@gov.scot.", target = "_blank"))
                                    ) # tagList
                                  ), # conditionalPanel


                               # Using the dashboard
                               conditionalPanel(
                                 condition= 'input.home_select == "use"',
                                 tagList(h3(tags$b("Using the dashboard")), br(),
                                         p("This interactive dashboard presents the results of the 2024 Scottish Cancer Patient Experience Survey.
                                           The dashboard has three tabs across the top which can be selected."),
                                 p(tags$b("Home: "), "includes sub-sections on the left hand side which provide an introduction
                                           to the SCPES publication, accessibility information, and suggested resources to find out more"),
                                 p(tags$b("Results by area: "), "view the results by Cancer Centre, NHS Board, and Regional Cancer Network,
                                           as well as at Scotland level"),
                                 p(tags$b("Results by cancer group: "), "view the results by cancer group"), br(),
                                 h5(tags$b("Interacting with the dashboard")),
                                 p("On each tab there are drop-down menus which allow the user to to select a question from the survey and update
                                   the charts and data tables for their specific NHS Board, hospital, or subgroup of interest. "), br(),

                               h5(tags$b("Downloading data")),
                               p("There is the option to download data as a .csv file by clicking the 'Download data' button which can be
                                         found above the table on both results tabs."),
                               p("To download an image of a chart, click the camera icon in the top-right
                                         corner of any chart in the dashboard and a .png image file will automatically download.")
                                 ) #tagList
                               ), # condtionalPanel


                               # Further information
                               conditionalPanel(
                                 condition= 'input.home_select == "info"',
                                 tagList(h3(tags$b("Further information")), br(),
                                         h4(tags$b("Publications")),
                                         p("The full report can be found on the  ",
                                           tags$a(href="https://www.gov.scot/collections/scottish-cancer-patient-experience-survey/#2024survey",
                                                  "Scottish Government website (external website).", target="_blank")), br(),

                                 h4(tags$b("SCPES Question Types")),
                                 h5(tags$b("Per cent positive style questions")),
                                 p("Responses to these questions have been categorised as positive, neutral or negative.
                                   For further information about how responses were categorised, please see the ",
                                   tags$a(href="https://www.gov.scot/isbn/9781836016793",
                                          "Analysis and Reporting chapter of the Technical Report.", target="_blank")), br(),

                                 h5(tags$b("Information questions")),
                                 p("Responses to some questions cannot be put on a positive/negative scale. For these questions the percentage
                                   of people selecting each response for each question is shown."), br(),

                                 h5(tags$b("Tick all that apply questions")),
                                 p("For these questions, respondents could tick more than one option and so results cannot be summed."), br(),

                                 h4(tags$b("Methodology")),
                                 h5(tags$b("Weighting")),
                                 p("Results at national, network and NHS Board level have been weighted. Results at Cancer Centre level have not
                                   been weighted. Full details on the weighting methodology applied are provided in  ",
                                   tags$a(href="https://www.gov.scot/isbn/9781836016793",
                                          "Annex D of the Technical Report.", target="_blank")), br(),

                                 h5(tags$b("Comparing results")),
                                 p("There is always a degree of uncertainty in survey results, caused by survey error or random variation.
                                   The results are presented with 95% confidence intervals which describes the range in which the true value of
                                   statistic is likely to be found."),
                                 p("A confidence interval gives an indication of the degree of uncertainty of an estimate and helps to describe
                                   how precise a sample estimate is. Confidence intervals use the standard error to derive a range in which we think
                                   the true value is likely to lie. It specifies a range of values likely to contain the unknown population value. A
                                   95% confidence interval means that, over many repeats of a survey under the same conditions, one would expect that
                                   the confidence interval would contain the true population value 95 times out of 100."),
                                 p("Confidence intervals allow comparisons to be made between statistics from different years' surveys or relating to
                                   different areas. Where confidence intervals overlap, observed differences may be due to survey error or random variation.
                                   Where confidence intervals do not overlap, the observed difference is said to be significant. Further details are available in ",
                                   tags$a(href = "https://www.gov.scot/isbn/9781836016793",
                                          "Annex E of the Technical Report.", target = "_blank")), br(),

                                 h5(tags$b("Variation in results")),
                                 p("When interpreting these data, it is important to remember that some variation in results between different areas should be
                                   expected. Variation in results could be caused by differences in local population characteristics, needs or geographical setting."), br(),

                                 h5(tags$b("Percentages")),
                                 p("All percentages have been rounded to two decimal places and are displayed to the nearest whole number; this means percentages may
                                   not sum to exactly 100 in the charts shown in this dashboard."), br(),

                                 h5(tags$b("Small numbers")),
                                 p("Results for questions based on a small number of responses should be interpreted with care and so the number of responses for
                                   each question has been provided for context. Local level results are only provided for local areas where at least 50 people within
                                   that area responded to the survey and, at all levels of reporting, results for questions with fewer than 20 respondents have been
                                   suppressed. This is to minimise the risk of an individual respondent being identified and to ensure that results are statistically
                                   robust."), br(),

                                 h4(tags$b("Contact us")),
                                 p("Please contact the ", tags$a(href="mailto:patientexperience@gov.scot",
                                                                 "Scottish Goverment survey team"), "if you have any
                                 questions about this publication.")
                                 ) # tagList
                               ), # conditionalPanel

                               # Accessibility
                               conditionalPanel(
                                 condition= 'input.home_select == "accessibility"',
                                 tagList(h3(tags$b("Accessibility")), br(),
                                         p("This website is run by ", tags$a(href="https://www.publichealthscotland.scot/",
                                                                             "Public Health Scotland", target="_blank"),
                                           " Scotland's national organisation for public health. As a new organisation formed
                                on 1 April 2020, Public Health Scotland is currently reviewing its web estate. Public
                                Health Scotland is committed to making its website accessible, in accordance with
                                the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility
                                Regulations 2018. This accessibility statement applies to the dashboard that complements the
                                Scottish Government’s 2024 SCPES publication"),
                                p(tags$a(href="https://mcmw.abilitynet.org.uk/", "AbilityNet (external website)", target="_blank"),
                                  " has advice on making your device easier to use if you have a disability."), br(),

                                h5(tags$b("Compliance status")),
                                p("This site has not yet been evaluated against Web Content Accessibility Guidelines
                                 version 2.1 level AA standard."), br(),

                                h5(tags$b("Reporting any accessibility problems with this website")),
                                p("If you wish to contact us about any accessibility issues you encounter on this
                                 site, please email ", tags$a(href="mailto:phs.patient-exp-surveys@phs.scot", "phs.patient-exp-surveys@phs.scot")), br(),
                                h5(tags$b("Enforcement procedure")),
                                p("The Equality and Human Rights Commission (EHRC) is responsible for enforcing the
                               Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations
                                 2018 (the ‘accessibility regulations’). If you’re not happy with how we respond to your complaint,",
                               tags$a(href="https://www.equalityadvisoryservice.com/", "contact the Equality Advisory and Support Service (EASS) (external website).",
                                      target = "_blank")), br(),

                               h5(tags$b("Preparation of this accessibility statement")),
                               p("This statement was prepared on 10 September 2024. It was last reviewed on 10 September 2024.")
                                 ) # tagList
                               ) # conditonalPanel
                        ) # mainPanel
                      ) # sidebarLayout
             ), # tabPanel


################################################################################
# Results by area tab --------------------------------------------------------
################################################################################
# Add headings and dropdowns for Results by area tab
             tabPanel(title = "Results by area", value = "results_area",
                      wellPanel(#actionButton("browser", "browser"), #used for debugging
                        column(3, div(title ="Select survey section", # tooltip
                                      selectInput("survey_section",
                                                  label = "Step 1. Select survey section:",
                                                  choices = survey_section_list,
                                                  selected =  "Getting diagnosed"),
                                      uiOutput("survey_section_ui"))),
                        column(3, div(title="Select a question",
                                      selectizeInput("select_question",
                                                     label = "Step 2. Select a survey question:",
                                                     choices= "",
                                                     selected = ""),
                               uiOutput("select_question_ui"))),
                        column(3, div(title="Select specific report",
                                      selectizeInput("select_report",
                                                     label = "Step 3. Select specific report area:",
                                                     choices= level_list,
                                                     selected = ""),
                                      uiOutput("select_report_ui"))),
                        column(3, div(title="Select comparator level",
                                      selectizeInput("select_comparator",
                                                     label = "Step 4. Select comparator area:",
                                                     choices= level_list,
                                                     selected = ""),
                                      uiOutput("select_comparator_ui"))),
                      ), # wellPanel
                      mainPanel(width = 12,
                                uiOutput("area")
                      ) # mainPanel
             ), # tabPanel


################################################################################
# Results by cancer group tab-------------------------------------------------
################################################################################
# Add headings and dropdowns for Results by area tab
             tabPanel(title = "Results by cancer group", value = "results_cancer_group",
                      wellPanel(
                        column(3, div(title="Select survey section",
                                      selectInput("group_survey_section",
                                                  label = "Step 1. Select survey section:",
                                                  choices = survey_section_list,
                                                  selected =  "Getting diagnosed"),
                                      uiOutput("group_survey_section_ui"))),
                        column(3, div(title="Select a question",
                                      selectizeInput("group_select_question",
                                                     label = "Step 2. Select a survey question:",
                                                     choices= "",
                                                     selected = ""),
                                      uiOutput("group_select_question_ui"))),
                        column(3, div(title="Select cancer group",
                                      selectizeInput("select_cancer_group",
                                                     label = "Step 3. Select a cancer group:",
                                                     choices= cancer_group,
                                                     selected = cancer_group[1]),
                                      uiOutput("select_cancer_group_ui")))
                      ), # wellPanel
                      mainPanel(width = 12,
                                uiOutput("group")
                      )# mainPanel bracket
             ), # tabPanel
)# navbarPage
)# tagList
 #)  #secureApp
) # fluidPage

################################################################################
# End of script ----------------------------------------------------------------
################################################################################



