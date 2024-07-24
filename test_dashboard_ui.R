# -------------------------------------------------------------------------
# CPES Dashboard development
# Denise Jennings, April 2024
# UI file
# -------------------------------------------------------------------------



ui <- fluidPage(
# secure_app( #uncomment if needing password protection
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
#############################################
### Home ----
###############################################.


tabPanel(title = "Home", icon = icon("info-circle"), value = "home",
                      # actionButton("browser", "browser"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     radioGroupButtons("home_select",
                                                       choices = home_list, status = "primary",
                                                       direction = "vertical", justified = T)),

                        mainPanel(width = 9,
                                  # About
                                  conditionalPanel(
                                    condition= 'input.home_select == "about"',
                                    tagList(h3(tags$b("Scottish Cancer Patient Experience Survey (SCPES)")),
                                            h4(tags$b(2024)),
                                            h5(tags$b(paste0("Publication date: September 2024"))),
                                            p(paste0("The Scottish Cancer Patient Experience Survey (SCPES) is a national postal
                                                     survey jointly funded by the Scottish Government and Macmillan Cancer Support.
                                                     It is run in partnership with Public Health Scotland.")),
                               p("The survey asks individuals about their experiences of cancer care, from thinking that something
                                 might be wrong to the support they received after diagnosis and treatment."),
                               p("The survey results inform health care providers and policy makers about people's experiences of
                                 cancer care. They highlight areas of best practice and areas for improvement in cancer care across
                                 Scotland."),
                               p("More information on the survey, including copies of the questionnaire and Technical Report, is
                                 available on the Scottish Government"), br(),

                               h5(tags$b("Contact us")),
                               p("Please contact the Survey team if you have any
                                 questions about this publication or dashboard.")
                                    ) # tagList
                                  ), # conditionalPanel


                               # Using the dashboard
                               conditionalPanel(
                                 condition= 'input.home_select == "use"',
                                 tagList(h3(tags$b("Using the dashboard")), br(),
                                         p("This interactive dashboard presents the results of the 2024 Scottish Cancer Patient Experiencer Survey.
                                           The dashboard has 3 tabs across the top which can be selected:
                                 Home, Results by area, and Results by cancer group."),
                                 p(tags$li(tags$b("Home: "), "Home: includes sub-sections on the left hand side which provide an introduction
                                           to the HSMR publication, accessibility information and suggested resources to find out more.")),
                                 p(tags$li(tags$b("Results by area: "), "view the results by Cancer Centre, NHS Board and Regional Cancer Network,
                                           as well as at Scotland level.")),
                                 p(tags$li(tags$b("Results by cancer group: "), "view the results by cancer group.")), br(),
                                 h5(tags$b("Interacting with the dashboard")),
                                 p("On each tab there are drop-down menus which allow the user to to select a question from the survey and update
                                   the charts and data tables for their specific NHS Board, hospital, or subgroup of interest. "), br(),

                               h5(tags$b("Downloading data")),
                               p(tags$li("There is the option to download data as a csv file by clicking the 'Download data' button which can be
                                         found above the table on each tab.")),
                               p(tags$li("To download an image of a chart, click the camera icon in the top-right
                                         corner of any chart in the dashboard and a png image file will automatically download."))
                                 ) #tagList
                               ), # condtionalPanel


                               # Further information
                               conditionalPanel(
                                 condition= 'input.home_select == "info"',
                                 tagList(h3(tags$b("Further information")), br(),
                                         h5(tags$b("Open data")),
                                         p(tags$li("Open data from this publication is available from the ",
                                           tags$a(href="https://www.opendata.nhs.scot/dataset/hospital-standardised-mortality-ratios",
                                                  "Scottish Health and Social Care Open Data platform (external website).", target="_blank"))),
                                         p(tags$li("The code used to produce the publication can be accessed in the ",
                                           tags$a(href= "https://github.com/Public-Health-Scotland/hsmr", "HSMR GitHub repository (external website).",
                                                  target="_blank"))),
                                         p(tags$li("The code used to produce this dashboard can be accessed in the ",
                                           tags$a(href= "https://github.com/Public-Health-Scotland/hsmr-public-dashboard", "HSMR dashboard GitHub repository (external website).",
                                                  target="_blank"))), br(),

                                 h5(tags$b("Data sources")),
                                 p(tags$li(tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=5",
                                                  "General Acute Inpatient and Day Case - Scottish Morbidity Record (SMR01) (external website).", target="_blank"))),
                                 p(tags$li(tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=3&SubID=13",
                                                  "National Records of Scotland (NRS) - Deaths Data (external website).", target="_blank"))), br(),

                                 h5(tags$b("Data completeness")),
                                 p("Information about the completeness of the SMR01 dataset at the time of this
                                 publication can be found on the ", tags$a(href="https://beta.isdscotland.org/products-and-services/data-management-hospital-activity/smr-completeness/",
                                                                           "SMR completeness webpage (external website).", target="_blank")), br(),

                                 h5(tags$b("Contact us")),
                                 p("Please contact the ", tags$a(href="mailto:phs.qualityindicators@phs.scot",
                                                                 "Quality Indicators team"), "if you have any
                                 questions about this publication or dashboard.")
                                 ) # tagList
                               ), # conditionalPanel

                               # Accessibility
                               conditionalPanel(
                                 condition= 'input.home_select == "accessibility"',
                                 tagList(h3(tags$b("Accessibility")), br(),
                                         p("This website is run by ", tags$a(href="https://www.publichealthscotland.scot/",
                                                                             "Public Health Scotland", target="_blank"),
                                           ", Scotland's national organisation for public health. As a new organisation formed
                                on 1 April 2020, Public Health Scotland is currently reviewing its web estate. Public
                                Health Scotland is committed to making its website accessible, in accordance with
                                the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility
                                Regulations 2018. This accessibility statement applies to the dashboard that accompanies
                                the HSMR quarterly publication."),
                                p(tags$a(href="https://mcmw.abilitynet.org.uk/", "AbilityNet (external website)", target="_blank"),
                                  " has advice on making your device easier to use if you have a disability."), br(),

                                h5(tags$b("Compliance status")),
                                p("This site has not yet been evaluated against Web Content Accessibility Guidelines
                                 version 2.1 level AA standard."), br(),

                                h5(tags$b("Reporting any accessibility problems with this website")),
                                p("If you wish to contact us about any accessibility issues you encounter on this
                                 site, please email ", tags$a(href="mailto:phs.qualityindicators@phs.scot", "phs.qualityindicators@phs.scot", ".")), br(),

                                h5(tags$b("Enforcement procedure")),
                                p("The Equality and Human Rights Commission (EHRC) is responsible for enforcing the
                               Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations
                                 2018 (the ‘accessibility regulations’). If you’re not happy with how we respond to your complaint,",
                               tags$a(href="https://www.equalityadvisoryservice.com/", "contact the Equality Advisory and Support Service (EASS) (external website).",
                                      target = "_blank")), br(),

                               h5(tags$b("Preparation of this accessibility statement")),
                               p("This statement was prepared on 15 June 2022. It was last reviewed on 15 June 2022.")
                                 ) # tagList
                               ) # conditonalPanel
                        ) # mainPanel
                      ) # sidebarLayout
             ), # tabPanel


             ###############################################.
             ### Results by area ----
             ###############################################.

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
                        column(3, div(title="Select report level",
                                      selectizeInput("select_level",
                                                     label = "Step 3. Select report level:",
                                                     choices= level_list,
                                                     selected = level_list[1]),
                                      uiOutput("select_level_ui"))),
                        column(3, div(title="Select specific report",
                                      pickerInput("select_report",
                                                     label = "Step 4. Select specific report:",
                                                     choices= report_list,
                                                     options = list('actions-box' = TRUE),
                                                     selected = "", multiple = T),
                                      uiOutput("select_report_ui")))
                      ), # wellPanel
                      mainPanel(width = 12,
                                uiOutput("area")
                        #plotlyOutput("plot"),
                        #plotlyOutput("plot2"),
                        #dataTableOutput("area_table"),
                        #download_data_UI(id = "download_area_data")
                      ) # mainPanel
             ), # tabPanel


             ###############################################.
             ### Results by cancer group ----
             ##############################################.

             tabPanel(title = "Results by cancer group", value = "results_cancer_group",
                      wellPanel(
                        column(3, div(title="Select survey section", # tooltip
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
                                                     label = "Step 3. Select cancer group:",
                                                     choices= cancer_group,
                                                     selected = cancer_group[1]),
                                      uiOutput("select_cancer_group_ui")))
                      ), # wellPanel
                      mainPanel(width = 12,
                                uiOutput("group")
                        #plotlyOutput("group_plot"),
                        #plotlyOutput("group_plot2"),
                        #dataTableOutput("cancer_group_table"),
                        #download_data_UI(id = "download_cancer_group_data")
                      )# mainPanel
             ), # tabPanel
))# mainPanel bracket

              # tabPanel
   # navbarPage
# tagList
 ) # secureApp





