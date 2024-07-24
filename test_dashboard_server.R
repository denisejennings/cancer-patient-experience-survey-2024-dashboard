# -------------------------------------------------------------------------
# CPES Dashboard development
# Denise Jennings, April 2024
# Server file
# -------------------------------------------------------------------------

server <- function(input, output, session) {

# Set the drop down menu so only questions relevant to the survey section selected appear
  observeEvent(
    input$survey_section,
    updateSelectizeInput(session, "select_question",
                         choices = unique(data_by_area$question_text[data_by_area$topic == input$survey_section])))

# Set the drop down menu so only report areas relevant to the level selected appear
  observeEvent(
    input$select_level,
    updateSelectizeInput(session, "select_report",
                         choices = unique(data_by_area$report_area_name[data_by_area$level == input$select_level])))

# Set the drop down menu so only questions relevant to the survey section selected appear
  observeEvent(
    input$group_survey_section,
    updateSelectizeInput(session, "group_select_question",
                         choices = unique(data_by_cancer_group$question_text[data_by_cancer_group$topic == input$group_survey_section])))

  # Data by area
  area_data <- reactive({

    data_by_area %>%
      select(question_text, topic, level, report_area_name, response_text_analysis,
             wgt_percent, low, upp, wgt_percent_2018, low_2018, upp_2018, wgt_percent_2015,
             low_2015, upp_2015, scotland) %>%
      filter(topic == input$survey_section &
             question_text == input$select_question &
             level == input$select_level &
             report_area_name %in% input$select_report)
  })

  ###############################################.
  ##  Reactive layout  ----
  ###############################################.

  # Data by area
  output$area <- renderUI({

    area_chart_title <- paste0("2024 Survey Results for", " ", input$select_report, " ", "against Scotland")
    year_chart_title <- paste0("Survey Results for", " ", input$select_report, " ", "over time")

    tagList(
      br(),
      fluidRow(column(9, h4(tags$b(paste0(area_chart_title))))),
               withSpinner(plotlyOutput("plot")),
      fluidRow(column(9, h4(tags$b(paste0(year_chart_title))))),
               withSpinner(plotlyOutput("plot2")),
               column(3, download_data_UI(id = "download_area_data")),
               column(12, dataTableOutput("area_table")), br(), br()
      ) #tagList

      })

  # Data by cancer group
  output$group <- renderUI({

    group_chart_title <- paste0("2024 Survey Results for", " ", input$select_cancer_group, " ", "cancer group against all cancer groups")
    #year_chart_title <- paste0("Survey Results for", " ", input$select_report, " ", "over time")

    tagList(
      br(),
      fluidRow(column(9, h4(tags$b(paste0(group_chart_title))))),
      withSpinner(plotlyOutput("group_plot")),
      #fluidRow(column(9, h4(tags$b(paste0(year_chart_title))))),
      #withSpinner(plotlyOutput("plot2")),
      column(3, download_data_UI(id = "download_cancer_group_data")),
      column(12, dataTableOutput("cancer_group_table")), br(), br()
    ) #tagList

  })
################################################################################

  # Survey results against Scotland
  output$plot <- renderPlotly({


   area <- area_data() %>%
      mutate(response_text_analysis = factor(response_text_analysis,
                                           levels = unique(response_text_analysis)))
      #plot_ly(x = ~response_text_analysis, y = ~wgt_percent*100, type = "bar", name = input$select_report,
              #marker = list(color = "#3F3685"), error_y = list(color = "#1E7F84", array = ~(upp-wgt_percent)*100,
                                                               #arrayminus = ~(wgt_percent-low)*100)) %>%
      group_num <- length(unique(area$report_area_name))

      plot <- plot_ly(data=area, x=~response_text_analysis, type = "bar") %>%

        # location line
        add_trace(y = ~wgt_percent*100, color= ~report_area_name, colors = chart_colours[1:group_num], name = ~report_area_name,
                  symbol= ~report_area_name, marker = list(size= 8))
      #filter(report_area_name == "Aberdeen Royal Infirmary") %>%
     #add_trace(x = ~response_text_analysis, y = ~wgt_percent*100, name = "Scotland",
                #marker = list(color = "#9F9BC2"), error_y = list(color = "#1E7F84", array = ~(upp - wgt_percent)*100,
                               #arrayminus = ~(wgt_percent-low)*100)) #%>%
      #add_trace(x = ~response_text_analysis, y = ~scotland*100, name = "Scotland", marker = list(color = "#9F9BC2"),
                #error_y = list(color = "#1E7F84", array = ~(upp-wgt_percent)*100,
                               #arrayminus = ~(wgt_percent-low)*100)) %>%
      #layout(#title = paste0("2024 Survey Results for", " ", input$select_report, " ", "against Scotland"),
             #xaxis = list(title = ""),
             #yaxis = list(ticksuffix = "%",
                          #title = "",
                          #range = list(0, 100)),
             #bargroupgap = 0.15,
             #legend = list(orientation = "h", x = 0, y = 1.2))
             #margin = list(l = 50, r = 50,
                                  #b = 50, t = 50,
                                  #pad = 20))
  }
  )#plotly end

  output$plot2 <- renderPlotly({


    plot2 <- area_data() %>%
      mutate(response_text_analysis = factor(response_text_analysis,
                                           levels = unique(response_text_analysis))) %>%
      plot_ly(x = ~response_text_analysis, y = ~wgt_percent*100, type = "bar", name = 2024, marker = list(color = "#1E7F84"),
              error_y = list(color = "black", array = ~upp, arrayminus = ~low)) %>%
      add_trace(x = ~response_text_analysis, y = ~wgt_percent_2018*100, type = "bar", name = 2018, marker = list(color = "#8FBFC2"),
                error_y = list(color = "black", array = ~upp_2018, arrayminus = ~low_2018)) %>%
      add_trace(x = ~response_text_analysis, y = ~wgt_percent_2015*100, type = "bar", name = 2015, marker = list(color = "#E9F2F3"),
                error_y = list(color = "black", array = ~upp_2015, arrayminus = ~low_2015)) %>%
      layout(#title = paste0("Survey Results for", " ", input$select_report, " ", "over time"),
             xaxis = list(title = ""),
             yaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100)),
             bargroupgap = 0.25,
             legend = list(orientation = "h", x = 0, y = 1.2))
  }
  )#plotly end

  # Data by cancer group

  cancer_group_data <- reactive({

    data_by_cancer_group %>%
      select(question_text, topic, report_area, response_text_analysis, wgt_percent, wgt_percent_low, wgt_percent_upp, #wgt_percent_2018, low_2018, upp_2018, wgt_percent_2015, low_2015, upp_2015
             all_cancers) %>%
      filter(topic == input$group_survey_section,
             question_text == input$group_select_question,
             report_area == input$select_cancer_group)
  })

  # Survey results against Scotland
  output$group_plot <- renderPlotly({


    cancer_group_data() %>%
      mutate(response_text_analysis = factor(response_text_analysis,
                                           levels = unique(response_text_analysis))) %>%
      plot_ly(x = ~response_text_analysis, y = ~wgt_percent*100, type = "bar", name = input$select_cancer_group,
              marker = list(color = "#3F3685"), error_y = list(color = "black", array = ~wgt_percent_upp, arrayminus = ~wgt_percent_low)) %>%
      add_trace(x = ~response_text_analysis, y = ~all_cancers*100, type = "bar", name = "All cancer groups", marker = list(color = "#9F9BC2")) %>%
      layout(#title = paste0("2024 Survey Results for", " ", input$select_cancer_group, " ", "cancer group against all cancer groups"),
             xaxis = list(title = ""),
             yaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100)),
             bargroupgap = 0.15,
             legend = list(orientation = "h", x = 0, y = 1.2))

  })

  #output$group_plot2 <- renderPlotly({


    #group_plot2 <- cancer_group_data() %>%
      #mutate(response_text_analysis = factor(response_text_analysis,
                                           #levels = unique(response_text_analysis))) %>%
      #plot_ly(x = ~response_text_analysis, y = ~wgt_percent*100, type = "bar", name = 2024,
              #marker = list(color = "#1E7F84"), error_y = list(color = "black", array = ~upp, arrayminus = ~low)) %>%
      #add_trace(x = ~response_text_analysis, y = ~wgt_percent_2018*100, type = "bar", name = 2018, marker = list(color = "#8FBFC2"),
                #error_y = list(color = "black", array = ~upp_2018, arrayminus = ~low_2018)) %>%
      #add_trace(x = ~response_text_analysis, y = ~wgt_percent_2015*100, type = "bar", name = 2015, marker = list(color = "#E9F2F3"),
                 #error_y = list(color = "black", array = ~upp_2015, arrayminus = ~low_2015)) %>%
      #layout(title = paste0("Results for", " ", input$select_cancer_group, " ", "cancer group over time"),
             #xaxis = list(title = ""),
             #yaxis = list(ticksuffix = "%",
                          #title = "",
                          #range = list(0, 100)),
             #bargroupgap = 0.25,
             #legend = list(orientation = "h", x = 0, y = 1.2))
  #}
  #)




  ###############################################
  ## Tables ----
  ###############################################

  # Data by area
  output$area_table <- renderDataTable({

    table <- area_data() %>%
      select(report_area_name, response_text_analysis, wgt_percent, scotland, wgt_percent_2018, wgt_percent_2015) %>%
      mutate(wgt_percent = wgt_percent*100,
             scotland = scotland*100,
             wgt_percent_2018 = wgt_percent_2018*100,
             wgt_percent_2015 = wgt_percent_2015*100) %>%
      rename("Location" = report_area_name,
             "Response option" = response_text_analysis,
             "Response % 2024" = wgt_percent,
             "Response % Scotland 2024" = scotland,
             "Response % 2018" = wgt_percent_2018,
             "Response % 2015" = wgt_percent_2015)

    datatable(table,
              caption = paste0("Responses for"," ", input$select_report, " ", "2024
                               compared to Scotland 2024 and", " ", input$select_report, " ", "in previous years"),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(scrollX = TRUE,
                             dom = 't'))
  })

  # Data by cancer group
  output$cancer_group_table <- renderDataTable({

    table <- cancer_group_data() %>%
      select(report_area, response_text_analysis, wgt_percent, all_cancers) %>% #, wgt_percent_2018, wgt_percent_2015) %>%
      mutate(wgt_percent = wgt_percent*100,
             all_cancers = all_cancers*100) %>% #,
             #wgt_percent_2018 = wgt_percent_2018*100,
             #wgt_percent_2015 = wgt_percent_2015*100) %>%
      rename("Cancer group" = report_area,
             "Response option" = response_text_analysis,
             "Response % 2024" = wgt_percent,
             "Response % all cancer groups" = all_cancers)#,
             #"Response % 2018" = wgt_percent_2018,
             #"Response % 2015" = wgt_percent_2015)

    datatable(table,
              caption = paste0("Responses for", " ", input$select_cancer_group, " ", " cancer group in 2024 compared to all responses and", " ",
                               input$select_cancer_group, " ", "responses in previous years"),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(scrollX = TRUE,
                             dom = 't'))
  })


  ###############################################.
  ## Data downloads ----
  ###############################################.
  # This section prepares the data in each tab for csv download.

  # Area data to be downloaded
  area_download <- reactive({

    area_extract <- area_data() %>%
      select(-c("question_text", "topic")) %>%
      rename("Level" = level,
             "Location" = report_area_name,
             "Response" = response_text_analysis,
             "Response % 2024" = wgt_percent,
             "Response % 2018" = wgt_percent_2018,
             "Response % 2015" = wgt_percent_2015,
             "Scotland response % 2024" = scotland)
  })


  # Cancer groups data to be downloaded
  cancer_group_download <- reactive({

    cancer_group_extract <- cancer_group_data() %>%
    select(-c("question_text", "topic")) %>%
    rename("Cancer group" = report_area,
           "Response" = response_text_analysis,
           "Response % 2024" = wgt_percent,
           #"Response % 2018" = wgt_percent_2018,
           #"Response % 2015" = wgt_percent_2015,
           "All cancer groups response % 2024" = all_cancers)

  })


  download_data_server(id = "download_area_data", data = area_download, filename = "area")
  download_data_server(id = "download_cancer_group_data", data = cancer_group_download, filename = "cancer_group")

} # server end













