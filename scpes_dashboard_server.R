# ------------------------------------------------------------------------------
# CPES Dashboard development
# Denise Jennings, April 2024
# Server file
# ------------------------------------------------------------------------------

# Set password protection
#credentials <- readRDS("admin/credentials.rds")


################################################################################
# Reactive controls  -----------------------------------------------------------
################################################################################

server <- function(input, output, session) {

# Shinymanager Auth
#res_auth <- secure_server(check_credentials = check_credentials(credentials))

  # Set the dropdown menu so only questions relevant to the survey section selected appear
  observeEvent(
    input$survey_section,
    updateSelectizeInput(session, "select_question",
                         choices = unique(data_by_area$question_text[data_by_area$topic == input$survey_section])))

  # Set the dropdown menu so only questions relevant to the survey section selected appear
  observeEvent(
    input$group_survey_section,
    updateSelectizeInput(session, "group_select_question",
                         choices = unique(data_by_cancer_group$question_text[data_by_cancer_group$topic == input$group_survey_section])))


################################################################################
# Create the reactive datasets  ------------------------------------------------
################################################################################

# Data by area
  area_data <- reactive({

    data_by_area %>%
      select(question, question_text, topic, level, report_area_name, response_text_dashboard, n_response,
             wgt_percent, wgt_percent_low, wgt_percent_upp, wgt_percent_2018, wgt_percent_low_2018, wgt_percent_upp_2018, wgt_percent_2015,
             wgt_percent_low_2015, wgt_percent_upp_2015) %>%
      filter(topic == input$survey_section,
             question_text == input$select_question,
             report_area_name %in% c(input$select_report,
                                     input$select_comparator))
  })

# Data for use in area data tab by year
  time_data <- reactive({

    data_by_area %>%
      select(question, question_text, topic, level, report_area_name, response_text_dashboard, n_response,
             wgt_percent, wgt_percent_low, wgt_percent_upp, wgt_percent_2018, wgt_percent_low_2018, wgt_percent_upp_2018, wgt_percent_2015,
             wgt_percent_low_2015, wgt_percent_upp_2015) %>%
      filter(topic == input$survey_section,
             question_text == input$select_question,
             report_area_name == input$select_comparator)
  })

# Data by cancer group

  cancer_group_data <- reactive({

    data_by_cancer_group %>%
      select(question, question_text, topic, report_area, response_text_dashboard, n_response, wgt_percent, wgt_percent_low, wgt_percent_upp, wgt_percent_all_cancers,
             wgt_percent_low_all_cancers, wgt_percent_upp_all_cancers, wgt_percent_2018, wgt_percent_low_2018, wgt_percent_upp_2018, wgt_percent_2015,
             wgt_percent_low_2015, wgt_percent_upp_2015) %>%
      filter(topic == input$group_survey_section,
             question_text == input$group_select_question,
             report_area == input$select_cancer_group)
  })

  #Data by cancer group solely for use in the data table
  cancer_group_data_table <- reactive({

    data_by_cancer_group %>%
      select(question, question_text, topic, report_area, response_text_dashboard, n_response, wgt_percent, wgt_percent_low, wgt_percent_upp, wgt_percent_all_cancers,
             wgt_percent_low_all_cancers, wgt_percent_upp_all_cancers, wgt_percent_2018, wgt_percent_low_2018, wgt_percent_upp_2018, wgt_percent_2015,
             wgt_percent_low_2015, wgt_percent_upp_2015) %>%
      filter(topic == input$group_survey_section,
             question_text == input$group_select_question,
             report_area %in% c(input$select_cancer_group,"All cancers"))
  })
################################################################################
#  Create info button for confidence interval description ----------------------
################################################################################

# Text for 'What are confidence intervals' info button
  ci_modal <- modalDialog(
    p(tags$b("Confidence intervals: "), "The red lines at the end of the bars in the charts represent 95% confidence intervals. There is always a degree of
      uncertainty in survey results, caused by survey error or random variation. The confidence interval describes the range in which the
      true value of statistic is likely to be found."),
    p(tags$b("Interpretation: "), "Confidence intervals allow comparisons to be made between statistics from different years' surveys, or
      relating to different areas.  Where confidence intervals overlap, observed differences may be due to survey error or random variation.
      Where confidence intervals do not overlap, the observed difference is said to be significant."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
  # Link action button click to modal launch
  observeEvent(input$ci_info, { showModal(ci_modal) })

# Repeat steps for data by cancer group tab
  group_ci_modal <- modalDialog(
    p(tags$b("Confidence intervals: "), "The red lines at the end of the bars in the charts represent 95% confidence intervals. There is always a degree of
      uncertainty in survey results, caused by survey error or random variation. The confidence interval describes the range in which the
      true value of statistic is likely to be found."),
    p(tags$b("Interpretation: "), "Confidence intervals allow comparisons to be made between statistics from different years' surveys, or
      relating to different areas.  Where confidence intervals overlap, observed differences may be due to survey error or random variation.
      Where confidence intervals do not overlap, the observed difference is said to be significant."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
  # Link action button click to modal launch
  observeEvent(input$group_ci_info, { showModal(group_ci_modal) })


################################################################################
#  Reactive layout  ------------------------------------------------------------
################################################################################

# Set layout for Data by area tab
  output$area <- renderUI({

    area_chart_title <- paste0("2024 Survey Results for", " ", input$select_report, " ", "against", " ", input$select_comparator)
    year_chart_title <- paste0("Survey Results for", " ", input$select_comparator, " ", "over time where available")

    tagList(
      br(),
      h3(tags$b(paste0(area_chart_title))),
      br(),
      fluidRow(column(9, h4(tags$b(paste0(input$select_question)))),
               column(3, div(actionButton("ci_info","What are confidence intervals?",
                                          icon = icon('question-circle')), style = "float: right"))),
      withSpinner(plotlyOutput("area_plot")),
      br(),
      h3(tags$b(paste0(year_chart_title))),
      fluidRow(column(12, tags$b("Note:"), "Where responses from previous years do not appear, the question is either not comparable
         between years, or is a new question for 2024"),
               column(9, h4(tags$b(input$select_question)))),
      withSpinner(plotlyOutput("area_plot2")),
      column(3, download_data_UI(id = "download_area_data")),
      column(12, dataTableOutput("area_table")), br(), br()
    ) #tagList

  })



# Repeat for Data by cancer group tab
  output$group <- renderUI({

    group_chart_title <- paste0("2024 Survey Results for", " ", input$select_cancer_group, " ", "cancer group against All cancer groups")
    year_chart_title <- paste0("Survey Results for", " ", input$select_cancer_group, " ", "cancer group over time")

    tagList(
      br(),
      h3(tags$b(paste0(group_chart_title))),
      br(),
      fluidRow(column(9, h4(tags$b(paste0(input$group_select_question)))),
               column(3, div(actionButton("group_ci_info","What are confidence intervals?",
                                          icon = icon('question-circle')), style = "float: right"))),
      withSpinner(plotlyOutput("cancer_group_plot")),
      br(),
      h3(tags$b(paste0(year_chart_title))),
      fluidRow(column(12, tags$b("Note:"), "Where responses from previous years do not appear, the question is either not comparable
         between years, or is a new question for 2024"),
               column(9, h4(tags$b(input$group_select_question)))),
      withSpinner(plotlyOutput("cancer_group_plot2")),
      column(3, download_data_UI(id = "download_cancer_group_data")),
      column(12, dataTableOutput("cancer_group_table")), br(), br()
    ) #tagList

  })

################################################################################
# Charts------------------------------------------------------------------------
################################################################################
# Set question numbers for horizontal charts
questions <- c("Q07", "Q46", "Q48")

# Survey results for comparing report areas
  output$area_plot <- renderPlotly({

  # set this for filtering so chart legend appears in correct order
  dropdown_items <- c(input$select_report,
                        input$select_comparator)

  area_plot <- area_data() %>%
        filter(report_area_name %in% dropdown_items) %>%

        # add unique to levels to ensure correct functionality
        mutate(report_area_name = factor(report_area_name, levels = unique(dropdown_items))) %>%

        # add so reponse options are displayed in correct order
        mutate(response_text_dashboard = factor(response_text_dashboard,
                                                levels = unique(response_text_dashboard)))

      # use if else statement to produce horizontal or vertical charts depending on question response length
      if(area_data()$question %in% questions) {

        area_plot %>%
        plot_ly(x = ~wgt_percent*100, y = ~response_text_dashboard,
                color = ~report_area_name,
                colors = plot1_colours,
                type = "bar",
                marker = list(line = list(color = "black",
                                          width = 1)),
                orientation = "h",
                error_x = list(color = ci_colour,
                               array = ~(wgt_percent_upp-wgt_percent)*100,
                               arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
          layout(yaxis = list(title = "",
                              autorange = "reversed"),
                 xaxis = list(ticksuffix = "%",
                              title = "",
                              range = list(0, 100)),
                 bargroupgap = 0.15,
                 legend = list(orientation = "h", x=0, y=1.2),
                 margin = list(pad = 10))


}
      else {

       area_plot %>%
        plot_ly(x = ~response_text_dashboard, y = ~wgt_percent*100,
                color = ~report_area_name,
                colors = plot1_colours,
                type = "bar",
                marker = list(line = list(color = "black",
                                          width = 1)),
                error_y = list(color = ci_colour,
                               array = ~(wgt_percent_upp-wgt_percent)*100,
                               arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
          layout(xaxis = list(title = ""),
                 yaxis = list(ticksuffix = "%",
                              title = "",
                              range = list(0, 100)),
                 bargroupgap = 0.15,
                 legend = list(orientation = "h", x=0, y=1.2),
                 margin = list(pad = 10))

    }
  }
  )  #plotly end

# add time chart to compare current and previous years for area selected in step 4
output$area_plot2 <- renderPlotly({

  area_plot2 <- time_data() %>%
    mutate(response_text_dashboard = factor(response_text_dashboard,
                                            levels = unique(response_text_dashboard)))

 if(time_data()$question %in% questions) {

    area_plot2 %>%
      plot_ly(x = ~wgt_percent*100, y = ~response_text_dashboard,
              type = "bar",
              orientation = "h",
              name = 2015,
              marker = list(color = "#E9F2F3",
                            line = list(color = "black",
                                        width = 1)),
              error_x = list(color = ci_colour,
                             array = ~(wgt_percent_upp-wgt_percent)*100,
                             arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
      add_trace(x = ~wgt_percent_2018*100, y = ~response_text_dashboard,
                type = "bar",
                orientation = "h",
                name = 2018,
                marker = list(color = "#8FBFC2",
                              line = list(color = "black",
                                          width = 1)),
                error_x = list(color = ci_colour,
                               array = ~(wgt_percent_upp_2018-wgt_percent_2018)*100,
                               arrayminus = ~(wgt_percent_2018-wgt_percent_low_2018)*100)) %>%
      add_trace(x = ~wgt_percent*100, y = ~response_text_dashboard,
                type = "bar",
                orientation = "h",
                name = 2024,
                marker = list(color = "#1E7F84",
                              line = list(color = "black",
                                          width = 1)),
                error_x = list(color = ci_colour,
                               array = ~(wgt_percent_upp-wgt_percent)*100,
                               arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
      layout(yaxis = list(ticksuffix = "  ",
                          title = "",
                          showline = T,
                          autorange = "reversed"),
             xaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100),
                          showgrid = T),
             bargroupgap = 0.25,
             legend = list(orientation = "h", x = 0, y = 1.2))


  }

  else{
    area_plot2 %>%
      plot_ly(x = ~response_text_dashboard, y = ~wgt_percent_2015*100,
              type = "bar",
              name = 2015,
              marker = list(color = "#E9F2F3",
                            line = list(color = "black",
                                        width = 1)),
              error_y = list(color = ci_colour,
                             array = ~(wgt_percent_upp_2015-wgt_percent_2015)*100,
                             arrayminus = ~(wgt_percent_2015-wgt_percent_low_2015)*100)) %>%
      add_trace(x = ~response_text_dashboard, y = ~wgt_percent_2018*100,
                type = "bar",
                name = 2018,
                marker = list(color = "#8FBFC2",
                              line = list(color = "black",
                                          width = 1)),
                error_y = list(color = ci_colour,
                               array = ~(wgt_percent_upp_2018-wgt_percent_2018)*100,
                               arrayminus = ~(wgt_percent_2018-wgt_percent_low_2018)*100)) %>%
      add_trace(x = ~response_text_dashboard, y = ~wgt_percent*100,
                type = "bar",
                name = 2024,
                marker = list(color = "#1E7F84",
                              line = list(color = "black",
                                          width = 1)),
                error_y = list(color = ci_colour,
                               array = ~(wgt_percent_upp-wgt_percent)*100,
                               arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100)),
             bargroupgap = 0.25,
             legend = list(orientation = "h", x = 0, y = 1.2),
             margin = list(pad = 10))
  }

  }
  )#plotly end



# Survey results for comparing individual cancer groups with All cancers group
output$cancer_group_plot <- renderPlotly({


  cancer_group_plot <- cancer_group_data() %>%

    # ensures responses appear in correct order
    mutate(response_text_dashboard = factor(response_text_dashboard,
                                            levels = unique(response_text_dashboard)))

  # use if else statement to produce horizontal or vertical charts depending on question response length
  if(cancer_group_data()$question %in% questions) {

    cancer_group_plot %>%
      plot_ly(x = ~wgt_percent*100, y = ~response_text_dashboard,
              type = "bar",
              orientation = "h",
              name = input$select_cancer_group,
              marker = list(color = "#3F3685",
                            line = list(color = "black",
                                        width = 1)),
              error_x = list(color = ci_colour,
                             array = ~(wgt_percent_upp-wgt_percent)*100,
                             arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
      add_trace(x = ~wgt_percent_all_cancers*100, y = ~response_text_dashboard,
                type = "bar",
                orientation = "h",
                name = "All cancer groups",
                marker = list(color = "#9F9BC2",
                              line = list(color = "black",
                                          width = 1)),
                error_x = list(color = ci_colour,
                               array = ~(wgt_percent_upp_all_cancers-wgt_percent_all_cancers)*100,
                               arrayminus = ~(wgt_percent_all_cancers-wgt_percent_low_all_cancers)*100)) %>%
      layout(yaxis = list(title = "",
                          autorange = "reversed"),
             xaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100)),
             bargroupgap = 0.15,
             legend = list(orientation = "h", x = 0, y = 1.2),
             margin = list(pad = 10))
  }

  else{

    cancer_group_plot %>%
      plot_ly(x = ~response_text_dashboard, y = ~wgt_percent*100,
              type = "bar",
              name = input$select_cancer_group,
              marker = list(color = "#3F3685",
                            line = list(color = "black",
                                        width = 1)),
              error_y = list(color = ci_colour,
                             array = ~(wgt_percent_upp-wgt_percent)*100,
                             arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
      add_trace(x = ~response_text_dashboard, y = ~wgt_percent_all_cancers*100,
                type = "bar",
                name = "All cancer groups",
                marker = list(color = "#9F9BC2",
                              line = list(color = "black",
                                          width = 1)),
                error_y = list(color = ci_colour,
                               array = ~(wgt_percent_upp_all_cancers-wgt_percent_all_cancers)*100,
                               arrayminus = ~(wgt_percent_all_cancers-wgt_percent_low_all_cancers)*100)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100)),
             bargroupgap = 0.15,
             legend = list(orientation = "h", x = 0, y = 1.2),
             margin = list(pad = 10))

  }

})


# add time chart to compare current and previous years for cancer group selected
output$cancer_group_plot2 <- renderPlotly({

  cancer_group_plot2 <- cancer_group_data() %>%
    mutate(response_text_dashboard = factor(response_text_dashboard,
                                            levels = unique(response_text_dashboard)))

  if(cancer_group_data()$question %in% questions) {

    cancer_group_plot2 %>%
      plot_ly(x = ~wgt_percent_2015*100, y = ~response_text_dashboard,
              type = "bar",
              orientation = "h",
              name = 2015,
              marker = list(color = "#E9F2F3",
                            line = list(color = "black",
                                        width = 1)),
              error_x = list(color = ci_colour,
                             array = ~(wgt_percent_upp_2015-wgt_percent_2015)*100,
                             arrayminus = ~(wgt_percent_2015-wgt_percent_low_2015)*100)) %>%
      add_trace(x = ~wgt_percent_2018*100, y = ~response_text_dashboard,
                type = "bar",
                orientation = "h",
                name = 2018,
                marker = list(color = "#8FBFC2",
                              line = list(color = "black",
                                          width = 1)),
                error_x = list(color = ci_colour,
                               array = ~(wgt_percent_upp_2018-wgt_percent_2018)*100,
                               arrayminus = ~(wgt_percent_2018-wgt_percent_low_2018)*100)) %>%
      add_trace(x = ~wgt_percent*100, y = ~response_text_dashboard,
                type = "bar",
                orientation = "h",
                name = 2024,
                marker = list(color = "#1E7F84",
                              line = list(color = "black",
                                          width = 1)),
                error_x = list(color = ci_colour,
                               array = ~(wgt_percent_upp-wgt_percent)*100,
                               arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
      layout(yaxis = list(ticksuffix = "  ",
                          title = "",
                          showline = T,
                          autorange = "reversed"),
             xaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100),
                          showgrid = T),
             bargroupgap = 0.25,
             legend = list(orientation = "h", x = 0, y = 1.2))
  }

  else{
    cancer_group_plot2 %>%
      plot_ly(x = ~response_text_dashboard, y = ~wgt_percent_2015*100,
              type = "bar",
              name = 2015,
              marker = list(color = "#E9F2F3",
                            line = list(color = "black",
                                        width = 1)),
              error_y = list(color = ci_colour,
                             array = ~(wgt_percent_upp_2015-wgt_percent_2015)*100,
                             arrayminus = ~(wgt_percent_2015-wgt_percent_low_2015)*100)) %>%
      add_trace(x = ~response_text_dashboard, y = ~wgt_percent_2018*100,
                type = "bar",
                name = 2018,
                marker = list(color = "#8FBFC2",
                              line = list(color = "black",
                                          width = 1)),
                error_y = list(color = ci_colour,
                               array = ~(wgt_percent_upp_2018-wgt_percent_2018)*100,
                               arrayminus = ~(wgt_percent_2018-wgt_percent_low_2018)*100)) %>%
      add_trace(x = ~response_text_dashboard, y = ~wgt_percent*100,
                type = "bar",
                name = 2024,
                marker = list(color = "#1E7F84",
                              line = list(color = "black",
                                          width = 1)),
                error_y = list(color = ci_colour,
                               array = ~(wgt_percent_upp-wgt_percent)*100,
                               arrayminus = ~(wgt_percent-wgt_percent_low)*100)) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(ticksuffix = "%",
                          title = "",
                          range = list(0, 100)),
             bargroupgap = 0.25,
             legend = list(orientation = "h", x = 0, y = 1.2),
             margin = list(pad = 10))
  }

}
)#plotly end




################################################################################
# Tables -----------------------------------------------------------------------
################################################################################

# Data by area table
  output$area_table <- renderDataTable({

    # make sure to use reactive dataset
    table <- area_data() %>%

      # select required variables for table
      select(- c(question, question_text, topic, level)) %>%

      # mutate for appropriate decimal places (include round() to avoid the rounding error in dashboard presentation)
      mutate(across(starts_with("wgt_percent"), ~ round(.x *100, 2))) %>%
      mutate(across(starts_with("wgt_percent") & !contains(c("low", "upp")), ~ round(.x))) %>%
      mutate("Confidence Interval 2024" = if_else(!is.na(wgt_percent),paste0("(", wgt_percent_low," - ",wgt_percent_upp,")"),""),
             "Confidence Interval 2018" = if_else(!is.na(wgt_percent_2018),paste0("(", wgt_percent_low_2018," - ",wgt_percent_upp_2018,")"),""),
             "Confidence Interval 2015" = if_else(!is.na(wgt_percent_2015),paste0("(", wgt_percent_low_2015," - ",wgt_percent_upp_2015,")"),"")) %>%
      #rename variables for table presentation
      select("Location" = report_area_name,
             "Response option" = response_text_dashboard,
             "Number of responses 2024" = n_response,
             "Response % 2024" = wgt_percent,
             "Confidence Interval 2024",
             "Response % 2018" = wgt_percent_2018,
             "Confidence Interval 2018",
             "Response % 2015" = wgt_percent_2015,
             "Confidence Interval 2015")

    # add caption and format table
    datatable(table,
              caption = paste0("Responses for"," ", input$select_report, " ", "and", " ",
                               input$select_comparator, " ",  "over time"),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(scrollX = TRUE,
                             dom = 't'))
  })

# Repeat steps for data by cancer group table
  output$cancer_group_table <- renderDataTable({

      table <- cancer_group_data_table() %>%
        select(report_area, response_text_dashboard, n_response, wgt_percent, wgt_percent_low, wgt_percent_upp,
               wgt_percent_2018,wgt_percent_low_2018, wgt_percent_upp_2018, wgt_percent_2015, wgt_percent_low_2015, wgt_percent_upp_2015) %>%
        # mutate for appropriate decimal places (include round() to avoid the rounding error in dashboard presentation)
        mutate(across(starts_with("wgt_percent"), ~ round(.x *100, 2))) %>%
        mutate(across(starts_with("wgt_percent") & !contains(c("low", "upp")), ~ round(.x))) %>%
        mutate("Confidence Interval 2024" = if_else(!is.na(wgt_percent),paste0("(", wgt_percent_low," - ",wgt_percent_upp,")"),""),
               "Confidence Interval 2018" = if_else(!is.na(wgt_percent_2018),paste0("(", wgt_percent_low_2018," - ",wgt_percent_upp_2018,")"),""),
               "Confidence Interval 2015" = if_else(!is.na(wgt_percent_2015),paste0("(", wgt_percent_low_2015," - ",wgt_percent_upp_2015,")"),"")) %>%
        #rename variables for table presentation
        select("Cancer group" = report_area,
               "Response option" = response_text_dashboard,
               "Number of responses 2024" = n_response,
               "Response % 2024" = wgt_percent,
               "Confidence Interval 2024",
               "Response % 2018" = wgt_percent_2018,
               "Confidence Interval 2018",
               "Response % 2015" = wgt_percent_2015,
               "Confidence Interval 2015")

    datatable(table,
              caption = paste0("Responses for the", " ", input$select_cancer_group, " ", "cancer group in 2024 compared to all responses 2024 and", " ",
                               input$select_cancer_group, " ", "cancer group responses in previous years"),
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              rownames = FALSE,
              options = list(scrollX = TRUE,
                             dom = 't'))

  })


################################################################################
# Data downloads ---------------------------------------------------------------
################################################################################
# This section prepares the data in each tab for csv download.

# Area data to be downloaded
  area_download <- reactive({

    area_extract <- area_data() %>%
      select(-c(question, question_text, topic, level)) %>%
      mutate(across(starts_with("wgt_percent"), ~ (.x *100))) %>%
      rename("Location" = report_area_name,
             "Response" = response_text_dashboard,
             "Number of responses 2024" = n_response,
             "Response % 2024" = wgt_percent,
             "Lower CI 2024" = wgt_percent_low,
             "Upper CI 2024" = wgt_percent_upp,
             "Response % 2018" = wgt_percent_2018,
             "Lower CI 2018" = wgt_percent_low_2018,
             "Upper CI 2018" = wgt_percent_upp_2018,
             "Response % 2015" = wgt_percent_2015,
             "Lower CI 2015" = wgt_percent_low_2015,
             "Upper CI 2015" = wgt_percent_upp_2015)
  })

# Cancer groups data to be downloaded
  cancer_group_download <- reactive({

    cancer_group_extract <- cancer_group_data() %>%
      select(-c(question, question_text, topic)) %>%
      mutate(across(starts_with("wgt_percent"), ~ (.x *100))) %>%
      rename("Cancer group" = report_area,
             "Response" = response_text_dashboard,
             "Number of responses 2024" = n_response,
             "Response % 2024" = wgt_percent,
             "Lower CI 2024" = wgt_percent_low,
             "Upper CI 2024" = wgt_percent_upp,
             "All cancer groups response % 2024" = wgt_percent_all_cancers,
             "Lower CI all cancer groups 2024" = wgt_percent_low_all_cancers,
             "Upper CI all cancer groups 2024" = wgt_percent_upp_all_cancers,
             "Response % 2018" = wgt_percent_2018,
             "Lower CI 2018" = wgt_percent_low_2018,
             "Upper CI 2018" = wgt_percent_upp_2018,
             "Response % 2015" = wgt_percent_2015,
             "Lower CI 2015" = wgt_percent_low_2015,
             "Upper CI 2015" = wgt_percent_upp_2015
      )

  })

# Add download data functionality using function (found in global.R)
  download_data_server(id = "download_area_data", data = area_download, filename = "area")
  download_data_server(id = "download_cancer_group_data", data = cancer_group_download, filename = "cancer_group")

} # server end

################################################################################
# End of script ----------------------------------------------------------------
################################################################################









