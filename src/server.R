# source('utils.R', local = TRUE) # For Elisa
# source('vars.R', local = TRUE) # For Elisa
source('~/Desktop/ctquery8/utils.R', local = TRUE) # For Alex
source('~/Desktop/ctquery8/vars.R', local = TRUE) # For Alex

rv <- reactiveValues()
rv$si <- NULL
rv$currentPlotData <- NULL

# Define server
server <- function(input, output) {
  ##############################################################################
  # Home Page ------------------------------------------------------------------
  ## Images
  output$yale <- renderText({ add_image(logo_yale, "25%") })
  
  ## Value Boxes
  output$Box01 <- vb_render(N, 'export', 'Entries in Data Base', '', 'green')
  output$Box02 <- vb_render(R, 'import', 'Number of Variables', '', 'blue')
  output$Box03 <- vb_render(R*N, 'import', 'Number of Data Points', '', 'purple')
  
  ## Plots
  output$world_static <- renderPlot({
    ggplot(data = countryData) + coord_map() + 
      geom_map(aes(map_id = name, fill = n), map = world, 
               color = "black", size =.2) +
      expand_limits(x=world$long, y=world$lat) +
      scale_x_continuous(limits=c(-180,180)) + 
      scale_y_continuous(limits=c(-55,120)) + 
      scale_fill_continuous(low = '#008CA3', high = '#BA0B13') +
      labs(fill = "Count") + 
      blankTheme()
  })
  
  ## Tables 
  output$top_countries_table = renderDataTable({
    countryData |> 
      arrange(desc(n)) |> 
      head(10) |>
      rename(`Country` = name, `Count` = n) 
  })
  
  ## Home Page has loaded
  removeUI(selector = '#main_wait_message' )
  
  ##############################################################################
  # Data Explore Page ----------------------------------------------------------
  ## Images
  output$aact <- renderText({ add_image(logo_aact, "25%") })
  
  ## Subsetting Data
  get_studies = reactive({
    loading_func()
    # Sponsor Subsetting
    if (input$spons_sub == 'All Sponsors') {
      data <- dt
    } else if (input$spons_sub == 'NA'){
      data <- dt %>% filter(!is.na(agency_class))
    } else {
      data <- query_kwds(dt, input$spons_sub, 'agency_class')
    }
    
    data2 <- data
    
    # Brief Title subsetting
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      
      rv$si <- si
      ret = query_kwds(data2, si, "brief_title", match_all = TRUE)
    } else {
      ret = data2
    }
    
    # Final data
    ret |>
      collect() |>
      subset(input$date[1] <= study_first_submitted_date
             & study_first_submitted_date <= input$date[2])
  })
  
  get_studies_table = reactive({
    loading_func()
    # Max Num Shown subseting for table 
    get_studies() |>
      head(max_num_studies) |>
      collect()
  })
  
  ## Value Boxes
  output$Box11 <- vb_render(N, 'export', 'Entries in Data Base', '', 'green')
  observe({
    loading_func()
    
    # Value box for number of rows in subset before max_num_studies subsetting
    subset <- nrow(get_studies() %>% collect())
    output$Box12 <- vb_render(subset, 'import', 'Entries in Subset & Plots', '', 'blue')
    
    # Value box for percent of rows shown of the subset data
    shown <- nrow(get_studies_table() %>% collect())
    perc <- round(100*(shown/subset), 2)
    perc <- ifelse(is.infinite(perc), 0, perc)
    output$Box13 <- vb_render(perc, 'check', 'Proportion in Table & Histogram', '%', 'orange')
  })
  
  output$trial_table <- renderDT({
    DT::datatable(get_studies_table() |> 
                    select(nct_id, brief_title, start_date, completion_date, 
                           phase, study_type, acronym, name) |>
                    rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
                           `Start Date` = start_date, 
                           `Completion Date` = completion_date, `Phase` = phase, 
                           `Condition` = study_type, `Acronym` = acronym, 
                           `Country` = name), 
                  options = list(scrollX = TRUE))
  })
  
  output$features_table <- renderDT({
    DT::datatable(
      get_studies() |> select(input$features) |> head(max_num_studies)
    )
  })
  
  ## Plots in tabs -------------------------------------------------------------
  # Phase bar plot
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_bar_plot()
  })
  
  # Concurrent histogram
  output$concurrent_plot = renderPlot({
    get_studies_table() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      plot_concurrent()
  })
  
  # Condition bar plot
  output$condition_plot = renderPlot({
    get_studies() |>
      plot_condition_bar_plot()
  })
  
  # Acronym Wordcloud
  output$wordcloud_plot <- renderPlot({
    set.seed(3)
    get_studies() |>
      plot_wordcloud()
  })
  
  # Distinct id by country, world map
  output$world_plot <- renderPlot({
    get_studies() |>
      plot_world()
  })
  
  ## Tables
  # Data table with updated names
  output$trial_table <- renderDT({
    DT::datatable(get_studies_table() |> 
                    select(nct_id, brief_title, start_date, completion_date, 
                           phase, study_type, acronym, name) |>
                    rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
                           `Start Date` = start_date, 
                           `Completion Date` = completion_date, `Phase` = phase, 
                           `Condition` = study_type, `Acronym` = acronym, 
                           `Country` = name), 
                  options = list(scrollX = TRUE))
  })
  
  # Data table with updated names
  observeEvent(rv$currentPlotData,{
    output$summary_table <- renderDT({
      DT::datatable(rv$currentPlotData, options = list(scrollX = TRUE))
    })
  })
  
  ## Data Explore Page has loaded
  removeUI(selector = '#main_wait_message' )
  
  ##############################################################################
  # Download Page --------------------------------------------------------------
  
  output$download <- downloadHandler(
    filename = function(){"studies.csv"}, 
    content = function(fname){
      write.csv(get_studies(), fname)
    }
  )
  
  ## Data Explore Page has loaded
  # removeUI(selector = '#main_wait_message' )

  ##############################################################################
  # About Page -----------------------------------------------------------------
  output$sds <- renderText({ add_image(logo_sds, "10%") })
  output$alex_jpg <- renderText({ add_image(img_alex, "75%") })
  output$elisa_jpg <- renderText({ add_image(img_elisa, "75%") })
  output$nokkvi_jpg <- renderText({ add_image(img_nokkvi, "75%") })
}


