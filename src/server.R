
#source('vars.R', local = TRUE) # For Elisa
#source('utils.R', local = TRUE) # For Elisa


# Define server
server <- function(input, output) {
  ##############################################################################
  # Home Page ------------------------------------------------------------------
  ## Images
  output$yale <- renderText({ add_image(logo_yale, "25%") })
  
  ## Value Boxes
  output$Box01 <- vb_render(N, 'export', 'Entries in Data Base', '', 'green')
  output$Box02 <- vb_render(R, 'import', 'Number of Columns', '', 'blue')
  
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
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background = element_blank())
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
      ret = query_kwds(data2, si, "brief_title", match_all = TRUE)
    } else {
      ret = data2
    }
    
    # Final data
    ret |>
      collect()
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
    output$Box13 <- vb_render(perc, 'check', 'Proportion in Table & Histogram', '%', 'orange')
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
      ggplot(aes(x = date, y = count)) +
      geom_line() +
      xlab("Date") +
      ylab("Count") + 
      theme_bw()
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
  output$trial_table = renderDataTable({
    get_studies_table() |> 
      select(nct_id, brief_title, start_date, completion_date, name) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date,
             `Country` = name)
  })
  
  ## Data Explore Page has loaded
  removeUI(selector = '#main_wait_message' )
  
  ##############################################################################
  # About Page -----------------------------------------------------------------
  output$sds <- renderText({ add_image(logo_sds, "10%") })
  output$alex_jpg <- renderText({ add_image(img_alex, "75%") })
  output$elisa_jpg <- renderText({ add_image(img_elisa, "75%") })
  output$nokkvi_jpg <- renderText({ add_image(img_nokkvi, "75%") })
}


