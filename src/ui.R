# source('vars.R', local = TRUE) # For Elisa
# source('utils.R', local = TRUE) # For Elisa
source('~/Desktop/ctquery8/utils.R', local = TRUE) # For Alex
source('~/Desktop/ctquery8/vars.R', local = TRUE) # For Alex
library(shinydashboard) # For Elisa

# 1. Header --------------------------------------------------------------------
header <- 
  dashboardHeader(title = HTML("CT Query"), disable = FALSE, titleWidth = 250,
                  dropdownMenuCustom(
                    type = 'messages', customSentence = 'Love it? Share it!',
                    icon = icon("share-alt"), share('Twitter'), 
                    share('Facebook'), share('LinkedIn')
                  )
  )

# 2. Sidebar -------------------------------------------------------------------
sidebar_main <- 
  dashboardSidebar(
    width = 250, 
    sidebarMenu(
      id = 'sidebar', style = "position: relative; overflow: visible;",
      ## 1st tab: Home Page
      menuItem("Home", tabName = 'dashboard_tab', icon = icon('globe')),
      ## 2nd tab: Data Explore
      menuItem("Data Explore", tabName = 'report_tab', icon = icon('wrench')),
      report_sidediv(),
      ## 3rd tab: Download
      menuItem("Download", tabName = 'data_tab', icon = icon('dashboard')),
      data_sidediv(),
      ## 4th tab: About
      menuItem("About", tabName = 'help', icon = icon('question-circle'))
    )
  )


# 3. Body ----------------------------------------------------------------------
body <- dashboardBody(
  body_styles(),
  tabItems(
    tabItem(tabName = 'dashboard_tab',
            htmlOutput("yale"),
            main_wait_msg(),                # Main Wait Message!
            h2("Welcome!"),
            text_w_link(home_ct, www_aact, 'here.'),
            h2("About the Data Frame"),
            # Value boxes for columns and rows
            fluidRow(
              valueBoxOutput("Box01"), 
              valueBoxOutput("Box02"),
              valueBoxOutput("Box03")
            ),
            # Non-reactive world map
            p(home_db),
            plotOutput("world_static"),
            # Data table with top 10 NCT_ID in countries
            p(home_map),
            dataTableOutput("top_countries_table")

    ),
    
    # Page 2, Municipality Report ---------------------------------------------
    tabItem(tabName = 'report_tab',
            main_wait_msg(),                # Main Wait Message!
            # Logo
            htmlOutput("aact"),
            # Subsetting info
            h4('Subsetting Instructions'),
            p(explore_subset),
            tags$hr(),
            # Data Exploration div
            div(h2('Subset Exploration Tabs'), 
                fluidRow(
                  valueBoxOutput("Box11"), 
                  valueBoxOutput("Box12"), 
                  valueBoxOutput("Box13")
                  ), 
                p(explore_tabs)),
            # Tabs
            tabsetPanel(type = "tabs",
              tabPanel("Phase", plotOutput("phase_plot")),
              tabPanel("Concurrent", plotOutput("concurrent_plot")),
              tabPanel("Conditions", plotOutput("condition_plot")),
              tabPanel("Acronym WordCloud", plotOutput("wordcloud_plot")),
              tabPanel("World Map", plotOutput("world_plot"))
            ),
            # Data Table
            tags$hr(),
            tabsetPanel(type = "tabs",
                        tabPanel("View Subset Data", DTOutput("trial_table")),
                        tabPanel("View Summarized Data", DTOutput("summary_table"))
            )
    ),
    
    # Page 3, Download ---------------------------------------------------------
    tabItem(tabName = 'data_tab',
            # main_wait_msg(),                # Main Wait Message!
            # htmlOutput("aact"),
            h4('Download Instructions'),
            p(download_instructions),
            tags$hr(),
            DTOutput("features_table")
    ), 
    
    # Page 4, About ------------------------------------------------------------
    tabItem(tabName = 'help',
            # Logo
            htmlOutput("sds"),
            fluidRow(
              # About website
              h2("About the website"),
              p(about_website),
              # About us
              h2("About Us!"),
              p(about_us),
              fluidRow(
                # Alex column
                column(width = 4, h4("Alex Han", align = 'center'),
                       htmlOutput("alex_jpg"),
                       h6(tags$a(href=www_alex, "S&DS Website"), 
                          align = 'center'), p(about_alex)),
                # Elisa column
                column(width = 4, h4("Elisa Loy", align = 'center'),
                       htmlOutput("elisa_jpg"),
                       h6(tags$a(href=www_elisa, "S&DS Website"), 
                          align = 'center'), p(about_elisa)),
                # Nokkvi column
                column(width = 4, 
                       h4("Nokkvi Ellidason", align = 'center'),
                       htmlOutput("nokkvi_jpg"),
                       h6(tags$a(href=www_nokkvi, "S&DS Website"), 
                          align = 'center'), p(about_nokkvi))),
              # Contact Us
              h2("Contact Us!"),
              text_w_link(
                about_feedback, "mailto:fakeemail@yale.edu", 'our email.'
              )
            )
    )
  )
)

# Make UI
ui <- dashboardPage(header, sidebar_main, body)