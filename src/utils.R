# This script hold utilities used for the shiny app. All utilities are created
# here except for variables.

source('~/Desktop/ctquery8/vars.R', local = TRUE) # For Alex

#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords 
#' (intersection) or any of the keywords (union)? (default FALSE; union).

query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  # Keyword can't be empty
  kwds = kwds[kwds != ""]
  # Keyword in between dollar signs and we switch ' for ''
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  
  # If we want to ignore case
  if (ignore_case) {
    like <- " ilike "
  } else {
    like <- " like "
  }
  
  # If we want to match all
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  
  # SQL filter query
  filter(d, sql(query)) 
}

#' @description Create a bar plot of the phases returned by a brief title 
#' keyword search.
#' @param d the studies to get the number of concurrent trials for.
#' @return A ggplot bar plot of phase

plot_phase_bar_plot = function(d) {
  # Set database as y
  y <- d
  
  # Create counts by phase
  z = y |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n()) |>
    collect()
  
  # Match with `all_phases`
  NAs <- unlist(all_phases)[!unlist(all_phases) %in% unlist(z)[1:nrow(z)]]
  
  # If some of the phases are missing from current db, append them
  # This is to add empty phases for a complete plot
  if (length(NAs) > 0) {
    z <- rbind(z, tibble(phase = NAs, n = 0))
  }
  
  # Set NAs as string
  z$phase[is.na(z$phase)] = "NA"
  
  # Arrange by name
  z$phase <- factor(z$phase, levels=sort(unique(z$phase)))
  
  # Save summary data for table
  rv$currentPlotData <- z |> select(phase, n) |> 
    rename(`Phase` = phase, `Count` = n)
  
  if (sum(z$n) > 0) {
    # Create plot
    ggplot(z, aes(x = phase, y = n)) +
      geom_col() +
      theme_bw() +
      xlab("Phase") +
      ylab("Count")
  } else {
    #print("----")
    msg <- paste0("Sorry, no phase available. Try ", closest_word(rv$si))
    grid <- expand.grid(1:5, 3:1)
    ggplot(grid) +
      geom_text(aes(1, 1, label = msg), size = 5.5) +
      blankTheme()
  }
}

#' @description Get the number of concurrent trials for each date in a set of 
#' studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.

get_concurrent_trials = function(d) {
  # Get all of the unique dates.
  all_dates = d |> 
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |> 
    arrange(value) |>
    na.omit() |> 
    rename(date = value)
  
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count = 
    map_dbl(
      all_dates$date, 
      ~ .x |> 
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}

#' @description Create a bar plot of the condition returned by a brief title 
#' keyword search
#' @param d the database table.
#' @return A ggplot bar plot of conditions

plot_condition_bar_plot = function(d) {
  # Set database as y
  y <- d
  
  # Count
  z = y |>
    select(study_type) |>
    group_by(study_type) |>
    summarize(n = n()) |>
    collect()
  
  # Set NA as string
  z$study_type[is.na(z$study_type)] = "NA"
  
  # Arrange by name
  z$study_type <- factor(z$study_type, levels=sort(unique(z$study_type)))
  
  # Save summary data for table
  rv$currentPlotData <- z |> select(study_type, n) |> 
    rename(`Condition` = study_type, `Count` = n)
  if (sum(z$n) > 0) {
    # Create plot
    ggplot(z, aes(x = study_type, y = n)) +
      geom_col() +
      theme_bw() +
      xlab("Conditions") +
      ylab("Count")
  } else {
    #print("----")
    msg <- paste0("Sorry, no condition available. Try ", closest_word(rv$si))
    grid <- expand.grid(1:5, 3:1)
    ggplot(grid) +
      geom_text(aes(1, 1, label = msg), size = 5.5) +
      blankTheme()
  }
}

#' @description Create a wordcloud of the acronyms returned by a brief title 
#' keyword search
#' @param d the database table.
#' @return a ggplot text word cloud of acronyms
plot_wordcloud <- function(d) {
  set.seed(3)
  # Create a vector containing only the text
  text <- d %>% 
    select(acronym) %>% 
    filter(!is.na(acronym)) %>% 
    group_by(acronym) %>% 
    summarize(n = n()) %>% 
    collect() %>% 
    arrange(-n)
  
  n <- nrow(text)
  if (n > 50) {
    w <- text$acronym[1:50]
    f <- text$n[1:50]
  } else if (n != 0) {
    w <- text$acronym[1:n]
    f <- text$n[1:n]
  }
  
  # Save summary data for table
  rv$currentPlotData <- text |> select(acronym, n) |> 
    rename(`Acronym` = acronym, `Count` = n)
  
  if (n != 0) {
    wordcloud(words = w,
              freq = f, 
              scale=c(1.5,0.7),
              min.freq = n, max.words=n,
              colors=brewer.pal(8, "Dark2"))
  } else {
    #print("----")
    msg <- paste0("Sorry, no acronym available. Try ", closest_word(rv$si))
    grid <- expand.grid(1:5, 3:1)
    ggplot(grid) +
      geom_text(aes(1, 1, label = msg), size = 5.5) +
      blankTheme()
  }
}

#' @description Create a plot of concurrent trials
#' @param d the database table.
#' @return a ggplot showing the concurrent trials
plot_concurrent <- function(d) {
  
  if (nrow(d) > 0) {
    ggplot(d, aes(x = date, y = count)) +
      geom_line() +
      xlab("Date") +
      ylab("Count") + 
      theme_bw()
  } else {
    #print("----")
    msg <- paste0("Sorry, no studies available. Try ", closest_word(rv$si))
    grid <- expand.grid(1:5, 3:1)
    ggplot(grid) +
      geom_text(aes(1, 1, label = msg), size = 5.5) +
      blankTheme()
  }
}

#' @description Get the (case-insensitive) closest word from `word_list`
#' based on Levenshtein edit distance
#' @param word the queried word
#' @return the closest word
closest_word <- function(word) {
  return( word_list[which.min(adist(word, word_list_lc))] )
}


#' @description Create a world map of country location by distinct `nct_id`
#' @param d the database table.
#' @return a ggplot map plot filled by country counts
plot_world <- function(d) {
  # Getting the data from data base
  countrySubset <- d %>% 
    distinct(nct_id, name) %>% 
    filter(!is.na(name)) %>% 
    group_by(name) %>% 
    summarize(n = n()) %>% 
    collect() 
  
  # Joining
  missingValues <- anti_join(worldJoiner, countrySubset, 
                             by= c("region" = "name")) %>% rename(name = region)
  # Setting missing values
  missingValues$n <- NA
  countrySubset <- rbind(countrySubset, missingValues)
  
  # Save summary data for table
  rv$currentPlotData <- countrySubset |> select(name, n) |> 
    rename(`Country` = name, `Count` = n) 
  # Plot map
  if (sum(countrySubset$n, na.rm = TRUE) > 0) {
    ggplot(data = countrySubset) + coord_map() + 
      geom_map(aes(map_id = name, fill = n), map = world, 
               color = "black", size = 0.2) +
      expand_limits(x = world$long, y = world$lat) +
      scale_fill_continuous(low = '#008CA3', high = '#BA0B13') +
      scale_x_continuous(limits = c(-180,180)) + 
      scale_y_continuous(limits = c(-55,120)) + 
      labs(fill = "Count") +
      blankTheme()
  } else {
    #print("----")
    msg <- paste0("Sorry, no countries available. Try ", closest_word(rv$si))
    grid <- expand.grid(1:5, 3:1)
    ggplot(grid) +
      geom_text(aes(1, 1, label = msg), size = 5.5) +
      blankTheme()
  }
}

#' @description Add a centered image to the shiny app
#' @param www url to the an image on the internet
#' @param width width of the image in percentage
add_image <- function(www, width) {
  c(
    '<body>',
    '<center>',
    '<img src="',
    www,
    '"width=', width, '>',
    '</center>',
    '</body>'
  )
}

#' @description Add a link to the end of a paragraph or in the middle of it
#' @param x the paragraph
#' @param href the url
#' @param href_text the text which will be linked to the url
#' @param href_text a option to add text after the link.
text_w_link <- function(x, href, href_text, y = '') {
  tags$p(x, tags$a(href=href, href_text), y)
}

#' @description sharing the website through social media
#' @param sm social media of preference, options include Twitter, Facebook and 
#' LinkedIn
#' @return links for social messaging platforms
share <- function(sm) {
  if (sm == 'Twitter') {
    hrefs = 'https://twitter.com/intent/tweet?url=
    https://https://www.yale.edu&text='
  } else if (sm == 'Facebook') {
    hrefs = "https://www.facebook.com/sharer/sharer
    .php?u=https://https://www.yale.edu"
  } else if (sm == 'LinkedIn') {
    hrefs = "https://www.linkedin.com/shareArticle?
    mini=true&url=https://https://www.yale.edu"
  }
  messageItem(from = sm, message = "", icon = icon(tolower(sm)), href = hrefs)
}

#' @description Create a condition panel for sidebar for the data exploration
#' page
report_sidediv <- function() {
  div(id = 'report_id', 
      conditionalPanel(
        "input.sidebar === 'report_tab'",
        textInput("brief_title_kw", "Brief title keywords"),
        selectInput('spons_sub', 'Type of Sponsor', 
                    c('All Sponsors', all_sponsors), selected = 'All Sponsors'
        )
      )
  )
}

data_sidediv <- function() {
  div(id = 'data_id', 
      conditionalPanel(
        "input.sidebar === 'data_tab'",
        downloadButton("download", "Download CSV"),
        selectizeInput(
          'features', 'Features', choices = NAMES, multiple = TRUE
        )
      )
  )
}

#' @description Loading function for loading and subsetting data. The function
#' should be used within the server part of a shiny app.
loading_func <- function(){
  i_prog <- 1
  tot_step <- 25
  withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
    # Increment the progress bar, and update the detail text.
    incProgress( i_prog/tot_step, detail = NULL)
    Sys.sleep(0.1)
  })
  i_prog <- i_prog + 1
}

#' @description A message shown while loading the app at first. It is associated
#' with the id 'main_wait_message'. The function should be used at preferred
#' positions in the UI. Having `removeUI(selector = '#main_wait_message')` 
#' in the server is highly recommended to remove the message when the app has 
#' been loaded
main_wait_msg <- function() {
  div(id = 'main_wait_message', 
      h1('Note, initial load may take up to 10 seconds.', 
         style = "color:black" , align = "center"), tags$hr())
}


#' @description style tags for value boxes
#' @param msg desired string to be output in value box
#' @param style string to define font size/other stylistic preferences for text
#' @return html tag for value box 
VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p(msg, style = style)
}

#' @description renders output for value boxes
#' @param value numeric or string input for desired information to be displayed 
#' in value box
#' @param icon_1 string value for icon name to be displayed at corner of the box
#' @param text text to be displayed under value in the box
#' @param col string value of desired box color
vb_render <- function(value, icon_1, text, special, col) {
  renderValueBox({
    valueBox(
      VB_style(paste0(format(value, big.mark=','), special), "font-size: 60%;"),
      tags$p(text),
      icon = icon(icon_1, lib = 'glyphicon'),
      color = col)
  })
}

#' @description creates custom dropdown with custom icons
#' @param ... shiny values to be listed in the dropdown
#' @param type a string that is either "messages", "notifications", or "tasks"
#' @param badgeStatus string that is either "primary", "success", "info", 
#' "warning", or "danger" to designate color or the icon. If input is not one
#' of these options, then it forces badge to be assigned by type
#' @param icon string value for icon graphic of the dropdowns
#' @param customSentence string title for dropdown to be displayed
#' @returns a dropdown with added specifications to be placed in ui
dropdownMenuCustom <- function (...,
                                type = c("messages", "notifications", "tasks"), 
                                badgeStatus = "primary", icon = NULL,
                                customSentence = '') {
  type <- match.arg(type)
  # Check if the badge color is a valid string option
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  
  # Create a list of drop down options and create as tags
  items <- c(list(...))
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  
  # Attach icons if not specified based on type
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), 
                   tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  # Assign badge color
  if (is.null(badgeStatus)) {
    badge <- NULL
  } else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), numItems)
  }
  
  # Create the custome dropdown
  tags$li(class = dropdownClass, a(href = "#", 
                                   class = "dropdown-toggle", 
                                   `data-toggle` = "dropdown", 
                                   icon, 
                                   badge), 
          tags$ul(class = "dropdown-menu", 
                  tags$li(class = "header", customSentence), 
                  tags$li(tags$ul(class = "menu", items))
          )
  )
}

#' @description stores the html body styles and color schemes of the whole app
body_styles <- function() {
  tags$head(
    tags$script("document.title = 'CT Query'"),
    ### Styles 
    tags$style(HTML(".small-box {height: 65px}")),
    tags$style(HTML(".fa { font-size: 35px; }")),
    tags$style(HTML(".glyphicon { font-size: 33px; }")), # use glyphicon package
    tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
    tags$style(HTML(".fa-globe { font-size: 20px; }")),
    tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    tags$style(HTML(".tab-content { padding-left:
                    20px; padding-right: 30px; }")) ,
    
    ## Modify Dashboard skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #00356b;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #00356b;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #00356b;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar 
                       .sidebar-menu .active a{
                       background-color: #00356b;
                                 }
                       ')
    ),
    
    ## Modify icon size in the sub side bar menu
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )),
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    # To not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    # Dropdown menu size
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.
    dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child>
    .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child> 
    .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
  )
}

#' @description empty theme for plots, removes background, tixks and axis
blankTheme <- function(){
  theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
}


