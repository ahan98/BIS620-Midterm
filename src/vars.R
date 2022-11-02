# This script hold the variables used for the shiny app. The variables aren't
# reactive, hence it makes sense to only run it once outside of either the UI
# or server functions. vars.R thus holds the data base connection and
# variables.

# Content:
## Libraries
## Connection to Data Base 
## Variables
## World Map data wrangling
## Variables used for paragraphs in app
### Landing page
### Data Explore page
### About page


################################################################################
############################ Libraries #########################################

library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
library(lubridate)
library(wordcloud)
library(mapproj)
library(shinydashboard)
library(tidyr)
library(DT)

################################################################################
############################ Connection to DB ##################################
# Create the connection to a database and "studies" and "sponsors" tables.
if (!exists("con")) {
  setwd("~")  # For Alex
  # Creating Connection
  con = dbConnect(
    duckdb(
      # file.path("..", "..", "ctrialsgovdb", "ctrialsgov.duckdb"),  # For Nokkvi
      # file.path(getwd(), "ctrialsgovdb", "ctrialsgov.duckdb"), # For Elisa
      file.path(getwd(), "Desktop", "clinical-trials", "ctrialsgov.duckdb"),
      read_only = TRUE
    )
  )
  # We need the tables to be 50, so we know it's the right database
  if (length(dbListTables(con)) != 50) {
    stop("Problem reading from connection.")
  }
  # Get info of studies, sponsors and conditions
  studies = tbl(con, "studies")
  sponsors = tbl(con, "sponsors")
  conditions = tbl(con, "conditions")
  keywords = tbl(con, "keywords")
  countries = tbl(con, "countries")
  
  # Join fully by `nct_id` for condition and sponsor data base 
  dt <- full_join(conditions, sponsors, by = c('nct_id'), 
                  suffix = c('.cond', '.spons'))
  # Merge all tables by studies
  dt <- left_join(studies, dt, by = 'nct_id')
  # Merge all tables by countries
  dt <- left_join(countries, dt, by = 'nct_id')
}

################################################################################
############################ Variables #########################################
# Get number of rows
N <- nrow(collect(dt))

# Get number of columns
R <- ncol(dt)

NAMES <- colnames(dt)

# All phases in the complete data frame
all_phases <- dt |>
  select(phase) |>
  distinct() |>
  collect() 

# Max number of studies shown in data table
max_num_studies <-  1000

# All sponsors in complete data frame
all_sponsors <- dt |> 
  select(agency_class) |> 
  distinct() |> 
  collect() |> 
  mutate(agency_class = replace(agency_class, is.na(agency_class), 'NA')) |>
  arrange(agency_class)

# list of tokens (space-separated) in brief_title column
# due to complexity, tokenize 1000 random rows instead of all data
word_list <- dt |>
  select(brief_title) |>
  collect() |>
  drop_na() |>
  # head(5) |>
  sample_n(1000, replace = FALSE) |> # takes ~30 secs on all rows
  lapply(function(x) strsplit(x, split = "[ ,()\"\n]+")) |>
  unlist() |>
  unique()

# lowercase tokens (see closest_word() function in utils.R)
word_list_lc <- tolower(word_list)

################################################################################
######################## World map data wrangling ##############################
# Changing the names of countries in maps so they match those from our database
world <- map_data("world")
world[world$region == "UK", "region"] <- "United Kingdom"
world[world$region == "USA", "region"] <- "United States"
world[world$region == "Republic of Congo ", "region"] <- "Congo"
world[world$region == "Brunei", "region"] <- "Brunei Darussalam"
world[world$region == "Ivory Coast", "region"] <- "Côte D'Ivoire"
world[world$region == "Sint Maarten", "region"] <- "Saint Martin"
world[world$region == "Russia", "region"] <- "Russian Federation"
world[world$region == "Syria", "region"] <- "Syrian Arab Republic"
world[world$region == "Tobago", "region"] <- "Trinidad and Tobago"
world[world$region == "Antigua", "region"] <- "Antigua and Barbuda"
world[world$region == "Barbuda", "region"] <- "Antigua and Barbuda"
world[world$region == "Nevis", "region"] <- "Saint Kitts and Nevis"
world[world$region == "Moldova", "region"] <- "Moldova, Republic of"
world[world$region == "Libya", "region"] <- "Libyan Arab Jamahiriya"
world[world$region == "Trinidad", "region"] <- "Trinidad and Tobago"
world[world$region == "Iran", "region"] <- "Iran, Islamic Republic of"
world[world$region == "South Korea", "region"] <- "Korea, Republic of"
world[world$region == "Saint Kitts ", "region"] <- "Saint Kitts and Nevis"
world[world$region == "Virgin Islands", "region"] <- "Virgin Islands (U.S.)"
world[world$region == "Vatican", "region"] <- "Holy See (Vatican City State)"
world[world$region == "Laos", "region"] <- "Lao People's Democratic Republic"
world[world$region == "Micronesia", "region"] <- 
  "Federated States of Micronesia"
world[world$region == "Grenadines", "region"] <- 
  "Saint Vincent and the Grenadines"
world[world$region == "North Korea", "region"] <- 
  "Korea, Democratic People's Republic of"
world[world$region == "South Georgia", "region"] <- 
  "South Georgia and the South Sandwich Islands"
world[world$region == "Saint Vincent", "region"] <- 
  "Saint Vincent and the Grenadines"
world[world$region == "South Sandwich Islands", "region"] <- 
  "South Georgia and the South Sandwich Islands"
world[world$region == "Democratic Republic of the Congo", "region"] <- 
  "The Democratic Republic of the Congo"
world[world$region == "French Southern and Antarctic Lands", "region"] <- 
  "Antarctic"

# Creating a list of distinct values to append empty countries for map drawing
worldJoiner <- world %>% distinct(region)

# Complete data for countries to create welcome page static world plot
countryData <- dt %>% 
  distinct(nct_id, name) %>% 
  filter(!is.na(name)) %>% 
  group_by(name) %>% 
  summarize(n = n()) %>% 
  collect() 

################################################################################
############################ The landing page ##################################

logo_yale <- "https://logos-download.com/wp-content/uploads/
2016/11/Yale_University_logo_logotype.png"

www_aact <- "https://aact.ctti-clinicaltrials.org/data_dictionary"

home_ct <- "This Shiny Website uses AACT data for data visualisations. AACT is 
composed of 54 tables that provide information related to clinical trials. 
The database contains multiple schemas, the main one being 'ctgov' which 
provides data retrieved from ClinicalTrials.gov. The main table in the ctgov 
schema is 'studies' which relates to all other ctgov tables through the NCT_ID. 
Other tables used in combination with 'studies' are the tables 'sponsors', 
'conditions' and 'keywords'. These four tables are all merged by the NCT_ID. 
NCT ID is a primarty key that has a unique identification code given to each 
clinical study registered on ClinicalTrials.gov. The format is the letters 
'NCT' followed by an 8-digit number (for example, NCT00000419). To learn more
about the data structure of AACT and ClinicalTrials.gov, please visit their
website by clicking"

home_db <- "The merged data set with the aforementioned tables has 1.719.090 
data entries and 79 columns as can be seen by the value boxes above. These 
entries come from different locations in the world. You can see the disparity
between the countries are in the plot below. The plot shows how many distinct 
NCT_IDs can be found within each country, i.e. how many clinical studies
are registered within each country."

home_map <- "As can be seen from the map, most of the studies are done in
the USA. The top ten countries are documented below."

################################################################################
############################ The explore page ##################################

logo_aact <- "https://upload.wikimedia.org/wikipedia/commons/
thumb/d/db/Clinical_Trials_Transformation_Initiative_logo.svg/
2560px-Clinical_Trials_Transformation_Initiative_logo.svg.png"

explore_subset <- 'Use the sidebar to explore the data further.'

explore_tabs <- "Below, you'll find data visulisations for the complete data of 
your selected subset. Additionally, there is a table provided for the first 1000
entries of the subset. Note that the concurrent plot uses the same data as the 
table. Unfortunately, the histogram output is generated too slowly if the subset
data is too large, thus we limit it to a maxmium of 1000 entries. If no brief 
title query or sponsor is selected, the first 1000 rows of the full data set is 
shown."

################################################################################
############################ The download page ##################################

download_instructions <- "After subsetting the data from the Explore page,
select the features of interest, then click the \"Download\" button to export
the data in .csv format."

################################################################################
############################ The about page ####################################

logo_sds <- "https://statistics.yale.edu/sites/default/files/
styles/user_picture_node/public/sized_vertical_sds_0.png?itok=7U3MjIrh"

about_alex <- 'Alex Han loves Greenwich.'
about_elisa <- 'Elisa Loy owns a pet snail, loves dnd do stats but hates it and 
loves math theory which is very weird… and a secret that no-one knows is that 
she actually just wants to be in a thrupple with a man who has money and a girl 
who bakes'
about_nokkvi <- 'Nokkvi Ellidason is Icelandic and loves working in R.
In his spare time he does sports analytical research and works out. Favorite
ice cream has to be the original Icelandic ice cream.'

img_alex <- "https://kittyinpink.co.uk/wp-content/uploads/2016/12/
facebook-default-photo-male_1-1.jpg"
img_elisa <- "https://media-exp1.licdn.com/dms/image/C4E03AQHqdg2P9pJsCA/
profile-displayphoto-shrink_800_800/0/1582586359000?
e=2147483647&v=beta&t=o_75jBXHHpov6whr3La6cePyg1GVMSIkAFZ2E1Mz6AQ"
img_nokkvi <- "https://pbs.twimg.com/profile_images/1526222586157441025/
aNXeDgei_400x400.jpg"

www_alex <- "https://statistics.yale.edu/people/alex-han"
www_elisa <- "https://statistics.yale.edu/people/elisa-loy"
www_nokkvi <- "https://statistics.yale.edu/people/n-kkvi-elli-ason"

about_website <- "This website is an extention of the web application developed
in the class of BIS620, Data Science Software Systems, in the Fall 2022. The 
course BIS620 is a graduate course at the Biostatistics Department at Yale 
University. This website is the efforts of Alex Han, Elisa Loy and Nokkvi Dan
Ellidason for their Midterm in said class. It was handed in before November
1st, 2022. In terms of structure, the website is developed using R Shiny. We
assume that the database created in class is available, named ctrials-gov.duckdb 
and can be found in at the local path ../ctrialsgovdb, relative to the app.R
file. More about the coding structures can be found at out GitHub repository, 
which can be found in the upper right corner on the website."

about_us <- "All authors of this website are masters students at the statistics
and data science department at Yale University. Nokkvi and Elisa are second year
students while Alex is a first year. More information about each author can be
found below."

about_feedback <- "Your suggestions, feedback, complaints or compliments 
are highly valued and will guide us to improve the dashboard continuously. 
Please email them to"
