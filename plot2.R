## header ##
## who:    m.2, it's me Muhammad Maher, and this is my nickname.
## when:   Tue Sep 22 08:38:28 2020
## why:    Our overall goal here is simply to examine how household energy usage
##         varies over a 2-day period in February, 2007.
############

## Please, use the file in a newly opened RStudio session.
rstudioapi::restartSession('result <- draw_2(); result')

## Loads a package and installs it if it's not installed.
load_up <- function(package_name){
  substituted <- substitute(package_name)
  characterized <- as.character(substituted)
  
  installed_packages <- installed.packages() 
  installed_package_names <- rownames(installed_packages)
  is_installed <- characterized %in% installed_package_names
  
  if (!is_installed) {
    install.packages(characterized)
  }
  library(characterized, character.only = T)
}
##

## Creates auto disposable png device.
careful_png <- function(png, plotting)
{
  png
  tryCatch(plotting, finally = dev.off())
}

## Deduces file name.
get_png_file <- function(){
  load_up(stringr)
  
  full_file_name <- rstudioapi::getActiveDocumentContext()$path
  r_file_name <- basename(full_file_name)
  png_file_name <- 
    str_replace(r_file_name, regex(".r$", ignore_case = T), ".png")
  png_file_name
}
##

draw_2 <- function(){
  # Decompress the dataset file.
  unzip("exdata_data_household_power_consumption.zip")
  
  ## Calculates start and end indexes to be read.
  load_up(lubridate) 
  
  skip_start <- dmy_hms("16/12/2006 17:24:00") 
  start <- ymd_hms("2007-02-01 00:00:00")
  
  skip_difference <- interval(skip_start, start)
  skip <- skip_difference / minutes(1)
  skip_exclusive <- skip + 1
  
  end <- ymd_hms("2007-02-02 23:59:00")
  n_max_difference <- interval(start, end)  
  n_max <- n_max_difference / minutes(1)
  
  ## + 2 to touch the next day.
  n_max_inclusive <- n_max + 2
  ##
  
  ## Reads sets.
  file_name <- "./household_power_consumption.txt"
  delimiter <- ";"
  
  load_up(readr)
  
  variables <- read_delim(file_name, delimiter, n_max = 0)
  variable_names <- names(variables)
  
  consumption <- read_delim(
    file_name
    ,
    delimiter
    ,
    col_names = variable_names
    ,
    col_types = cols(
      col_date("%d/%m/%Y")
      ,
      col_time()
      ,
      col_double()
      ,
      col_double()
      ,
      col_double()
      ,
      col_double()
      ,
      col_double()
      ,
      col_double()
      ,
      col_double()
      )
    ,
    skip = skip_exclusive
    ,
    n_max = n_max_inclusive
    ,
    na = "?"
    )
  ##
  
  ## Plots.
  png_file_name <- get_png_file()
  
  augment_plot <- function(){
    date_time <- with(
      consumption
      ,
      make_datetime(
        year(Date)
        ,
        month(Date)
        ,
        day(Date)
        ,
        hour(Time)
        ,
        minute(Time)
        , 
        second(Time)
      )
    )
    
    with(
      consumption, 
      plot(
        date_time
        ,
        Global_active_power
        ,
        type = "l"
        ,
        xlab = ""
        ,
        ylab = "Global Active Power (kilowatts)"
      )
    )
    
    axis(side = 2, lwd = 2, lwd.ticks = 2, at = c(0, 2, 4, 6))
    
    x <- quantile(date_time, c(0, .5, 1))
    axis(
      side = 1
      ,
      lwd = 2
      ,
      lwd.ticks = 2
      ,
      at = x
      ,
      labels = c("Thu", "Fri", "Sat")
    )
  }
  
  
  careful_png(
    png(png_file_name, width = 480, height = 480, unit = "px")
    ,
    augment_plot()
    )
  ##
}

