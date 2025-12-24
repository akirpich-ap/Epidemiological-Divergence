# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2025.03.03. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)

# Library to perform column medians and other useful matrix algebra computations.
#library(matrixStats)

# Library for the latex exports in the nice format.
#library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
#library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
#library(methods)

# Library for read_csv
#library(readr)

# Loading package required to read library(readxl)
library(readxl)

# To convert dates from text to dates
# library(lubridate)

# Installing missing package for json files.
# install.packages("rjson")

# Loading library(rjson) for json files. 
# library(rjson)


# Getting current folder
getwd()


# Fix 2024.11.08.
# Reading new and old regions match file
load(file = "../R_Data/part01_01_districts_redistribution.RData")
# Removing districts of the oblast since those data are not there.
part01_01_districts_redistribution <- part01_01_districts_redistribution[ part01_01_districts_redistribution$region != "Districts of the oblast", ]


# Fix 2024.11.08.
# Reading master data files

# datasets
# Path
diseases_names_master_file_path <- paste0("../Data/diseases_names_master_file.xlsx")
# Actual file
diseases_names_master_file <- data.frame(read_xlsx(path = diseases_names_master_file_path)) 

# Here is the list of infections which will be processed.
infections_list <- unique(diseases_names_master_file$short_name_part1)
# infections_list <- unique(diseases_names_master_file$infection_name)[12:length(unique(diseases_names_master_file$infection_name))]


# plot configurations
# Path
master_file_all_infections_plots_configuration_path <- paste0("../Data/master_file_all_infections_plots_configuration.xlsx")
# Actual file
master_file_all_infections_plots_configuration <- data.frame(read_xlsx(path = master_file_all_infections_plots_configuration_path)) 

# Here is the list of infections which will be processed.
table(master_file_all_infections_plots_configuration$infection_name, master_file_all_infections_plots_configuration$region_new)




# Library to perform column medians and other useful matrix algebra computations.
# library(matrixStats)

# Library for the latex exports in the nice format.
# library(xtable)

# library(Matrix) for blog-diagonal matrices creation and other matrix manipulations.
# library(Matrix)

# This package is required to run in RScript mode rather than interactive mode.
# library(methods)

# Loading package required to read library(readxl)
# library(readxl)

# Loading library(rjson) for json files.
# library(rjson)

# install.packages("pdftools")
# library(pdftools)

# install.packages("tm")
# library(tm)

# Libraries to read hml pages
# library(XML)
# library(RCurl)
# library(rlist)

# install.packages("forecast")
# library("forecast") - libary for time series forecasting.
# library("forecast")


war_start <- as.Date("2022-02-24")


# install.packages("prophet")
# library("prophet") - library for time series forecasting.
# library("prophet")



# Arrangements fo the regions
regions_unique_sorted <- c( sort(unique(part01_01_districts_redistribution$districts_created_since_2021_shp)), "Districts of the oblast" )

regions_unique_rearranged <- c(regions_unique_sorted[1],
                               regions_unique_sorted[5],
                               regions_unique_sorted[2],
                               regions_unique_sorted[7],
                               regions_unique_sorted[6],
                               regions_unique_sorted[4],
                               regions_unique_sorted[8],
                               regions_unique_sorted[3],
                               regions_unique_sorted[9],
                               regions_unique_sorted[10])
                         

regions_unique_sorted_short <- regions_unique_sorted[-c(length(regions_unique_sorted),length(regions_unique_sorted)-1)]

regions_unique_rearranged_short <- regions_unique_rearranged[-c(length(regions_unique_rearranged),length(regions_unique_rearranged)-1)]



# Label function
label_function <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from="in", to="user")
  y <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  
  txt <- label_value
  x <- x[1] + strwidth(txt, cex=4) * 6 / 5
  y <- y[2] - strheight(txt, cex=4) * 6 / 5
  text(x, y, txt, cex = label_cex )
  
  
  # End of -> label_function <- function(label_value = "A", label_cex = 4) {
}  

















for( infection_index in c(1:dim(diseases_names_master_file)[1]) )
{
  
  # Debugging step
  # infection_index <- 5
  infection_current    <- diseases_names_master_file$short_name_part1[infection_index]
  file_name_current    <- diseases_names_master_file$file_name[infection_index]
  display_name_current <- diseases_names_master_file$output_name[infection_index]

  # Name for the current file
  file_path_current    <- paste0("../R_Data/", file_name_current, ".RData")
  # Loading the current file
  load( file = file_path_current )
  ls()  
  
  # Re-assigning a name to a generic one i.e. current data in dataset_current
  dataset_current_command <- paste0("dataset_current <- ", file_name_current)
  eval(parse(text = dataset_current_command))

  
  # Getting plot characteristics current disease
  plot_characteristics_current_disease <- master_file_all_infections_plots_configuration[master_file_all_infections_plots_configuration$infection_name == infection_current,]
  
  
  
  
  
  # Generating pdf output.
  pdf(paste("../Plots/part02_01_figure_raw_data_", infection_current, ".pdf", sep = ""), height = 12, width = 28)
  # Definign the number of plots
  par(mfrow = c(2, length(regions_unique_rearranged_short)/2 ), mar = c(5.5, 5.1, 5.1, 2.1))

  
  for(region_current_name in regions_unique_rearranged_short)
  {
    # Debugging step
    # region_current_name <- regions_unique_rearranged_short[1]
    
    # Getting a dataset for a current region
    dataset_current_region <- dataset_current[dataset_current$districts_created_since_2021_shp == region_current_name,]

    # Getting plot for a current region
    plot_characteristics_current_disease_current_region <- plot_characteristics_current_disease[plot_characteristics_current_disease$region_new == region_current_name, ]
        

    plot(
      x = dataset_current_region$period_last_date,
      y = dataset_current_region$total_cases,
      # x = dataset_current_region$period_last_date[dataset_current_region$period_last_date <= war_start],
      # y = dataset_current_region$total_cases[dataset_current_region$period_last_date <= war_start],
      col = "#238443",
      # col = color_01,
      # col = color_01, 
      # col = color_01,
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l",
      # main = paste( colnames(proporions_all_locations_data_baseline)[compartment],  sep = ""),
      # main = display_name_current,
      main = region_current_name,
      xlim = range(dataset_current_region$period_last_date),
      ylim = c( min(dataset_current_region$total_cases), 
                max(dataset_current_region$total_cases) * 1.01  ),
      # ylim = c(0, y_max_value_current * 1.2  ),
      # xlab = "Time",    
      xlab = "",
      ylab = "Counts",
      xaxt = "n",
      yaxt = "n",
      cex = 3,
      cex.axis = 1.55,
      cex.lab = 2,
      cex.main = 2.45,
      cex.sub = 2
    )
    lines(
      x = dataset_current_region$period_last_date,
      y = dataset_current_region$total_cases,
      # x = dataset_current_region$period_last_date[dataset_current_region$period_last_date < war_start],
      # y = dataset_current_region$total_cases[dataset_current_region$period_last_date < war_start],
      col = "#238443",
      # col = "#238443",
      # col = color_01,
      lwd = 10,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
    )
    lines(
      x = dataset_current_region$period_last_date[dataset_current_region$period_last_date >= war_start],
      y = dataset_current_region$total_cases[dataset_current_region$period_last_date >= war_start],
      # col = "#238443",
      col = "#d9f0a3",
      # col = color_01,
      lwd = 5,
      # pch = 16,
      # pch = shape_01,
      # pch = 17,
      type = "l"
    )
    lines(
      x = dataset_current_region$period_last_date[dataset_current_region$period_last_date >= war_start],
      y = dataset_current_region$total_cases[dataset_current_region$period_last_date >= war_start],
      # col = "#238443",
      col = "#d9f0a3",
      # col = color_01,
      lwd = 10,
      pch = 19,
      # pch = shape_01,
      # pch = 17,
      type = "p"
    )
    lines(
      x = rep(as.integer(war_start), 10),
      y = c(rep(min(dataset_current_region$total_cases), 5), rep(max(dataset_current_region$total_cases), 5)),
      col = "red",
      lwd = 2,
      lty = 2
    )
    
    legend(
      x = plot_characteristics_current_disease_current_region$location,
      inset = c(plot_characteristics_current_disease_current_region$coordinate1, plot_characteristics_current_disease_current_region$coordinate2),
      legend = c("Pre-War Data", "Wartime Data", "War Start"),
      col = "black",   
      fill = c("#238443", "#d9f0a3", "red"),
      pt.cex = c(4, 2),
      # pch = c(19, 20),
      # pch = c(19, 20),  
      # pch = c(19, 20),
      cex = plot_characteristics_current_disease_current_region$legend_cex
    )
    # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
    # Creating labels by month and converting.
    
    
    # X-axis
    # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
    # Creating labels by month and converting.
    initial_date <- min(dataset_current_region$period_last_date)
    final_date <- max(dataset_current_region$period_last_date)
    number_of_dates <- length(dataset_current_region$period_last_date)
    
    
    # Indexes to display
    x_indexes_to_display <- 
      dataset_current_region$period_last_date[seq( from  =  1, to  = length(dataset_current_region$period_last_date),  by = 5 )]
    # x_indexes_to_display[1] <- 1
    # Actual lab elements
    x_tlab <- x_indexes_to_display
    # ctual lab labels
    # x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
    x_lablist <- 
      dataset_current_region$period[seq( from  =  1, to  = length(dataset_current_region$period_last_date),  by = 5 )]
    axis(1, at = x_tlab, labels = FALSE)
    text(x = x_tlab, y = par()$usr[3] - 0.03 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 1.2)
    
    
    # Y-axis
    # Adding axis label
    # labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
    y_min_value <- min(dataset_current_region$total_cases)
    y_max_value <- max(dataset_current_region$total_cases)
    y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
    y_lablist <- as.character(round(y_tlab, digits = 0))
    axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)
    
    
    # Label for the current plot
    region_index <- which(regions_unique_rearranged_short == region_current_name)
    label_function(label_value = toupper(letters)[region_index], label_cex = 4)
    
    
    
  # End of -> for(region_current in regions_unique_rearranged_short) 
  }  


  dev.off()

  
  
# End of -> for( infection_current in infections_list )  
}  
































































