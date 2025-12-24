# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2025.03.10. ask
rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)

# Library to perform column medians and other useful matrix algebra computations.
#library(matrixStats)

# Library for the latex exports in the nice format.
library(xtable)

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

# install.packages("prophet")
# library("prophet") - library for time series forecasting.
library("prophet")



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

# Package for ARIMA and Box-Cox Transformation
# install.packages("forecast")
# library("forecast") - library for time series forecasting.
library("forecast")


war_start <- as.Date("2022-02-24")


# install.packages("prophet")
# library("prophet") - library for time series forecasting.
# library("prophet")



# Arrangmenets fo the regions
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











# Fix 2024.12.10.
# Fitting a prophet model in the loop

# Enabling Box-Cox Transformation to increase stability of estimates.
box_cox_local_list <- c(0,1)
# box_cox_local_list <- c(0)
# box_cox_local <- 0
# box_cox_local <- 1





for (box_cox_local in box_cox_local_list )
{
  # Debugging step
  # box_cox_local <- box_cox_local_list[1]

  
  for( infection_index in c(1:dim(diseases_names_master_file)[1]) )
  {
    
    # Debugging step
    # infection_index <- 5
    # infection_index <- 13
    
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
    pdf(paste("../Plots/part02_02_prophet_", infection_current, "_inc_box_cox_", box_cox_local, ".pdf", sep = ""), height = 12, width = 28)
    # Defining the number of plots
    par(mfrow = c(2, length(regions_unique_rearranged_short)/2 ), mar = c(5.5, 5.1, 5.1, 2.1))
    
    
    for(region_current_name in regions_unique_rearranged_short)
    {
      # Debugging step
      # region_current_name <- regions_unique_rearranged_short[1]
      # region_current_name <- regions_unique_rearranged_short[6]
      
      # Getting a dataset for a current region
      dataset_current_region <- dataset_current[dataset_current$districts_created_since_2021_shp == region_current_name,]
      
      # Getting plot for a current region
      plot_characteristics_current_disease_current_region <- plot_characteristics_current_disease[plot_characteristics_current_disease$region_new == region_current_name, ]
      
      
      # Fix 2024.12.04.
      # Fitting a model.
      
      if(box_cox_local == 1){ # Runs if box-cox options is chosen 
        
        print("Box-Cox transformation Requested.")
        
        # Lambda parameter 
        lam <- BoxCox.lambda(dataset_current_region$total_cases + 1, method = "loglik")
        
        # Transforming the calibration based on value of Lambda 
        dataset_current_region$total_cases_transformed <- BoxCox(dataset_current_region$total_cases + 1, lam)
        
      }else{ # Prints if the box-cox option IS NOT chosen 
        
        print("No Box-Cox transformation Requested. -> Log Base 10000 of (Y + 1) is used (then back).")
        
        # Transforming the calibration based on value of Lambda 
        dataset_current_region$total_cases_transformed <- log( x = dataset_current_region$total_cases + 1, base = 10000 )
        
      }  
      
      
      
      
      
      
      # Creating a sub-dataset which ends before the war start
      
      
      # Creating truncated and full datasets
      # Number of records BEFORE the war start
      number_of_records_total_cases <- max(which(dataset_current_region$period_last_date < war_start))
      
      
      data_to_feed_full_total_cases <- data.frame( ds = dataset_current_region$period_last_date,
                                                    y = dataset_current_region$total_cases_transformed  )
      
      data_to_feed_truncated_total_cases <- data.frame( ds = dataset_current_region$period_last_date[c(1:number_of_records_total_cases)],
                                                         y = dataset_current_region$total_cases_transformed[c(1:number_of_records_total_cases)] )
      
      
      # Creating a prophet object.
      # "changepoint.prior.scale" Controls the flexibility of the trend. 
      # The default value is 0.05. It is changed to 0.02 to ensure better fits.
      prophet_object_total_cases <- prophet(data_to_feed_truncated_total_cases, changepoint.prior.scale = 0.02)
      
      
      
      # Full frame for predictions. Dates only extraction
      data_to_feed_full_total_cases_dates_only <- subset(data_to_feed_full_total_cases, select = -c(y))
      
      # Predicting for the specified dates.
      prophet_predictions_total_cases <- predict(prophet_object_total_cases, data_to_feed_full_total_cases_dates_only)
      # Fixing dates
      prophet_predictions_total_cases$ds <- as.Date(prophet_predictions_total_cases$ds)

 
           
      
      # Forecast diagnostics 
      # horizon <- 365
      # last_date <- max(data_to_feed_truncated_total_cases$ds)
      # cutoffs = data_to_feed_truncated_total_cases$ds[data_to_feed_truncated_total_cases$ds < (last_date-horizon)]
      # cutoffs = cutoffs[2:length(cutoffs)]
      # print(cutoffs)
      # cv_results = cross_validation(prophet_object_total_cases, period=30, initial = 365*4, horizon=horizon, units="days")
      # prophet_predictions_total_cases_plus_original_data_diag = performance_metrics(cv_results)
      # save(prophet_predictions_total_cases_plus_original_data_diag, file = paste("../../R_Data/prophet_predictions_total_cases_plus_original_data_diag.RData"))
      
      
      # Adding original data
      prophet_predictions_total_cases_plus_original_data <- base::merge(x = data_to_feed_full_total_cases, y = prophet_predictions_total_cases, by = "ds")
      dim(prophet_predictions_total_cases_plus_original_data)
      
      
      # Summaries for mortalities
      dim(prophet_predictions_total_cases_plus_original_data)
      head(prophet_predictions_total_cases_plus_original_data)
      tail(prophet_predictions_total_cases_plus_original_data)
      
      
      
      if(box_cox_local == 1){ # Runs if box-cox option was selected
        
        # y
        prophet_predictions_total_cases_plus_original_data$y_original <- InvBoxCox(x = prophet_predictions_total_cases_plus_original_data$y, lambda = lam) - 1 
        
        # y-hat 
        prophet_predictions_total_cases_plus_original_data$yhat_original <- InvBoxCox(x = prophet_predictions_total_cases_plus_original_data$yhat, lambda = lam) - 1 
        
        # Lower Bound 
        prophet_predictions_total_cases_plus_original_data$yhat_lower_original <- InvBoxCox(x = prophet_predictions_total_cases_plus_original_data$yhat_lower, lambda = lam) - 1
        
        # Upper Bound 
        prophet_predictions_total_cases_plus_original_data$yhat_upper_original <- InvBoxCox(x = prophet_predictions_total_cases_plus_original_data$yhat_upper, lambda = lam) - 1
        
      }else{ # Prints if no box-cox option was selected
        
        print("No Box-Cox Transformation Selected. -> exp of (result) - 1 is used")
        
        # y
        prophet_predictions_total_cases_plus_original_data$y_original <- 10000^(prophet_predictions_total_cases_plus_original_data$y) - 1
        
        # y-hat 
        prophet_predictions_total_cases_plus_original_data$yhat_original <- 10000^(prophet_predictions_total_cases_plus_original_data$yhat) -1
        
        # Lower Bound 
        prophet_predictions_total_cases_plus_original_data$yhat_lower_original <- 10000^(prophet_predictions_total_cases_plus_original_data$yhat_lower) - 1 
        
        # Upper Bound 
        prophet_predictions_total_cases_plus_original_data$yhat_upper_original <- 10000^(prophet_predictions_total_cases_plus_original_data$yhat_upper) - 1
        
      }
      
      
      
      plot(
        # x = prophet_predictions_total_cases_plus_original_data$ds,
        # y = prophet_predictions_total_cases_plus_original_data$y_original,
        x = dataset_current_region$period_last_date[dataset_current_region$period_last_date <= war_start],
        y = dataset_current_region$total_cases[dataset_current_region$period_last_date <= war_start],
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
        xlim = range(prophet_predictions_total_cases_plus_original_data$ds),
        ylim = c( min( c(prophet_predictions_total_cases_plus_original_data$y_original, prophet_predictions_total_cases_plus_original_data$yhat_original) ), 
                  # max( prophet_predictions_total_cases_plus_original_data$y_original) * 1.1  ),
                  max( c(prophet_predictions_total_cases_plus_original_data$y_original, prophet_predictions_total_cases_plus_original_data$yhat_original) ) * 1.1  ),
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

      
      # Fix 2025.03.03.
      
      lines(
        # x = prophet_predictions_total_cases_plus_original_data$ds,
        # y = prophet_predictions_total_cases_plus_original_data$y_original,
        x = dataset_current_region$period_last_date[dataset_current_region$period_last_date >= war_start-30],
        y = dataset_current_region$total_cases[dataset_current_region$period_last_date >= war_start-30],
        col = "#238443",
        # col = color_01,
        # col = color_01, 
        # col = color_01,
        lwd = 5,
        # pch = 16,
        # pch = shape_01,
        # pch = 17,
        type = "l"
      )
      
      
      lines(
        # x = prophet_predictions_total_cases_plus_original_data$ds,
        # y = prophet_predictions_total_cases_plus_original_data$y_original,
        x = dataset_current_region$period_last_date[dataset_current_region$period_last_date < war_start],
        y = dataset_current_region$total_cases[dataset_current_region$period_last_date < war_start],
        col = "#238443",
        # col = "#238443",
        # col = color_01,
        lwd = 10,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "p"
      )

      
      # Fix 2025.03.03.
      
      lines(
        # x = prophet_predictions_total_cases_plus_original_data$ds,
        # y = prophet_predictions_total_cases_plus_original_data$y_original,
        x = dataset_current_region$period_last_date[dataset_current_region$period_last_date > war_start],
        y = dataset_current_region$total_cases[dataset_current_region$period_last_date > war_start],
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
        x = prophet_predictions_total_cases_plus_original_data$ds[prophet_predictions_total_cases_plus_original_data$ds < war_start],
        y = prophet_predictions_total_cases_plus_original_data$yhat_original[prophet_predictions_total_cases_plus_original_data$ds < war_start],
        # col = "#005BBB",
        col = "#005BBB",
        # col = color_01,
        lwd = 5,
        # pch = 16,
        # pch = shape_01,
        # pch = 17,
        type = "l"
      )
      lines(
        x = prophet_predictions_total_cases_plus_original_data$ds[prophet_predictions_total_cases_plus_original_data$ds < war_start],
        y = prophet_predictions_total_cases_plus_original_data$yhat_original[prophet_predictions_total_cases_plus_original_data$ds < war_start],
        # col = "#005BBB",
        col = "#005BBB",
        # col = color_01,
        lwd = 10,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "p"
      )
      
      lines(
        x = prophet_predictions_total_cases_plus_original_data$ds[prophet_predictions_total_cases_plus_original_data$ds >= war_start -30 ],
        y = prophet_predictions_total_cases_plus_original_data$yhat_original[prophet_predictions_total_cases_plus_original_data$ds >= war_start -30],
        # col = "#005BBB",
        col = "#FFD500",
        # col = color_01,
        lwd = 5,
        # pch = 16,
        # pch = shape_01,
        # pch = 17,
        type = "l"
      )
      lines(
        x = prophet_predictions_total_cases_plus_original_data$ds[prophet_predictions_total_cases_plus_original_data$ds >= war_start],
        y = prophet_predictions_total_cases_plus_original_data$yhat_original[prophet_predictions_total_cases_plus_original_data$ds >= war_start],
        # col = "#005BBB",
        col = "#FFD500",
        # col = color_01,
        lwd = 10,
        pch = 19,
        # pch = shape_01,
        # pch = 17,
        type = "p"
      )
      
      
      lines(
        x = rep(as.integer(war_start), 10),
        y = c(rep(min(prophet_predictions_total_cases_plus_original_data$y_original), 5), rep(max(prophet_predictions_total_cases_plus_original_data$y_original), 5)),
        col = "red",
        lwd = 2,
        lty = 2
      )
      
      legend(
        x = plot_characteristics_current_disease_current_region$location,
        inset = c(plot_characteristics_current_disease_current_region$coordinate1, plot_characteristics_current_disease_current_region$coordinate2),
        legend = c("Fitted Trend", "Predicted Trend", "Actual Data", "War Start"),
        col = "black",   
        fill = c("#005BBB", "#FFD500", "#238443", "red"),
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
      initial_date <- min(prophet_predictions_total_cases_plus_original_data$ds)
      final_date <- max(prophet_predictions_total_cases_plus_original_data$ds)
      number_of_dates <- length(prophet_predictions_total_cases_plus_original_data$ds)
      
      
      # Indexes to display
      x_indexes_to_display <- 
        prophet_predictions_total_cases_plus_original_data$ds[seq( from  =  1, to  = length(prophet_predictions_total_cases_plus_original_data$ds),  by = 5 )]
      # x_indexes_to_display[1] <- 1
      # Actual lab elements
      x_tlab <- x_indexes_to_display
      # ctual lab labels
      # x_lablist  <- as.character( p_scores_frame_five_jan_june$Month )
      x_lablist <- 
        dataset_current_region$period[seq( from  =  1, to  = length(prophet_predictions_total_cases_plus_original_data$ds),  by = 5 )]
      axis(1, at = x_tlab, labels = FALSE)
      text(x = x_tlab, y = par()$usr[3] - 0.03 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex = 1.2)
      
      
      # Y-axis
      # Adding axis label
      # labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
      y_min_value <- min( c(prophet_predictions_total_cases_plus_original_data$y_original, prophet_predictions_total_cases_plus_original_data$yhat_original) )
      y_max_value <- max( c(prophet_predictions_total_cases_plus_original_data$y_original, prophet_predictions_total_cases_plus_original_data$yhat_original) )
      y_tlab <- seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5)
      y_lablist <- as.character(round(y_tlab, digits = 0))
      axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.5)
      
      
      # Label for the current plot
      region_index <- which(regions_unique_rearranged_short == region_current_name)
      label_function(label_value = toupper(letters)[region_index], label_cex = 4)
      
      
      # Fix 2025.03.05.
      
      # Computing p-scores
      prophet_predictions_total_cases_plus_original_data$p_score <- 
        100 * (prophet_predictions_total_cases_plus_original_data$y_original - prophet_predictions_total_cases_plus_original_data$yhat_original) / 
          abs(prophet_predictions_total_cases_plus_original_data$yhat_original)

      # Fixing the NaN values i.e. overwriting with 0-s
      prophet_predictions_total_cases_plus_original_data$p_score[is.nan(prophet_predictions_total_cases_plus_original_data$p_score)] <- 0
        
      
      # Generating year and month as text
      prophet_predictions_total_cases_plus_original_data$month_text <- substr( x = as.character(prophet_predictions_total_cases_plus_original_data$ds), start = 1, stop = 7)
        
      
      # Saving the results so that they can be used for p-scores
      
      # Creating objects in R.
      current_prediction_creation_text_to_run <- paste0("part02_02_prophet_", infection_current, "_", region_current_name, "_", box_cox_local, " <- prophet_predictions_total_cases_plus_original_data")
      
      current_prediction_saving_path <- paste0( "../R_Data/part02_02_prophet_", infection_current, "_", region_current_name, "_", box_cox_local, ".RData")
      current_prediction_saving_text_to_run <- paste0("save( part02_02_prophet_", infection_current, "_", region_current_name, "_", box_cox_local, ", file = current_prediction_saving_path )")
      
      eval(parse(text = current_prediction_creation_text_to_run))
      eval(parse(text = current_prediction_saving_text_to_run))
      

      # End of -> for(region_current in regions_unique_rearranged_short) 
    }  
    
    
    dev.off()
    
    
    
    
    
    
    
    
    # Fix 2025.03.05.
    

    # Generating pdf output.
    pdf(paste("../Plots/part02_02_prophet_", infection_current, "_psc_box_cox_", box_cox_local, ".pdf", sep = ""), height = 12, width = 28)
    # Definign the number of plots
    par(mfrow = c(2, length(regions_unique_rearranged_short)/2 ), mar = c(5.5, 5.1, 5.1, 2.1))
    
    
    
    for(region_current_name in regions_unique_rearranged_short)
    {
      # Debugging step
      # region_current_name <- regions_unique_rearranged_short[1]
      # region_current_name <- regions_unique_rearranged_short[6]
      

      # Creating objects in R with a generic name.
      current_prediction_recover_text_to_run <- 
        paste0("prophet_predictions_total_cases_plus_original_data <- part02_02_prophet_", infection_current, "_", region_current_name, "_", box_cox_local)
      eval(parse(text = current_prediction_recover_text_to_run))
      
      
      # Extracting p-scores for the output.
      if (region_current_name == regions_unique_rearranged_short[1])
      {
        frame_p_scores <- data.frame( date =  prophet_predictions_total_cases_plus_original_data$ds, 
                                      current_region =  round( x= prophet_predictions_total_cases_plus_original_data$p_score, digits = 2) ) 
        
        # Fixing name
        names(frame_p_scores)[names(frame_p_scores) == "current_region" ] <- region_current_name
        
      # End of -> if (region_current_name == regions_unique_rearranged_short[1])  
      }  
      
      
      
      if (region_current_name != regions_unique_rearranged_short[1])
      {
        frame_p_scores <- data.frame( frame_p_scores, 
                                      current_region =  round( x= prophet_predictions_total_cases_plus_original_data$p_score, digits = 2) ) 
        
        names(frame_p_scores)[names(frame_p_scores) == "current_region" ] <- region_current_name                
                
      # End of -> if (region_current_name != regions_unique_rearranged_short[1])    
      }  
      

            
      
      # Min and Max
      p_score_min <- min(prophet_predictions_total_cases_plus_original_data$p_score)
      p_score_max <- max(prophet_predictions_total_cases_plus_original_data$p_score)
      
      indexes_blue   <- which(prophet_predictions_total_cases_plus_original_data$ds <  war_start)
      indexes_yellow <- which(prophet_predictions_total_cases_plus_original_data$ds >= war_start) 
      
      color_codes <- rep(x = "#000000", times = length(prophet_predictions_total_cases_plus_original_data$ds))
      color_codes[indexes_blue]   <- "#005BBB"
      color_codes[indexes_yellow] <- "#FFD500"
      
      
      
      barplot(prophet_predictions_total_cases_plus_original_data$p_score,
              col = color_codes,
              legend = TRUE,
              border = TRUE,
              # xlim = c(1, 5),
              ylim = c(p_score_min - 5, p_score_max + 5),
              args.legend = list(bty = "n", border = TRUE),
              ylab = "",
              xlab = "",
              main = region_current_name,
              # names.arg = as.character(p_scores_frame_five$Month),
              names.arg = prophet_predictions_total_cases_plus_original_data$month_text,
              cex.names = plot_characteristics_current_disease_current_region$legend_cex - 0.1,
              cex.lab = 2,
              cex.axis = 1.75,
              cex.main = 2,
              cex = 2,
              las = 2
      )

      # Adding Legends
      legend(
        # x = plot_characteristics_current_disease_current_region$location,
        # inset = c(plot_characteristics_current_disease_current_region$coordinate1, plot_characteristics_current_disease_current_region$coordinate2),
        x = "topleft",
        inset = c(0.05, 0.05),
        legend = c("Pre War", "During War"),
        col = "black",
        fill = c("#005BBB", "#FFD500"),
        pt.cex = c(4, 2),
        # pch = c(19, 20),
        # cex = plot_characteristics_current_disease_current_region$legend_cex
        cex = 1.4
      )

            

    # End of -> for(region_current in regions_unique_rearranged_short) 
    }  
    
    
    dev.off()
    

    # Saving RData files.
    
    frame_p_scores_text_to_run <- paste0("part02_02_prophet_", infection_current, "_pscores_combined_", box_cox_local, " <- frame_p_scores")
    
    frame_p_scores_saving_rdata_path <- paste0( "../R_Data/part02_02_prophet_", infection_current, "_pscores_combined_", box_cox_local, ".RData")
    frame_p_scores_saving_text_to_run <- paste0("save( part02_02_prophet_", infection_current, "_pscores_combined_", box_cox_local, ", file = frame_p_scores_saving_rdata_path )")
    
    eval(parse(text = frame_p_scores_text_to_run))
    eval(parse(text = frame_p_scores_saving_text_to_run))


    
    # Saving csv files.
    frame_p_scores_saving_csv_path <- paste0( "../R_Output/part02_02_prophet_", infection_current, "_pscores_combined_", box_cox_local, ".csv")
    write.csv(x = frame_p_scores, row.names = FALSE, file = frame_p_scores_saving_csv_path)


    # Saving tex files.
    frame_p_scores$date <- substr( x = as.character(frame_p_scores$date), start = 1, stop = 7)
    frame_p_scores_tex <- xtable( x = frame_p_scores, include.rownames = FALSE )  
    
    # Exporting as tex file
    # Creating a path 
    frame_p_scores_saving_tex_path <- paste0( "../R_Output/part02_02_prophet_", infection_current, "_pscores_combined_", box_cox_local, ".tex")
    # Printing
    print.xtable( x = frame_p_scores_tex, type="latex", file = frame_p_scores_saving_tex_path, include.rownames = FALSE )

        

    
  # End of -> for( infection_current in infections_list )  
  }  
  
  
  
  
# End of -> for (box_cox_local in c(0,1))  
}  


































































