rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
options(scipen=20)


# Library for the latex exports in the nice format.
library(xtable)


# Getting current folder
getwd()

# War start
war_start <- as.Date("2022-02-24")


# Reading data to compare
load(file = "../R_Data/part01_06_air_raid_sirens_official_data_kharkivska_oblast_aggregated.RData")
load(file = "../R_Data/part01_08_combined_intestinal3_annual_data_monthly_aggregated.RData")
load(file = "../R_Data/part01_08_influenza_plus_ari_annual_data_monthly_aggregated.RData")


# infection abbreviation list
infection_abbreviation <- c("combined_intestinal3", "influenza_plus_ari")
current_alers <- part01_06_air_raid_sirens_official_data_kharkivska_oblast_aggregated
# Dropping "Oblast"
current_alers <- current_alers[current_alers$districts_created_since_2021_shp != "Oblast", ]

names(current_alers)
names(part01_08_influenza_plus_ari_annual_data_monthly_aggregated)


# influenza_plus_ari
# Creating data sets with alerts data and infection i.e. merge by date and location.
influenza_plus_ari <- merge(x = current_alers, y = part01_08_influenza_plus_ari_annual_data_monthly_aggregated, 
                            by = c("period_last_date", "districts_created_since_2021_shp"), all.x = TRUE, all.y = FALSE)
# Extracting war data only
influenza_plus_ari <- influenza_plus_ari[ influenza_plus_ari$period_last_date >= war_start, ]
# Sorting
influenza_plus_ari <- influenza_plus_ari[  order(influenza_plus_ari$period_last_date, influenza_plus_ari$districts_created_since_2021),]


# combined_intestinal3
# Creating data sets with alerts data and infection i.e. merge by date and location.
combined_intestinal3 <- merge(x = current_alers, y = part01_08_combined_intestinal3_annual_data_monthly_aggregated, 
                            by = c("period_last_date", "districts_created_since_2021_shp"), all.x = TRUE, all.y = FALSE)
# Extracting war data only
combined_intestinal3 <- combined_intestinal3[ combined_intestinal3$period_last_date >= war_start, ]
# Sorting
combined_intestinal3 <- combined_intestinal3[  order(combined_intestinal3$period_last_date, combined_intestinal3$districts_created_since_2021),]


# list of subregions
subregions_list <- sort(unique(current_alers$districts_created_since_2021_shp))

# number of days to drop
drop_days_list <- c(0:10)
# number of statistics to save
statistics_list <- c("mean", "t_value", "p_value", "t_lower", "t_upper")
no_of_statistics_to_save <- length(statistics_list)


for(infection_current in infection_abbreviation)
{
  # Debugging step
  # infection_current <- infection_abbreviation[1]
  
  # current_data creating 
  curent_data_code_to_run <- paste0("current_data <- ", infection_current, "")
  eval(parse(text = curent_data_code_to_run))
  
  
  diseace_current_frame <- data.frame(districts_created_since_2021_shp = c(subregions_list, statistics_list), 
                                      matrix(data = -99, nrow = length(subregions_list) + no_of_statistics_to_save, ncol =  length(drop_days_list) ) )
  
  names(diseace_current_frame)[-1] <- paste0( "drop" , drop_days_list )

    
  for (drop_days_current in drop_days_list)
  {
    # Debugging step
    # drop_days_current <- drop_days_list[2]
    # drop_days_current <- drop_days_list[1]
    # drop_days_current <- drop_days_list[10]
    
    
    for (subregion_current in subregions_list)
    {
      # Debugging step
      # subregion_current <- subregions_list[2]
      
      # Current subsegions
      current_data_subregion <- current_data[ current_data$districts_created_since_2021_shp == subregion_current,]
      
      # Only dates needed
      current_data_subregion_truncated  <- current_data_subregion
      # Dropping if needed
      if (drop_days_current > 0) {
        current_data_subregion_truncated <- head(current_data_subregion, -drop_days_current)
      }
  
      # Computing correlations.
      corr_actual  <- round( x = cor(x = current_data_subregion_truncated$total_cases, y = current_data_subregion_truncated$total_hours), digits = 2)
      
      # Saving into frame
      diseace_current_frame[ diseace_current_frame$districts_created_since_2021_shp == subregion_current, 
                             names(diseace_current_frame) == paste0("drop", drop_days_current) ] <- corr_actual
    
       
      
    # End of -> for (subregion_current in subregions_list)  
    }  
    
    # Computing summary statistics for the current drop
    data_current <- diseace_current_frame[ c(1:length(subregions_list)), names(diseace_current_frame) == paste0("drop", drop_days_current) ] 
    t_result <- t.test(x = data_current)
    
    # Saving the results for the current drop
    # mean
    diseace_current_frame[ diseace_current_frame$districts_created_since_2021_shp == statistics_list[1], 
                           names(diseace_current_frame) == paste0("drop", drop_days_current) ] <- round( x = mean(data_current), digits = 2) 
    # t_value
    diseace_current_frame[ diseace_current_frame$districts_created_since_2021_shp == statistics_list[2], 
                           names(diseace_current_frame) == paste0("drop", drop_days_current) ] <- round( x = t_result$statistic, digits = 2)
    # p_value
    diseace_current_frame[ diseace_current_frame$districts_created_since_2021_shp == statistics_list[3], 
                           names(diseace_current_frame) == paste0("drop", drop_days_current) ] <- round( x = t_result$p.value, digits = 2)
    # t_result$conf.int[1]
    diseace_current_frame[ diseace_current_frame$districts_created_since_2021_shp == statistics_list[4], 
                           names(diseace_current_frame) == paste0("drop", drop_days_current) ] <- round( x = t_result$conf.int[1], digits = 2)
    # t_result$conf.int[2]
    diseace_current_frame[ diseace_current_frame$districts_created_since_2021_shp == statistics_list[5], 
                           names(diseace_current_frame) == paste0("drop", drop_days_current) ] <- round( x = t_result$conf.int[2], digits = 2)
    

  # End of -> for (drop_days_current in drop_days_list)  
  }  
    
  
  # Ranamaing the first column 
  names(diseace_current_frame)[names(diseace_current_frame) == "districts_created_since_2021_shp"] <- "name"
  
  
  
  # Saving RData files.
  
  diseace_current_frame_text_to_run <- paste0("part03_03_correlation_", infection_current, " <- diseace_current_frame")
  
  diseace_current_frame_saving_rdata_path <- paste0( "../R_Data/part03_03_correlation_", infection_current, ".RData")
  diseace_current_frame_saving_text_to_run <- paste0("save( part03_03_correlation_", infection_current, ", file = diseace_current_frame_saving_rdata_path )")
  
  eval(parse(text = diseace_current_frame_text_to_run))
  eval(parse(text = diseace_current_frame_saving_text_to_run))
  
  
  
  # Saving csv files.
  diseace_current_frame_saving_csv_path <- paste0( "../R_Output/part03_03_correlation_", infection_current, ".csv")
  write.csv(x = diseace_current_frame, row.names = FALSE, file = diseace_current_frame_saving_csv_path)
  
  
  # Saving tex files.
  diseace_current_frame_tex <- xtable( x = diseace_current_frame, include.rownames = FALSE )  
  
  # Exporting as tex file
  # Creating a path 
  diseace_current_frame_saving_tex_path <- paste0( "../R_Output/part03_03_correlation_", infection_current, ".tex")
  # Printing
  print.xtable( x = diseace_current_frame_tex, type="latex", file = diseace_current_frame_saving_tex_path, include.rownames = FALSE )
  
  

# End of -> for(infection_current in infection_abbreviation)
}


# part03_03_correlation_combined_intestinal3
# part03_03_correlation_influenza_plus_ari  


