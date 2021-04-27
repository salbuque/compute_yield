#' Almond Yield Model
#'
#' This function computes Almond Yield from a dataframe of monthly climate data
#’ Monthly precipitation and temperature will have an significant effect of yield
#' @param climate_data_df A data frame of inputs including but not limited to the following columns: numeric day, numeric month, year, tmin_c, and precip 
#' @param coeff_t1 First Coefficient of Average Monthly Temperature Value is -0.015
#' @param coeff_t2 Second Coefficient of Average Monthly Temperature Value squared is -0.0046
#' @param coeff_p1 First Coefficient of Average Total Precipitation Value is -0.07
#' @param coeff_p2 Second Coefficient of Average Total Precipitation Value squared is 0.0043
#' @param constant 0.28
#' @author Becca Reynolds, Madeline Oliver, Siya Qiu, and Simone Albuquerque
#' @examples almond_yield_model(almond_yield_model(climate_data_df = climate_data))
#' @references D.B. Lobell et al. Agricultural and Forest Meteorology 141 (2006) 208–218.
#' @return Dataframe of summary statistics for sensitivity analysis and year and Almond Yield
almond_yield_model_SA = function(#add climate_data_df
                              climate_data_df, 
                              #add default parameters based on equation given in Lobell
                              coeff_t1 = -0.015, 
                              coeff_t2 = -0.0046, 
                              coeff_p1 = -0.07,
                              coeff_p2 = 0.0043,
                              constant = 0.28)
  {#function including data frame sub-setting, temperature, precipitation, and finally the equation
  #require necessary package dplyr for df sub-setting
  require(dplyr)
  
  #calculate mean minimum temperature and sum of precipitation for each year of observation
  mean_climate_data <- climate_data_df %>% 
    select(day, month, year, tmin_c, precip) %>% 
    group_by(year, month) %>% 
    summarise(mean_tmin_c = mean(tmin_c),
              sum_precip = sum(precip)) %>% 
    ungroup()
  
  #Calculate mean min temp for Feb in each year
  mean_tmin_c_month2 <- mean_climate_data %>% 
    filter(month == 2) %>% 
    select(year, mean_tmin_c)
  
  #Calculate sum precip for Jan in each year
  sum_precip_month1 <- mean_climate_data %>% 
    filter(month == 1) %>% 
    select(year, sum_precip)
  
   #add a column for each year's yield using the function from Lobell 
  yield_result_df <- full_join(mean_tmin_c_month2, sum_precip_month1) %>% 
    mutate(almond_yield = (coeff_t1*mean_tmin_c) + (coeff_t2*(mean_tmin_c^2)) + coeff_p1*sum_precip + (coeff_p2*(sum_precip^2)) + constant) %>% 
    summarise(mean = mean(almond_yield), sd = sd(almond_yield)) %>% 
    select(mean, sd) 
  return(yield_result_df)
  
}

  
 