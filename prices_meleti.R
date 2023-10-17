# microdata analysis / constructing price indexes per HH income quintile

#load libraries

library(survey)
library(tidyverse)
library(eurostat)

# load data (2022 and 2021 HBS)
data22 <- read.csv2("hbs_hh_basic_2022_gr.csv")

data21 <- read.csv2("hbs_hh_basic_2021_gr.csv")
# memo

# HC03 = sex of reference: 1 = male, 2 = female
# HC05 = 1 = unmaried, 2= married including registered partnership, 3 = widowed, divorced, separated
# HH09 5 = monetary net income , blank = no income
#HI11 = main source of income,1 = wages and salary, 2 = self employment, 3 ,.....
# HE = household expenditure

#HE01 FOOD AND NON- ALCOHOLIC BEVERAGES 
#HE02 ALCOHOLIC BEVERAGES AND TABACOO 
#HE03 CLOTHING AND FOOTWEAR 
#HE04 HOUSING, WATER, ELECTRICITY, GAS AND OTHER FUELS 
#HE05 FURNISHINGS, HOUSEHOLD EQUIPMENT AND ROUTINE HOUSEHOLD MAINTENANCE 
#HE06 HEALTH 
#HE07 TRANSPORT 
#HE08 COMMUNICATIONS 
#HE09 RECREATION AND CULTURE 
#HE10 EDUCATION 
#HE11 HOTELS, CAFES AND RESTAURANTS 
#HE12 MISCELLANEOUS GOODS AND SERVICE


# sum of expenditures. rowSums must be used to deal with NAs
data22$sum_HE <- rowSums(data22[, c("HE01", "HE02", "HE03", "HE04", "HE05", "HE06", "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")])
data21$sum_HE <- rowSums(data21[, c("HE01", "HE02", "HE03", "HE04", "HE05", "HE06", "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")])


# Create a survey design object
survey_design22 <- svydesign(ids = ~1, weights = ~HA10, data = data22)
survey_design21 <- svydesign(ids = ~1, weights = ~HA10, data = data21)


# Calculate the weighted quintiles of "HH095" (income)
weighted_quintiles_HH095_22 <- svyquantile(~HH095, design = survey_design22, quantiles = c(0.20, 0.40, 0.60, 0.80))
weighted_quintiles_HH095_21 <- svyquantile(~HH095, design = survey_design21, quantiles = c(0.20, 0.40, 0.60, 0.80))


# Extract the quantile values
quintile_20_22 <- weighted_quintiles_HH095_22[[1]][1]
quintile_40_22 <- weighted_quintiles_HH095_22[[1]][2]
quintile_60_22 <- weighted_quintiles_HH095_22[[1]][3]
quintile_80_22 <- weighted_quintiles_HH095_22[[1]][4]

# Extract the quantile values
quintile_20_21 <- weighted_quintiles_HH095_21[[1]][1]
quintile_40_21 <- weighted_quintiles_HH095_21[[1]][2]
quintile_60_21 <- weighted_quintiles_HH095_21[[1]][3]
quintile_80_21 <- weighted_quintiles_HH095_21[[1]][4]



# Create empty vectors to store the consumption ratios per quintile
ratios_20_22 <- numeric(12)
ratios_40_22 <- numeric(12)
ratios_60_22 <- numeric(12)
ratios_80_22 <- numeric(12)

ratios_20_21 <- numeric(12)
ratios_40_21 <- numeric(12)
ratios_60_21 <- numeric(12)
ratios_80_21 <- numeric(12)

# Loop through quintiles
for (quintile in c(quintile_20_22, quintile_40_22, quintile_60_22, quintile_80_22)) {
  
  # Loop through household consumption items and calculate the ratios for the current quintile
  for (i in 1:12) {
    
    # Generate variable names with leading "0" for variables 1 to 9 and without "o" for 10 to 12
    variable_name <- ifelse(i < 10, paste0("HE0", i), paste0("HE", i))
    
    # Calculate the ratio of HEi over sum_HE
    ratio_variable <- svyratio(~get(variable_name), ~sum_HE,
                               design = subset(survey_design22, HH095 <= quintile))
    
    # Extract the ratio value from ratio_variable
    ratio_value <- ratio_variable$ratio[1]
    
    # Assign the ratio value to the appropriate vector based on the quintile
    if (quintile == quintile_20_22) {
      ratios_20_22[i] <- ratio_value
    } else if (quintile == quintile_40_22) {
      ratios_40_22[i] <- ratio_value
    } else if (quintile == quintile_60_22) {
      ratios_60_22[i] <- ratio_value
    } else if (quintile == quintile_80_22) {
      ratios_80_22[i] <- ratio_value
    }
  }
}

# Print the ratios for all quintiles
print(ratios_20_22)
print(ratios_40_22)
print(ratios_60_22)
print(ratios_80_22)



# for 2021

for (quintile in c(quintile_20_21, quintile_40_21, quintile_60_21, quintile_80_21)) {
  
  # Loop through household consumption items and calculate the ratios for the current quintile
  for (i in 1:12) {
    
    # Generate variable names with leading "0" for variables 1 to 9 and without "o" for 10 to 12
    variable_name <- ifelse(i < 10, paste0("HE0", i), paste0("HE", i))
    
    # Calculate the ratio of HEi over sum_HE
    ratio_variable_21 <- svyratio(~get(variable_name), ~sum_HE,
                               design = subset(survey_design21, HH095 <= quintile))
    
    # Extract the ratio value from ratio_variable
    ratio_value <- ratio_variable_21$ratio[1]
    
    # Assign the ratio value to the appropriate vector based on the quintile
    if (quintile == quintile_20_21) {
      ratios_20_21[i] <- ratio_value
    } else if (quintile == quintile_40_21) {
      ratios_40_21[i] <- ratio_value
    } else if (quintile == quintile_60_21) {
      ratios_60_21[i] <- ratio_value
    } else if (quintile == quintile_80_21) {
      ratios_80_21[i] <- ratio_value
    }
  }
}

# Print the ratios for all quintiles
print(ratios_20_21)
print(ratios_40_21)
print(ratios_60_21)
print(ratios_80_21)



#Prices

id<- "prc_hicp_aind" 
data_prices <- get_eurostat(id, time_format = "num")
data_prices_GR <- data_prices[data_prices$geo == "EL" & data_prices$time >= 2021, ]
data_prices_GR_index <- data_prices_GR[data_prices_GR$unit=="INX_A_AVG",]
data_prices_GR_index_2022 <- data_prices_GR_index[data_prices_GR_index$time==2022,]
data_prices_GR_index_2021 <- data_prices_GR_index[data_prices_GR_index$time==2021,]


coicop_values <- c("CP01", "CP02", "CP03", "CP04", "CP05", "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12")

# Create an empty dataframe with the same structure as data_prices_GR_index
filtered_data_df <- data_prices_GR_index[0, ]

# Loop through the coicop values and append filtered data to the dataframe
for (coicop_value in coicop_values) {
  filtered_data <- data_prices_GR_index[data_prices_GR_index$coicop == coicop_value, ]
  filtered_data_df <- rbind(filtered_data_df, filtered_data)
}


filtered_data_df_2022 <- filtered_data_df[filtered_data_df$time == 2022,]
filtered_data_df_2021 <- filtered_data_df[filtered_data_df$time == 2021,]

# Price indexes 2022
sum(ratios_20_22*filtered_data_df_2022$values)
sum(ratios_40_22*filtered_data_df_2022$values)
sum(ratios_60_22*filtered_data_df_2022$values)
sum(ratios_80_22*filtered_data_df_2022$values)

#Price indexes 2021
sum(ratios_20_21*filtered_data_df_2021$values)
sum(ratios_40_21*filtered_data_df_2021$values)
sum(ratios_60_21*filtered_data_df_2021$values)
sum(ratios_80_21*filtered_data_df_2021$values)


# inflation rates
sum(ratios_20_22*filtered_data_df_2022$values)/sum(ratios_20_21*filtered_data_df_2021$values) -1
sum(ratios_40_22*filtered_data_df_2022$values)/sum(ratios_40_21*filtered_data_df_2021$values) -1
sum(ratios_60_22*filtered_data_df_2022$values)/sum(ratios_60_21*filtered_data_df_2021$values) -1
sum(ratios_80_22*filtered_data_df_2022$values)/sum(ratios_80_21*filtered_data_df_2021$values) -1
