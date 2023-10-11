# microdata analysis / constructing price indexes per HH income quintile

#load libraries

library(survey)


# load data (silc 2022)
data <- read.csv2("hbs_hh_basic_2022_gr.csv")

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
data$sum_HE <- rowSums(data[, c("HE01", "HE02", "HE03", "HE04", "HE05", "HE06", "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")])


# Create a survey design object
survey_design <- svydesign(ids = ~1, weights = ~HA10, data = data)

# Calculate the weighted quintiles of "HH095" (income)
weighted_quintiles_HH095 <- svyquantile(~HH095, design = survey_design, quantiles = c(0.20, 0.40, 0.60, 0.80))

# Extract the quantile values
quintile_20 <- weighted_quintiles_HH095[[1]][1]
quintile_40 <- weighted_quintiles_HH095[[1]][2]
quintile_60 <- weighted_quintiles_HH095[[1]][3]
quintile_80 <- weighted_quintiles_HH095[[1]][4]



# Create empty vectors to store the consumption ratios per quintile
ratios_20 <- numeric(12)
ratios_40 <- numeric(12)
ratios_60 <- numeric(12)
ratios_80 <- numeric(12)


# Loop through quintiles
for (quintile in c(quintile_20, quintile_40, quintile_60, quintile_80)) {
  
  # Loop through household consumption items and calculate the ratios for the current quintile
  for (i in 1:12) {
    
    # Generate variable names with leading "0" for variables 1 to 9 and without "o" for 10 to 12
    variable_name <- ifelse(i < 10, paste0("HE0", i), paste0("HE", i))
    
    # Calculate the ratio of HEi over sum_HE
    ratio_variable <- svyratio(~get(variable_name), ~sum_HE,
                               design = subset(survey_design, HH095 <= quintile))
    
    # Extract the ratio value from ratio_variable
    ratio_value <- ratio_variable$ratio[1]
    
    # Assign the ratio value to the appropriate vector based on the quintile
    if (quintile == quintile_20) {
      ratios_20[i] <- ratio_value
    } else if (quintile == quintile_40) {
      ratios_40[i] <- ratio_value
    } else if (quintile == quintile_60) {
      ratios_60[i] <- ratio_value
    } else if (quintile == quintile_80) {
      ratios_80[i] <- ratio_value
    }
  }
}

# Print the ratios for all quintiles
print(ratios_20)
print(ratios_40)
print(ratios_60)
print(ratios_80)
