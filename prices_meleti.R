# microdata analysis / constructing price indexes per HH income quintile

#load libraries

library(survey)



data <- read.csv2("hbs_hh_basic_2022_gr.csv")

# HC03 = sex of reference: 1 = male, 2 = female
# HC05 = 1 = unmaried, 2= married including registered partnership, 3 = widowed, divorced, separated
# HH09 5 = monetary net income , blank = no income
#HI11 = main source of income,1 = wages and salary, 2 = self employment, 3 ,.....
# HE = household expenditure




subset_data <- data[, c("HH095", "HA10")]

subset_data <- na.omit(subset_data)

survey_design <- svydesign(ids = ~1, weights = ~HA10, data = subset_data)


weighted_mean_H095 <- svymean(~HH095, design = survey_design)

weighted_mean_H095[[1]]


weighted_quantiles_HH095 <- svyquantile(~HH095, design = survey_design, quantiles = c(0.25, 0.50, 0.75))
print(weighted_quantiles_HH095)



# Calculate the weighted quintiles of "HH095"
weighted_quintiles_HH095 <- svyquantile(~HH095, design = survey_design, quantiles = c(0.20, 0.40, 0.60, 0.80))

# Print the weighted quintiles
print(weighted_quintiles_HH095)