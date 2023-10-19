# microdata analysis / constructing price indexes per HH income quintile

#load libraries

#C/Y

# poso parapano an Q21=Q22

library(survey)
library(tidyverse)
library(eurostat)
library(gridExtra)


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

###############



##############################

# Create a survey design object
survey_design22 <- svydesign(ids = ~1, weights = ~HA10, data = data22)
survey_design21 <- svydesign(ids = ~1, weights = ~HA10, data = data21)


# Calculate the weighted quintiles of "HH095" (income)
weighted_quintiles_HH095_22 <- svyquantile(~HH095, design = survey_design22, quantiles = c(0.20, 0.40, 0.60, 0.80))
weighted_quintiles_HH095_21 <- svyquantile(~HH095, design = survey_design21, quantiles = c(0.20, 0.40, 0.60, 0.80,1))


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
p20_22= sum(ratios_20_22*filtered_data_df_2022$values)
p40_22=sum(ratios_40_22*filtered_data_df_2022$values)
p60_22=sum(ratios_60_22*filtered_data_df_2022$values)
p80_22=sum(ratios_80_22*filtered_data_df_2022$values)

#Price indexes 2021
p_20_21=sum(ratios_20_21*filtered_data_df_2021$values)
p_40_21=sum(ratios_40_21*filtered_data_df_2021$values)
p_60_21=sum(ratios_60_21*filtered_data_df_2021$values)
p_80_21=sum(ratios_80_21*filtered_data_df_2021$values)


# inflation rates
inf20=sum(ratios_20_22*filtered_data_df_2022$values)/sum(ratios_20_21*filtered_data_df_2021$values) -1
inf40=sum(ratios_40_22*filtered_data_df_2022$values)/sum(ratios_40_21*filtered_data_df_2021$values) -1
inf60=sum(ratios_60_22*filtered_data_df_2022$values)/sum(ratios_60_21*filtered_data_df_2021$values) -1
inf80=sum(ratios_80_22*filtered_data_df_2022$values)/sum(ratios_80_21*filtered_data_df_2021$values) -1

# inflation rates
inf20b=sum(ratios_20_21*filtered_data_df_2022$values)/sum(ratios_20_21*filtered_data_df_2021$values) -1
inf40b=sum(ratios_40_21*filtered_data_df_2022$values)/sum(ratios_40_21*filtered_data_df_2021$values) -1
inf60b=sum(ratios_60_21*filtered_data_df_2022$values)/sum(ratios_60_21*filtered_data_df_2021$values) -1
inf80b=sum(ratios_80_21*filtered_data_df_2022$values)/sum(ratios_80_21*filtered_data_df_2021$values) -1




prices_2021 <- c(p_20_21, p_40_21, p_60_21, p_80_21)
prices_2022 <- c(p20_22, p40_22, p60_22, p80_22)

# Create a data frame to store both years
price_data <- data.frame(
  Quantile = rep(c("20", "40", "60", "80"), 2),
  Year = rep(c("2021", "2022"), each = 4),
  PriceIndex = c(prices_2021, prices_2022)
)

price_plot <- ggplot(price_data, aes(x = Quantile, y = PriceIndex, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Price Index Comparison (2021 vs. 2022)",
       y = "Price Index",
       x = "Quantile") +
  scale_fill_manual(values = c("#3366CC", "#DC3912")) +
  theme_minimal()

inflation <- c(inf20, inf40, inf60, inf80)

# Convert inflation rates to percentage
inflation_percent <- inflation * 100  # Multiply by 100 to convert to percentage

# Create a data frame for inflation
inflation_data <- data.frame(
  Quantile = c("20", "40", "60", "80"),
  Inflation = inflation_percent  # Use the converted percentage values
)


inflation_plot <- ggplot(inflation_data, aes(x = as.numeric(Quantile), y = Inflation)) +
  geom_point(color = "#009E73", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add linear trendline
  labs(title = "Inflation Rates (2022 vs. 2021)",
       y = "Inflation Rate (%)",
       x = "Quantile") +
  theme_minimal()

# Display the plot
print(inflation_plot)

combined_plot <- grid.arrange(price_plot, inflation_plot, ncol = 2)

# Display the combined plot
print(combined_plot)

##################################################

data22detailed <- data22
data21detailed <- data21

#data22detailed$sum_HE <- rowSums(data22[, c("HE0111A","HE0112A","HE0113A","HE0114A","HE0115A","HE0116A","HE0117A","HE0118A","HE0119A","HE0121A","HE0122A","HE0211A","HE0212A","HE0213A","HE0220A","HE0311A","HE0312A","HE0313A","HE0314A","HE0321A","HE0322A","HE041", "HE043", "HE044", "HE0451", "HE0452", "HE0453","HE0454","HE0455","HE05", "HE06", "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")])


survey_design22detailed <- svydesign(ids = ~1, weights = ~HA10, data = data22detailed)
survey_design21detailed <- svydesign(ids = ~1, weights = ~HA10, data = data21detailed)


weighted_quintiles_HH095_22detailed <- svyquantile(~HH095, design = survey_design22detailed, quantiles = c(0.20, 0.40, 0.60, 0.80))
weighted_quintiles_HH095_21detailed <- svyquantile(~HH095, design = survey_design21detailed, quantiles = c(0.20, 0.40, 0.60, 0.80))


quintile_20_22d <- weighted_quintiles_HH095_22detailed[[1]][1]
quintile_40_22d<- weighted_quintiles_HH095_22detailed[[1]][2]
quintile_60_22d <- weighted_quintiles_HH095_22detailed[[1]][3]
quintile_80_22d <- weighted_quintiles_HH095_22detailed[[1]][4]

quintile_20_21d <- weighted_quintiles_HH095_21detailed[[1]][1]
quintile_40_21d<- weighted_quintiles_HH095_21detailed[[1]][2]
quintile_60_21d <- weighted_quintiles_HH095_21detailed[[1]][3]
quintile_80_21d <- weighted_quintiles_HH095_21detailed[[1]][4]

# List of more detailed consumption items
#more_detailed_items <- c("HE0111A","HE0112A","HE0113A","HE0114A","HE0115A","HE0116A","HE0117A","HE0118A","HE0119A","HE0121A","HE0122A","HE0211A","HE0212A","HE0213A","HE0220A","HE0311A","HE0312A","HE0313A","HE0314A","HE0321A","HE0322A","HE041", "HE043", "HE044", "HE0451", "HE0452", "HE0453","HE0454","HE0455","HE05", "HE06", "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")
more_detailed_items <- c("HE0111A","HE0112A","HE0113A","HE0114A","HE0115A","HE0116A","HE0117A","HE0118A","HE0119A","HE0121A","HE0122A","HE0211A","HE0212A","HE0213A","HE022","HE0311A","HE0312A","HE0313A","HE0314A","HE0321A","HE0322A","HE041","HE042", "HE043", "HE044", "HE0451", "HE0452", "HE0453","HE0454","HE05", "HE06", "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")
# Create empty vectors to store the consumption ratios for detailed items
detailed_ratios_20_22 <- numeric(length(more_detailed_items))
detailed_ratios_40_22 <- numeric(length(more_detailed_items))
detailed_ratios_60_22 <- numeric(length(more_detailed_items))
detailed_ratios_80_22 <- numeric(length(more_detailed_items))


detailed_ratios_20_21 <- numeric(length(more_detailed_items))
detailed_ratios_40_21 <- numeric(length(more_detailed_items))
detailed_ratios_60_21 <- numeric(length(more_detailed_items))
detailed_ratios_80_21 <- numeric(length(more_detailed_items))


# Loop through quintiles
for (quintile in c(quintile_20_22, quintile_40_22, quintile_60_22, quintile_80_22)) {
  for (i in 1:length(more_detailed_items)) {
    item_name <- more_detailed_items[i]
    ratio_variable <- svyratio(as.formula(paste("~", item_name)), ~sum_HE,
                               design = subset(survey_design22, HH095 <= quintile))
    ratio_value <- ratio_variable$ratio[1]
    
    # Assign the ratio value to the appropriate vector based on the quintile
    if (quintile == quintile_20_22) {
      detailed_ratios_20_22[i] <- ratio_value
    } else if (quintile == quintile_40_22) {
      detailed_ratios_40_22[i] <- ratio_value
    } else if (quintile == quintile_60_22) {
      detailed_ratios_60_22[i] <- ratio_value
    } else if (quintile == quintile_80_22) {
      detailed_ratios_80_22[i] <- ratio_value
    }
  }
}

print(detailed_ratios_20_22)
print(detailed_ratios_40_22)
print(detailed_ratios_60_22)
print(detailed_ratios_80_22)

sum(detailed_ratios_20_22)
sum(detailed_ratios_40_22)
sum(detailed_ratios_60_22)
sum(detailed_ratios_80_22)

# Loop through quintiles
for (quintile in c(quintile_20_21, quintile_40_21, quintile_60_21, quintile_80_21)) {
  for (i in 1:length(more_detailed_items)) {
    item_name <- more_detailed_items[i]
    ratio_variable <- svyratio(as.formula(paste("~", item_name)), ~sum_HE,
                               design = subset(survey_design21detailed, HH095 <= quintile))
    ratio_value <- ratio_variable$ratio[1]
    
    # Assign the ratio value to the appropriate vector based on the quintile
    if (quintile == quintile_20_21) {
      detailed_ratios_20_21[i] <- ratio_value
    } else if (quintile == quintile_40_21) {
      detailed_ratios_40_21[i] <- ratio_value
    } else if (quintile == quintile_60_21) {
      detailed_ratios_60_21[i] <- ratio_value
    } else if (quintile == quintile_80_21) {
      detailed_ratios_80_21[i] <- ratio_value
    }
  }
}

print(detailed_ratios_20_21)
print(detailed_ratios_40_21)
print(detailed_ratios_60_21)
print(detailed_ratios_80_21)

sum(detailed_ratios_20_21)
sum(detailed_ratios_40_21)
sum(detailed_ratios_60_21)
sum(detailed_ratios_80_21)



#coicop_values_d <-  c("CP0111","CP0112","CP0113","CP0114","CP0115","CP0116","CP0117","CP0118","CP0119","CP0121","CP0122","CP0211","CP0212","CP0213","CP022","CP0311","CP0312","CP0313","CP0314","CP0321","CP0322","CP0411","CP0412","CP042","CP0431","CP0432","CP044","CP0451","CP0452","CP0453","CP0454","CP0455","CP05", "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12")
coicop_values_d <- c("CP0111","CP0112","CP0113","CP0114","CP0115","CP0116","CP0117","CP0118","CP0119","CP0121","CP0122","CP0211","CP0212","CP0213","CP022","CP0311","CP0312","CP0313","CP0314","CP0321","CP0322","CP041","CP041", "CP043", "CP044", "CP0451", "CP0452", "CP0453","CP0454","CP05", "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12")
# Create an empty dataframe with the same structure as data_prices_GR_index
filtered_data_df_d <- data_prices_GR_index[0, ]

# Loop through the coicop values and append filtered data to the dataframe
for (coicop_value in coicop_values_d) {
  filtered_data_d <- data_prices_GR_index[data_prices_GR_index$coicop == coicop_value, ]
  filtered_data_df_d <- rbind(filtered_data_df_d, filtered_data_d)
}


filtered_data_df_2022_d <- filtered_data_df_d[filtered_data_df_d$time == 2022,]
filtered_data_df_2021_d <- filtered_data_df_d[filtered_data_df_d$time == 2021,]

# Price indexes 2022
p20_22d= sum(detailed_ratios_20_22*filtered_data_df_2022_d$values)
p40_22d=sum(detailed_ratios_40_22*filtered_data_df_2022_d$values)
p60_22d=sum(detailed_ratios_60_22*filtered_data_df_2022_d$values)
p80_22d=sum(detailed_ratios_80_22*filtered_data_df_2022_d$values)

# Price indexes 2021
p20_21d= sum(detailed_ratios_20_21*filtered_data_df_2021_d$values)
p40_21d=sum(detailed_ratios_40_21*filtered_data_df_2021_d$values)
p60_21d=sum(detailed_ratios_60_21*filtered_data_df_2021_d$values)
p80_21d=sum(detailed_ratios_80_21*filtered_data_df_2021_d$values)


# inflation rates
inf20d=sum(detailed_ratios_20_22*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1
inf40d=sum(detailed_ratios_40_22*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1
inf60d=sum(detailed_ratios_60_22*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1
inf80d=sum(detailed_ratios_80_22*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1

print(inf20d)
print(inf40d)
print(inf60d)
print(inf80d)


sum(detailed_ratios_20_21*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1
sum(detailed_ratios_40_21*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1
sum(detailed_ratios_60_21*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1
sum(detailed_ratios_80_21*filtered_data_df_2022_d$values)/sum(detailed_ratios_20_21*filtered_data_df_2021_d$values) -1



# Create a dataframe
df_detailed <- data.frame(
  coicop = c("CP0111","CP0112","CP0113","CP0114","CP0115","CP0116","CP0117","CP0118","CP0119","CP0121","CP0122","CP0211","CP0212","CP0213","CP022","CP0311","CP0312","CP0313","CP0314","CP0321","CP0322","CP041","CP041", "CP043", "CP044", "CP0451", "CP0452", "CP0453","CP0454","CP05", "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12"),
  p20_22d = detailed_ratios_20_22 * filtered_data_df_2022_d$values,
  p40_22d = detailed_ratios_40_22 * filtered_data_df_2022_d$values,
  p60_22d = detailed_ratios_60_22 * filtered_data_df_2022_d$values,
  p80_22d = detailed_ratios_80_22 * filtered_data_df_2022_d$values,
  p20_21d = detailed_ratios_20_21*filtered_data_df_2021_d$values,
  p40_21d = detailed_ratios_40_21*filtered_data_df_2021_d$values,
  p60_21d = detailed_ratios_60_21*filtered_data_df_2021_d$values,
  p80_21d = detailed_ratios_80_21*filtered_data_df_2021_d$values
  
)

# Print the dataframe
print(df_detailed)


# Create a bar plot for p20_22d
p20_22d_plot <- ggplot(df_detailed, aes(x = reorder(coicop, p20_22d-p20_21d), y = p20_22d)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Inflation (p20_22d)",
    x = "coicop",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Create a bar plot for p20_22d
p80_22d_plot <- ggplot(df_detailed, aes(x = reorder(coicop, p80_22d-p80_21d), y = p20_22d)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Inflation (p80_22d)",
    x = "coicop",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


library(patchwork)

# Combine the two plots
combined_plots <- p20_22d_plot + p80_22d_plot

# Display the combined plot
combined_plots
#Create a new observation "CP042b" with the same value
# Sort the data frame by p20_22d and p80_22d in ascending order
df_detailed_sorted <- df_detailed[order(df_detailed$p20_22d, df_detailed$p80_22d), ]

# Create bar plots with sorted data
p20_22d_plot_sorted <- ggplot(df_detailed_sorted, aes(x = reorder(coicop, p20_22d-p20_21d), y = p20_22d)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Inflation (p20_22d)",
    x = "coicop",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

p80_22d_plot_sorted <- ggplot(df_detailed_sorted, aes(x = reorder(coicop, p80_22d-p80_21d), y = p80_22d)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Inflation (p80_22d)",
    x = "coicop",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Combine the sorted plots
combined_plots_sorted <- p20_22d_plot_sorted + p80_22d_plot_sorted

# Display the combined sorted plot
combined_plots_sorted