# setwd("~/Desktop/Spring 2023/Statistics and Data Analytics/Final Presentation")
library(countrycode)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggplot2)
library(grid)
library(stargazer)

# mask for all countries
validcodes = codelist$iso3c

# How did the impact of COVID-19 on education outcome differ 
# accross countries with different levels of GDP?


# literacy rate - world development indicator
education = read.csv("education.csv")



# wealth indicators - world development indicator
wealth = read.csv("wealth.csv")

# covid indicators - WHO
covid = read.csv("WHO-COVID-19-global-data.csv")

# population - used to get a percentage for the covid indicator (from UN)
population = read.csv("population.csv")

population = population[,c(4, 13, 17)]

# deleting useless rows/columns
education = education[,-4]
education = education[education$Country.Code %in% validcodes,]

wealth = wealth[,-4]
wealth = wealth[wealth$Country.Code %in% validcodes,]

covid$Country_code= countrycode(sourcevar = covid$Country_code, 
                                origin = "iso2c",
                                destination ="iso3c" )
covid = covid[covid$Country_code %in% validcodes,]
covid <- covid[complete.cases(covid), ] #removes data with missing country codes


population = population[population$ISO3_code %in% validcodes,]

# replacing unavailable data ".." with NA
education[education == ".."] = NA
wealth[wealth == ".."] = NA

# renaming columns
colnames(education) = c("Country_Name", "Country_Code", "Year", "Primary_Enrollment_Rate", "Youth_Literacy_Rate" ,
                        "Secondary_Enrollment_Rate")

colnames(wealth) = c("Country_Name", "Country_Code", "Year", "Highest_10ps",
                     "Lowest_10ps", "GDP_pc", "GNI_pc", "Poverty_ratio", "Gini",
                     "National_income_pc", "Urban_pop", "Gross_savings", "Unemployment_rate", "Net_primary_income")

colnames(population) = c("Country_Code", "Year", "Population")

# simplifying the covid data to per year - to match other data sets
covid$Date_reported = as.Date(covid$Date_reported)
covid_year = covid %>%
  group_by(Country, Country_code, Year = year(Date_reported)) %>%
  summarize(New_cases = sum(New_cases))

# removing all rows with year == 2023 since it is not complete
covid_year <- covid_year %>% filter(Year != 2023)

# changing data formats
education$Year = as.numeric(education$Year)
education$Primary_Enrollment_Rate = as.numeric(education$Primary_Enrollment_Rate)
education$Youth_Literacy_Rate = as.numeric(education$Youth_Literacy_Rate)
education$Secondary_Enrollment_Rate = as.numeric(education$Secondary_Enrollment_Rate)


wealth$Year = as.numeric(wealth$Year)
wealth$Highest_10ps = as.numeric(wealth$Highest_10ps)
wealth$Lowest_10ps = as.numeric(wealth$Lowest_10ps)
wealth$GDP_pc = as.numeric(wealth$GDP_pc)
wealth$GNI_pc = as.numeric(wealth$GNI_pc)
wealth$Poverty_ratio = as.numeric(wealth$Poverty_ratio)
wealth$Gini = as.numeric(wealth$Gini)
wealth$National_income_pc = as.numeric(wealth$National_income_pc)
wealth$Urban_pop = as.numeric(wealth$Urban_pop)
wealth$Gross_savings = as.numeric(wealth$Gross_savings)
wealth$Unemployment_rate = as.numeric(wealth$Unemployment_rate)
wealth$Net_primary_income = as.numeric(wealth$Net_primary_income)


population$Year = as.numeric(population$Year)
population$Population = as.numeric(population$Population)
population$Population = 1000*population$Population #population is in thousands

# Remove rows with year > 2021 (they are estimates using models)
population <- subset(population, Year <= 2021 & Year > 2018)

# merge covid_year and population and make it new_cases/population (a percentage)
covid_prop = left_join(covid_year, population,
                       by = c("Country_code" = "Country_Code", "Year" = "Year"))

covid_prop = covid_prop[complete.cases(covid_prop),]  #remove rows with missing data

# add column for the proportion of population of new cases
covid_prop$Cases_prop = covid_prop$New_cases/covid_prop$Population * 100

#add rows for year 2019 and add into data frame - make all values 0
new_rows2 = expand.grid(Country_code = unique(covid_prop$Country_code), Year = 2019, Cases_prop = 0)

covid_prop = rbind(covid_prop, new_rows2)

#add in the country names
covid_prop = covid_prop %>% group_by(Country_code) %>% 
  fill(Country, .direction = "down")
  
#Create the data frame with all variables
main_data_frame_youthliteracy = covid_prop %>% select(Country, Country_code, Year, Cases_prop)

main_data_frame_youthliteracy = left_join(main_data_frame_youthliteracy, select(education, c("Country_Code", "Year", "Youth_Literacy_Rate")), 
                            by = c("Country_code" = "Country_Code", "Year" = "Year"))

#removing some variables because of very little data availbility - ~70
main_data_frame_youthliteracy = left_join(main_data_frame_youthliteracy, select(wealth, -c("Country_Name", "Highest_10ps", "Lowest_10ps", "Poverty_ratio", "Gini")),
                                          by = c("Country_code" = "Country_Code", "Year" = "Year"))

#Create data frame for secondary enrollment rate
main_data_frame_secondary = covid_prop %>% select(Country, Country_code, Year, Cases_prop)

main_data_frame_secondary = left_join(main_data_frame_secondary, select(education, c("Country_Code", "Year", "Secondary_Enrollment_Rate")), 
                                          by = c("Country_code" = "Country_Code", "Year" = "Year"))

#removing some variables because of very little data availbility - ~70
main_data_frame_secondary = left_join(main_data_frame_secondary, select(wealth, -c("Country_Name", "Highest_10ps", "Lowest_10ps", "Poverty_ratio", "Gini")),
                                          by = c("Country_code" = "Country_Code", "Year" = "Year"))                                          


# data visualisation of youth literacy rate distributions
# grid.arrange(
#   ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2019)) +
#     aes(x = Youth_Literacy_Rate) +
#     geom_histogram(binwidth = 5) +
#     coord_cartesian(xlim = c(0, 105)) +
#     ylim(0, 40) +
#     xlab("Youth Literacy Rate (%)") +
#     ylab("Frequency") +
#     ggtitle("2019")
#   ,
#   ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2020)) +
#     aes(x = Youth_Literacy_Rate) +
#     geom_histogram(binwidth = 5) +
#     coord_cartesian(xlim = c(0, 105)) +
#     ylim(0, 40) +
#     xlab("Youth Literacy Rate (%)") +
#     ylab("Frequency") +
#     ggtitle("2020")
#   ,
#   ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2021)) +
#     aes(x = Youth_Literacy_Rate) +
#     geom_histogram(binwidth = 5) +
#     coord_cartesian(xlim = c(0, 105)) +
#     ylim(0, 40) +
#     xlab("Youth Literacy Rate (%)") +
#     ylab("Frequency") +
#     ggtitle("2021")
#   ,
#   nrow = 1,
#   top = textGrob("Figure 1.1 Youth Literacy Rate Distribution \n", gp = gpar(fontsize = 15))   #adding title
# )



# data visualisation of secondary enrollment rate distributions
# grid.arrange(
#   ggplot(data = main_data_frame_secondary %>% filter(Year == 2019)) +
#     aes(x = Secondary_Enrollment_Rate) +
#     geom_histogram(binwidth = 5) +
#     coord_cartesian(xlim = c(0, 160)) +
#     ylim(0, 18) +
#     xlab("Enrollment Rate (%)") +
#     ylab("Frequency") +
#     ggtitle("2019")
#   ,
#   ggplot(data = main_data_frame_secondary %>% filter(Year == 2020)) +
#     aes(x = Secondary_Enrollment_Rate) +
#     geom_histogram(binwidth = 5) +
#     coord_cartesian(xlim = c(0, 160)) +
#     ylim(0, 18) +
#     xlab("Enrollment Rate (%)") +
#     ylab("Frequency") +
#     ggtitle("2020")
#   ,
#   ggplot(data = main_data_frame_secondary %>% filter(Year == 2021)) +
#     aes(x = Secondary_Enrollment_Rate) +
#     geom_histogram(binwidth = 5) +
#     coord_cartesian(xlim = c(0, 160)) +
#     ylim(0, 18) +
#     xlab("Enrollment Rate (%)") +
#     ylab("Frequency") +
#     ggtitle("2021")
#   ,
#   nrow = 1,
#   top = textGrob("Figure 1.2 Secondary School Enrollment Rate Distribution \n", gp = gpar(fontsize = 15))   #adding title
# )



# data visualization of GDP distributions
# grid.arrange(
#   ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2019)) +
#     aes(x = GDP_th) +
#     geom_histogram(binwidth = 5) +
#     xlab("Youth Literacy Rate (%)") +
#     ylab("Frequency") +
#     coord_cartesian(xlim = c(-0, 240)) +
#     ylim(0, 60) +
#     ggtitle("2019")
#   ,
#   ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2020)) +
#     aes(x = GDP_th) +
#     geom_histogram(binwidth = 5) +
#     xlab("Youth Literacy Rate (%)") +
#     ylab("Frequency") +
#     coord_cartesian(xlim = c(0, 240)) +
#     ylim(0, 60) +
#     ggtitle("2020")
#   ,
#   ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2021)) +
#     aes(x = GDP_th) +
#     geom_histogram(binwidth = 5) +
#     xlab("Youth Literacy Rate (%)") +
#     ylab("Frequency") +
#     coord_cartesian(xlim = c(0, 240)) +
#     ylim(0, 60) +
#     ggtitle("2021")
#   ,
#   nrow = 1,
#   top = textGrob("Figure 1.3 GDP per capita Distribution \n", gp = gpar(fontsize = 15))   #adding title
# )



# data visualisation of COVID distributions - no 2019 because it's all 0 anyways
# grid.arrange(
#   ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2020)) +
#     aes(x = Cases_prop) +
#     geom_histogram(binwidth = 1) +
#     coord_cartesian(xlim = c(0, 25)) +
#     ylim(0, 120) +
#     xlab("Number of New COVID-19 Cases") +
  #   ylab("Frequency") +
  #   ggtitle("2020")
  # ,
  # ggplot(data = main_data_frame_youthliteracy %>% filter(Year == 2021)) +
  #   aes(x = Cases_prop) +
  #   geom_histogram(binwidth = 1) +
  #   coord_cartesian(xlim = c(0, 25)) +
  #   ylim(0, 120) +
#     xlab("Number of New COVID-19 Cases") +
#     ylab("Frequency") +
#     ggtitle("2021")
#   ,
#   nrow = 1,
#   top = textGrob("Figure 1.4 New COVID-19 Cases Distribution \n", gp = gpar(fontsize = 15))   #adding title
# )



#table of top performing countries for each variable - youth literacy rate
top_literacy <- bind_rows(    #binding top performing countries' rows altogether
  education %>% filter(Year == 2019) %>% arrange(desc(Youth_Literacy_Rate)) %>% select(Country_Name, Year, Youth_Literacy_Rate) %>% head(10) %>% mutate(Rank = rank(desc(Youth_Literacy_Rate))),
  education %>% filter(Year == 2020) %>% arrange(desc(Youth_Literacy_Rate)) %>% select(Country_Name, Year, Youth_Literacy_Rate) %>% head(10) %>% mutate(Rank = rank(desc(Youth_Literacy_Rate))),
  education %>% filter(Year == 2021) %>% arrange(desc(Youth_Literacy_Rate)) %>% select(Country_Name, Year, Youth_Literacy_Rate) %>% head(10) %>% mutate(Rank = rank(desc(Youth_Literacy_Rate)))
)

literacy2019 = top_literacy[c(1:10),]    #collecting data for each year - used for final table
literacy2020 = top_literacy[c(11:20),]
literacy2021 = top_literacy[c(21:30),]

literacy2019 = literacy2019[,c(4, 1, 3)]   #reordering columns
literacy2020 = literacy2020[,c(4, 1, 3)]
literacy2021 = literacy2021[,c(4, 1, 3)]

colnames(literacy2019) = c("Rank", "2019_Country", "2019_Youth_Literacy_Rate")   #renaming the columns
colnames(literacy2020) = c("Rank", "2020_Country", "2020_Youth_Literacy_Rate")
colnames(literacy2021) = c("Rank", "2021_Country", "2021_Youth_Literacy_Rate")

literacytable = left_join(literacy2019, literacy2020, by = c("Rank" = "Rank"))     #join into one table
literacytable = left_join(literacytable, literacy2021, by = c("Rank" = "Rank"))

stargazer(literacytable, type = "text",
          summary = F,
          header = F,
          rownames = F,
          title = "Figure 2.1 : Top 10 Youth Literacy Rate Performances per Year")


#table of top performing countries for each variable - secondary enrollment rate
top_enrollment <- bind_rows(    #binding top performing countries' rows altogether
  education %>% filter(Year == 2019) %>% arrange(desc(Secondary_Enrollment_Rate)) %>% select(Country_Name, Year, Secondary_Enrollment_Rate) %>% head(10) %>% mutate(Rank = rank(desc(Secondary_Enrollment_Rate))),
  education %>% filter(Year == 2020) %>% arrange(desc(Secondary_Enrollment_Rate)) %>% select(Country_Name, Year, Secondary_Enrollment_Rate) %>% head(10) %>% mutate(Rank = rank(desc(Secondary_Enrollment_Rate))),
  education %>% filter(Year == 2021) %>% arrange(desc(Secondary_Enrollment_Rate)) %>% select(Country_Name, Year, Secondary_Enrollment_Rate) %>% head(10) %>% mutate(Rank = rank(desc(Secondary_Enrollment_Rate)))
)

enrollment2019 = top_enrollment[c(1:10),]    #collecting data for each year - used for final table
enrollment2020 = top_enrollment[c(11:20),]
enrollment2021 = top_enrollment[c(21:30),]

enrollment2019 = enrollment2019[,c(4, 1, 3)]   #reordering columns
enrollment2020 = enrollment2020[,c(4, 1, 3)]
enrollment2021 = enrollment2021[,c(4, 1, 3)]

colnames(enrollment2019) = c("Rank", "2019_Country", "2019_Enrollment_Rate")   #renaming the columns
colnames(enrollment2020) = c("Rank", "2020_Country", "2020_Enrollment_Rate")
colnames(enrollment2021) = c("Rank", "2021_Country", "2021_Enrollment_Rate")

enrollment_table = left_join(enrollment2019, enrollment2020, by = c("Rank" = "Rank"))     #join into one table
enrollment_table = left_join(enrollment_table, enrollment2021, by = c("Rank" = "Rank"))

stargazer(enrollment_table, type = "text",
          summary = F,
          header = F,
          rownames = F,
          title = "Figure 2.2 : Top 10 Secondary Enrollment Rate (Gross) Performances per Year")


#table of top performing countries for each variable - GDP_pc
top_GDPpc <- bind_rows(    #binding top performing countries' rows altogether
  wealth %>% filter(Year == 2019) %>% arrange(desc(GDP_pc)) %>% select(Country_Name, Year, GDP_pc) %>% head(10) %>% mutate(Rank = rank(desc(GDP_pc))),
  wealth %>% filter(Year == 2020) %>% arrange(desc(GDP_pc)) %>% select(Country_Name, Year, GDP_pc) %>% head(10) %>% mutate(Rank = rank(desc(GDP_pc))),
  wealth %>% filter(Year == 2021) %>% arrange(desc(GDP_pc)) %>% select(Country_Name, Year, GDP_pc) %>% head(10) %>% mutate(Rank = rank(desc(GDP_pc)))
)

GDP2019 = top_GDPpc[c(1:10),]    #collecting data for each year - used for final table
GDP2020 = top_GDPpc[c(11:20),]
GDP2021 = top_GDPpc[c(21:30),]

GDP2019 = GDP2019[,c(4, 1, 3)]   #reordering columns
GDP2020 = GDP2020[,c(4, 1, 3)]
GDP2021 = GDP2021[,c(4, 1, 3)]

colnames(GDP2019) = c("Rank", "2019_Country", "2019_GDP_pc")   #renaming the columns
colnames(GDP2020) = c("Rank", "2020_Country", "2020_GDP_pc")
colnames(GDP2021) = c("Rank", "2021_Country", "2021_GDP_pc")

GDP_table = left_join(GDP2019, GDP2020, by = c("Rank" = "Rank"))     #join into one table
GDP_table = left_join(GDP_table, GDP2021, by = c("Rank" = "Rank"))

stargazer(GDP_table, type = "text",
          summary = F,
          header = F,
          rownames = F,
          title = "Figure 2.3 : Top 10 GDP per capita Performances per Year")



#table of top performing countries for each variable - COVID
tmp = covid_prop[,-2]
top_COVID <- bind_rows(    #binding top performing countries' rows altogether
  tmp %>% filter(Year == 2020) %>% arrange(desc(Cases_prop)) %>% select(Country, Year, Cases_prop) %>% head(10) %>% mutate(Rank = rank(desc(Cases_prop))),
  tmp %>% filter(Year == 2021) %>% arrange(desc(Cases_prop)) %>% select(Country, Year, Cases_prop) %>% head(10) %>% mutate(Rank = rank(desc(Cases_prop)))
)

COVID2020 = top_COVID[c(1:10),]    #collecting data for each year - used for final table
COVID2021 = top_COVID[c(11:20),]

COVID2020 = COVID2020[,c(4, 1, 3)]   #reordering columns
COVID2021 = COVID2021[,c(4, 1, 3)]

colnames(COVID2020) = c("Rank", "2020_Country", "2020_cases(%)")   #renaming the columns
colnames(COVID2021) = c("Rank", "2021_Country", "2021_cases(%)")

COVID_table = left_join(COVID2020, COVID2021, by = c("Rank" = "Rank"))     #join into one table

stargazer(COVID_table, type = "text",
          summary = F,
          header = F,
          rownames = F,
          title = "Figure 2.4 : Top 10 COVID cases (Proportion of Population) per Year")


# creating scatterplots for literacy rate (for highest, lowest 25% in terms of GDP pc)
#figuring out how many countries there are - finding out how many countries account for 25%
length(unique(main_data_frame_youthliteracy$Country))*.25   #58

#finding out which countries are the top/bottom 25%
average_GDPpc = main_data_frame_youthliteracy %>%
  group_by(Country_code) %>%
  summarise(Average_GDP_pc = mean(GDP_pc, na.rm = TRUE)) %>%
  arrange(desc(Average_GDP_pc))

topGDP = average_GDPpc %>% head(58)   #top 25%
lowGDP = average_GDPpc %>% arrange(Average_GDP_pc) %>% head(58)   #lowest 25%

#subset data for top/lowest countries only
datafortop_literacy = main_data_frame_youthliteracy %>% filter(Country_code %in% topGDP$Country_code) %>% filter(Cases_prop != 0)
dataforlow_literacy = main_data_frame_youthliteracy %>% filter(Country_code %in% lowGDP$Country_code) %>% filter(Cases_prop != 0)

#get the best fit line
lr_toplit = lm(Youth_Literacy_Rate ~ Cases_prop, datafortop_literacy)
lr_lowlit = lm(Youth_Literacy_Rate ~ Cases_prop, dataforlow_literacy)

#plot the scatterplot for literacy data
grid.arrange(
  ggplot(datafortop_literacy, aes(x = Cases_prop, y = Youth_Literacy_Rate)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "black") +
    labs(title = "Top 25% GDP_pc Performers",
         x = "COVID Cases Proportion to Population",
         y = "Youth Literacy Rate")
  ,
  ggplot(dataforlow_literacy, aes(x = Cases_prop, y = Youth_Literacy_Rate)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "black") +
    labs(title = "Bottom 25% GDP_pc Performers",
         x = "COVID Cases Proportion to Population",
         y = "Youth Literacy Rate")
  ,
  nrow = 1,
  top = textGrob("Figure 3.1: COVID Cases (Proportion) vs Youth Literacy Rate \n", gp = gpar(fontsize = 15))   #adding title
  
)

#summary of linear regression
summary(lr_toplit)
summary(lr_lowlit)


#scatterplot for enrollment rate
#subset data for top/lowest countries only
datafortop_enrollment = main_data_frame_secondary %>% filter(Country_code %in% topGDP$Country_code) %>% filter(Cases_prop != 0)
dataforlow_enrollment = main_data_frame_secondary %>% filter(Country_code %in% lowGDP$Country_code) %>% filter(Cases_prop != 0)

#get the best fit line
enrollment_toplit = lm(Secondary_Enrollment_Rate ~ Cases_prop, datafortop_enrollment)
enrollment_lowlit = lm(Secondary_Enrollment_Rate ~ Cases_prop, dataforlow_enrollment)

#plot the scatterplot for enrollment rate
grid.arrange(
  ggplot(datafortop_enrollment, aes(x = Cases_prop, y = Secondary_Enrollment_Rate)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "black") +
    labs(title = "Top 25% GDP_pc Performers",
         x = "COVID Cases Proportion to Population",
         y = "Secondary Enrollment Rate")
  ,
  ggplot(dataforlow_enrollment, aes(x = Cases_prop, y = Secondary_Enrollment_Rate)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "black") +
    labs(title = "Bottom 25% GDP_pc Performers",
         x = "COVID Cases Proportion to Population",
         y = "Secondary Enrollment Rate")
  ,
  nrow = 1,
  top = textGrob("Figure 3.2: COVID Cases (Proportion) vs Secondary Enrollment Rate (Gross) \n", gp = gpar(fontsize = 15))   #adding title
  
)

#summary of linear regression
summary(enrollment_toplit)
summary(enrollment_lowlit)



# obtaining linear regression with interaction effect and all covariates
model_youthliteracy = lm(Youth_Literacy_Rate ~ GDP_pc + Cases_prop + GDP_pc*Cases_prop +
                           GNI_pc + National_income_pc + Urban_pop + Gross_savings + Unemployment_rate + 
                           Net_primary_income, data = main_data_frame_youthliteracy)

model_secondary = lm(Secondary_Enrollment_Rate ~ GDP_pc + Cases_prop + GDP_pc*Cases_prop +
                       GNI_pc + National_income_pc + Urban_pop + Gross_savings + Unemployment_rate + 
                       Net_primary_income, data = main_data_frame_secondary)


summary(model_youthliteracy)
summary(model_secondary)













