# Import Libraries
library(tidyverse)
library(WDI)
library(countrycode)
library(ggplot2)
library(devtools)
library(Rcpp)
library(gganimate)

# Download Data from World Bank Database
birth <- WDI(country="all", indicator=c("SP.DYN.CBRT.IN", "SP.DYN.IMRT.IN", "SP.POP.TOTL","NY.GDP.PCAP.CD"),    start=1980, end=2018) %>%
  rename(Birth_Rate=SP.DYN.CBRT.IN, Infant_Mortality=SP.DYN.IMRT.IN, Population1=SP.POP.TOTL, GDP_PC=NY.GDP.PCAP.CD)

# Remove data for non-country entities 
birth <- birth[-c(1:140),]

# Classify countries into continents for colour coding in animation
birth$Continent <- countrycode(sourcevar = birth[, "country"],
                               origin = "country.name",
                               destination = "continent")

# Remove data entries with value "NA"
birth <- na.omit(birth)

# Univariate transformation for individual datafields
birth$Population = 10*(birth$Population1)**0.25
birth$Inft_mort1 = (birth$Infant_Mortality)**0.4
birth$birth_rate1 = (birth$Birth_Rate)**1
birth$gdp_pc1 = (birth$GDP_PC)**0.1

# Creating a linear model of regression with GDPPC and INFMRT as independent variables
model <- lm(data = birth, birth$birth_rate1 ~ birth$Inft_mort1 + birth$gdp_pc1)
birth$model <- stats::predict(model, newdata=birth)
err <- stats::predict(model, newdata=birth, se = TRUE)
birth$ucl <- err$fit + 1.96 * err$se.fit #Define Upper Confidence Limit
birth$lcl <- err$fit - 1.96 * err$se.fit #Define Lower Confidence Limit

# Plotting the model to transition with time
theme_set(theme_bw())
p <- ggplot(birth)
p <- p + geom_point(aes(x=model, y = birth$birth_rate1, size = Population, color = Continent))
p <- p + geom_smooth(data=birth, aes(x=model, y = birth$birth_rate1, ymin=lcl, ymax=ucl), size = 1.5, 
                     colour = "red", se = TRUE, stat = "smooth")

p + transition_time(year) + 
  labs(title = "Birth Rate vs Infant Mortality Rate and GDP per Capita               Year: {frame_time}", x="Infant Mortality Rate & GDP per Capita", y="Birth Rate", "Year: {frame_time}") +
  ease_aes('cubic-in-out')


