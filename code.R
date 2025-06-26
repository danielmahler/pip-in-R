### INTRODUCTION ###
# This script contains the code behind the slides:
# https://danielmahler.quarto.pub/pip-in-r
  
  
### Installing the Stata pip.ado ###
  
# To install the pipr package, you first need to install the devtools package
install.packages("devtools")
# The devtools package allows you to install pipr through GitHub
devtools::install_github("worldbank/pipr")
# Now load pipr
library(pipr)
# We'll use dplyr and ggplot2 a lot in these slides, so best to load them as well
library(dplyr)
library(ggplot2)


### Counggplot2### Country-level estimates ###
get_stats() 


### Variable information ###
get_aux("dictionary")


### Ghanaian estimates ###
get_stats(country = "GHA") |> 
  select(country_code, year, poverty_line, headcount,mean,gini)


### Different poverty line ###
get_stats(country = "LUX", povline = 10)  |>    
  select(country_code, year, poverty_line, headcount)


### Multiple poverty liens ###
povlines <- c(10, 20, 50,100) 
purrr::map_dfr(.x      = povlines,                 
               .f      = get_stats,                 
               country = "LUX",                
               year    = 2022) |>    
  select(country_code, year, poverty_line, headcount)


### Estimates across selected countries ###
get_stats(country = c("LUX","GHA","VNM"), year=2022) |> 
  select(country_code, year, poverty_line, headcount, mean, gini)


### Interpolated and extrapolated values ###
get_stats(country = "GHA", fill_gaps = TRUE)  |> 
  filter(year>=2016) |>
  select(country_code, year, poverty_line, headcount)


### Poverty by welfare type ###
df <- get_stats(country="PHL")       
gr <- ggplot(df,aes(y=100*headcount,x=year,color=welfare_type)) +          
  geom_line() +                     
  labs(                          
    title = "Extreme poverty in the Philippines, 1981-2022",                       
    y = "Poverty rate (%)",                          
    x = ""                   
  )

gr


### Poverty by reporting level ###
df <- get_stats(country="CHN") |>
  filter(year>=1990)

gr <- ggplot(df,aes(y=100*headcount,x=year,color=reporting_level)) +
  geom_line() + 
  labs(
    title = "Extreme poverty in China, 1990-2022",
    y = "Poverty rate (%)",
    x = "")

gr


### Within-country comparability ###
df <- get_stats(country="CHN") |> 
  filter(reporting_level=="national" & year>=1990) |>
  select(year,headcount,comparable_spell,welfare_type)
print(df, n = 21)


### Data from LIS ###
df <- get_stats() |>
  filter(grepl("LIS", survey_acronym)) |>
  group_by(country_name) |>
  summarize(obs = n()) |>
  ungroup()
print(df, n=29)


### Global and regional estimates ###
get_wb()


### Global poverty, 1990-2022 ###
df <- get_wb() |>
  filter(year >= 1990, region_code %in% c("WLD","EAP","SSA"))

gr <- ggplot(df, aes(x = year,y = 100*headcount,color=region_code)) +
  geom_line() +
  labs(
    title = "Global poverty, 1990-2025",
    y = "Poverty rate (%)",
    x = ""
  )
