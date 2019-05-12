library(shiny)
library(ggsn)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(igraph)
library(igraphdata)

immigrants_nationality <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/immigrants_by_nationality.csv?token=AJLWINQL6MYBRGTSDQ6OYW2424SLU")
uniq <- unique(immigrants_nationality$Nationality)
#typeof(uniq)

#immigrants_nationality
#colnames(uniq)[which(names(uniq) == "Nationality")] <- "region"

#colnames(uniq)

not_shared <- c("Brasil", "United States", "United Kingdom", "Camerun", "State of Palestine", "Congo", "Trinidad and Tobago", "Cambodja", "The Bahamas", "São Tomé and Príncipe", "Antigua and Barbuda", "Saint Kitts and Nevis", "East Timor", "Palestinian territories")
not_shared <- c("Brazil", "USA", "UK", "Cameroon", "Palestine", "Republic of Congo", "Trinidad", "Tobago", "Cambodia", "Bahamas", "São Tomé and Príncipe", "Antigua", "Barbuda", "Saint Kitts", "Nevis", "Timor-Leste", "Palestinian territories")


countries <- map_data("world2")
uniq <- unique(immigrants_nationality$Nationality)
country_cords <- data.frame()
for (i in 1:length(uniq)){
  curr.country <- uniq[i]
  curr.country.data <- countries[which(countries$region == curr.country),]
  curr.avg.lat <- mean(curr.country.data$lat)
  curr.avg.long <- mean(curr.country.data$long)
  curr.avg.grp <- round(median(curr.country.data$group))
  curr.dist <- data.frame(Country = curr.country, Lat = curr.avg.lat, long =  curr.avg.long, group = curr.avg.grp)
  country_cords <- rbind(country_cords, curr.dist)
}

# Manually fill in entries not found



country_cords[which(country_cords$Country == 'Brasil'),2] <- mean(countries[which(countries$region == "Brazil"),2])
country_cords[which(country_cords$Country == 'Brasil'),3] <- mean(countries[which(countries$region == "Brazil"),1])
country_cords[which(country_cords$Country == 'Brasil'),4] <- round(median(countries[which(countries$region == "Brazil"),3]))


country_cords[which(country_cords$Country == 'United States'),2] <- mean(countries[which(countries$region == "USA"),2])
country_cords[which(country_cords$Country == 'United States'),3] <- mean(countries[which(countries$region == "USA"),1])
country_cords[which(country_cords$Country == 'United States'),4] <- round(median(countries[which(countries$region == "USA"),3]))

country_cords[which(country_cords$Country == 'United Kingdom'),2] <- mean(countries[which(countries$region == "UK"),2])
country_cords[which(country_cords$Country == 'United Kingdom'),3] <- mean(countries[which(countries$region == "UK"),1])
country_cords[which(country_cords$Country == 'United Kingdom'),4] <- round(median(countries[which(countries$region == "UK"),3]))

country_cords[which(country_cords$Country == 'Camerun'),2] <- mean(countries[which(countries$region == "Cameroon"),2])
country_cords[which(country_cords$Country == 'Camerun'),3] <- mean(countries[which(countries$region == "Cameroon"),1])
country_cords[which(country_cords$Country == 'Camerun'),4] <- round(median(countries[which(countries$region == "Cameroon"),3]))

country_cords[which(country_cords$Country == 'State of Palestine'),2] <- mean(countries[which(countries$region == "Palestine"),2])
country_cords[which(country_cords$Country == 'State of Palestine'),3] <- mean(countries[which(countries$region == "Palestine"),1])
country_cords[which(country_cords$Country == 'State of Palestine'),4] <- round(median(countries[which(countries$region == "Palestine"),3]))

country_cords[which(country_cords$Country == 'Congo'),2] <- mean(countries[which(countries$region == "Republic of Congo"),2])
country_cords[which(country_cords$Country == 'Congo'),3] <- mean(countries[which(countries$region == "Republic of Congo"),1])
country_cords[which(country_cords$Country == 'Congo'),4] <- round(median(countries[which(countries$region == "Republic of Congo"),3]))

country_cords[which(country_cords$Country == 'Cambodja'),2] <- mean(countries[which(countries$region == "Cambodia"),2])
country_cords[which(country_cords$Country == 'Cambodja'),3] <- mean(countries[which(countries$region == "Cambodia"),1])
country_cords[which(country_cords$Country == 'Cambodja'),4] <- round(median(countries[which(countries$region == "Cambodia"),3]))

country_cords[which(country_cords$Country == 'The Bahamas'),2] <- mean(countries[which(countries$region == "Bahamas"),2])
country_cords[which(country_cords$Country == 'The Bahamas'),3] <- mean(countries[which(countries$region == "Bahamas"),1])
country_cords[which(country_cords$Country == 'The Bahamas'),4] <- round(median(countries[which(countries$region == "Bahamas"),3]))

country_cords[which(country_cords$Country == 'East Timor'),2] <- mean(countries[which(countries$region == "Timor-Leste"),2])
country_cords[which(country_cords$Country == 'East Timor'),3] <- mean(countries[which(countries$region == "Timor-Leste"),1])
country_cords[which(country_cords$Country == 'East Timor'),4] <- round(median(countries[which(countries$region == "Timor-Leste"),3]))


country_cords[which(country_cords$Country == 'Trinidad and Tobago'),2] <- mean(countries[which(countries$region == "Trinidad" | countries$region == "Tobago"),2])
country_cords[which(country_cords$Country == 'Trinidad and Tobago'),3] <- mean(countries[which(countries$region == "Trinidad" | countries$region == "Tobago"),1])
country_cords[which(country_cords$Country == 'Trinidad and Tobago'),4] <- round(median(countries[which(countries$region == "Trinidad" | countries$region == "Tobago"),3]))

country_cords[which(country_cords$Country == 'Saint Kitts and Nevis'),2] <- mean(countries[which(countries$region == "Saint Kitts" | countries$region == "Nevis"),2])
country_cords[which(country_cords$Country == 'Saint Kitts and Nevis'),3] <- mean(countries[which(countries$region == "Saint Kitts" | countries$region == "Nevis"),1])
country_cords[which(country_cords$Country == 'Saint Kitts and Nevis'),4] <- round(median(countries[which(countries$region == "Saint Kitts" | countries$region == "Nevis"),3]))


country_cords[which(country_cords$Country == 'Antigua and Barbuda'),2] <- mean(countries[which(countries$region == "Antigua" | countries$region == "Barbuda"),2])
country_cords[which(country_cords$Country == 'Antigua and Barbuda'),3] <- mean(countries[which(countries$region == "Antigua" | countries$region == "Barbuda"),1])
country_cords[which(country_cords$Country == 'Antigua and Barbuda'),4] <- round(median(countries[which(countries$region == "Antigua" | countries$region == "Barbuda"),3]))

immigrants_nat.2017 <- immigrants_nationality[which(immigrants_nationality$Year == 2017),]
nation_list <- unique(immigrants_nat.2017$Nationality)


nation.immigrants_nat.2017 <- data.frame()

for (i in 1:length(nation_list)){
  curr.country <- nation_list[i]
  curr.country.data <- immigrants_nat.2017[which(immigrants_nat.2017$Nationality == curr.country),]
  curr.population.sum <-  sum(curr.country.data$Number)
  curr.data.frame <- data.frame(Country = curr.country, population = curr.population.sum)
  nation.immigrants_nat.2017 <- rbind(nation.immigrants_nat.2017,curr.data.frame)
}

all.immigrants.2017.data <- merge(nation.immigrants_nat.2017,country_cords, by = intersect("Country", "Country"))

# Converts a numeric array to an array of latitude labels
latitude <- function(x)
  paste0(format(x, digits = 3), "° ", ifelse(x > 0, "N", "S"))

# Converts a numeric array to an array of longitude labels
longitude <- function(x) 
  paste0(format(x, digits = 3), "° ", ifelse(x > 0, "E", "W"))

barcelona_immigrants.2017.map <- ggplot(all.immigrants.2017.data, 
                                        aes(x = long,
                                            y = Lat,
                                            group = group,
                                            fill = population)) +
              geom_polygon(aes(x = long + 0.005, y = Lat - 0.002),
                           color = "grey50", size = 0.2, fill = "grey50") +
              geom_polygon(color = "gray10", size = 0.2) +
              coord_equal() +
              labs(title = "Choropleth Map: Barcelona Immigrants Nationality",
                   subtitle = "Year: 2017",
                   fill = NULL) +
              theme_void() +
              theme(panel.background = element_rect(fill = NA, colour = "#cccccc"),
                    text = element_text(family = "Arial Narrow", size = 8),
                    plot.title = element_text(size = 12, face = "bold"),
                    plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
                    panel.border = element_rect(fill = NA, colour = "#cccccc"),
                    legend.text = element_text(size = 8),
                    legend.position = "bottom") +
              scale_y_continuous(labels = latitude) +
              scale_x_continuous(labels = longitude) +
              theme(axis.text = element_text(size = 8, colour = "#333333"))
              ggsn::north(all.immigrants.2017.data, location = "bottomright", symbol = 10)
            
barcelona_immigrants.2017.map

