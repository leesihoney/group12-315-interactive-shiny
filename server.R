library(shiny)
library(ggsn)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(igraph)
library(igraphdata)
library(plotly)
library(ggthemes)
library(ggdendro)
library(forcats)
library(wordcloud)
library(dendextend)



# dataset
accidents <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/accidents_2017.csv?token=AI2P3NBRJNC7OMNYFPNTPGS42MGNA")
accidents <- accidents[complete.cases(accidents),]
accidents$Weekday <- as.factor(accidents$Weekday)
accidents$Month <- as.factor(accidents$Month)
accidents$Day_of_Month <- accidents$Day
accidents$Time_of_Day <-accidents$Part.of.the.day
accidents$Month <-recode(accidents$Month, 'January'= 1, 'February' = 2, 'March' = 3, 'April' = 4,'May' = 5, 'June' = 6, 'July' = 7, 'August' = 8, 'September' = 9, 'October' = 10, 'November' = 11, 'December' = 12) 
accidents$Date <- as.Date(with(accidents, paste(2016, Month, Day, sep = "-")), "%Y-%m-%d")
accidents <- mutate(accidents,
                    timegroup = factor(ifelse(accidents$Date <= "2016-04-01", "Before 4/1/16", ifelse(accidents$Date > "2016-04-01" & accidents$Date <= "2016-07-01", "Between 4/1/16 and 7/1/16", ifelse(accidents$Date > "2016-07-01" & accidents$Date <= "2016-10-01", "Between 7/1/16 and 10/1/16", ifelse(accidents$Date > "2016-10-01", "After 10/1/16", NA))))))
accidents$facet = factor(accidents$timegroup, levels = c("Before 4/1/16", "Between 4/1/16 and 7/1/16", "Between 7/1/16 and 10/1/16", "After 10/1/16"))


deaths <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/deaths.csv?token=AI2P3NGRMDEN4XGYVNIXGT243MKDE")
population <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/population.csv?token=AI2P3NCUW6KGWTSS3WJZTG243MFBI")
unemployment <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/unemployment.csv?token=AI2P3NAFMM7Z3MPU4MZNWUS43MFDW")
births <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/births.csv?token=AI2P3NBX2365P3KI3ZEEVNS43MPSC")



population.by.district <- population %>% group_by(District.Name) %>% summarise(sum(Number))
deaths.by.district <- deaths %>% group_by(District.Name) %>% summarise(sum(Number))
births.by.district <- births %>% group_by(District.Name) %>% summarise(sum(Number))
unemployment.by.district <- unemployment %>% group_by(District.Name) %>% summarise(mean(Number))
death.population<- inner_join(deaths.by.district, population.by.district, by = "District.Name")
death.population.births <- inner_join(death.population, births.by.district,by = "District.Name")

collated.data <- inner_join(death.population.births, unemployment.by.district,by = "District.Name")
colnames(collated.data) <- c("District.Name", "Death","Population","Birth","Unemployment")

collated.data.rates <- data.frame(District.Name = collated.data$District.Name, 
                                  Death = collated.data$Death/collated.data$Population,
                                  Birth = collated.data$Birth/collated.data$Population,
                                  Unemployment = collated.data$Unemployment/collated.data$Population)

row.names(collated.data.rates) <- collated.data.rates$District.Name

immigrants_nationality <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/immigrants_by_nationality.csv?token=AJLWINQL6MYBRGTSDQ6OYW2424SLU")
baby.names <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/most_frequent_baby_names.csv?token=AI2P3NG2QNKS54QSUDRRJPC43NSZY")
baby.names$Year <- as.character(baby.names$Year)
uniq <- unique(immigrants_nationality$Nationality)
not_shared <- c("Brasil", "United States", "United Kingdom", "Camerun", "State of Palestine", "Congo", "Trinidad and Tobago", "Cambodja", "The Bahamas", "São Tomé and Príncipe", "Antigua and Barbuda", "Saint Kitts and Nevis", "East Timor", "Palestinian territories")
not_shared <- c("Brazil", "USA", "UK", "Cameroon", "Palestine", "Republic of Congo", "Trinidad", "Tobago", "Cambodia", "Bahamas", "São Tomé and Príncipe", "Antigua", "Barbuda", "Saint Kitts", "Nevis", "Timor-Leste", "Palestinian territories")
countries <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()
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
#country_cords$Country[country_cords$Country == "Brasil"] <- "Brazil"


country_cords[which(country_cords$Country == 'United States'),2] <- mean(countries[which(countries$region == "USA"),2])
country_cords[which(country_cords$Country == 'United States'),3] <- mean(countries[which(countries$region == "USA"),1])
country_cords[which(country_cords$Country == 'United States'),4] <- round(median(countries[which(countries$region == "USA"),3]))
#country_cords$Country[country_cords$Country == "United States"] <- "USA"


country_cords[which(country_cords$Country == 'United Kingdom'),2] <- mean(countries[which(countries$region == "UK"),2])
country_cords[which(country_cords$Country == 'United Kingdom'),3] <- mean(countries[which(countries$region == "UK"),1])
country_cords[which(country_cords$Country == 'United Kingdom'),4] <- round(median(countries[which(countries$region == "UK"),3]))
#country_cords$Country[country_cords$Country == "United Kingdom"] <- "UK"


country_cords[which(country_cords$Country == 'Camerun'),2] <- mean(countries[which(countries$region == "Cameroon"),2])
country_cords[which(country_cords$Country == 'Camerun'),3] <- mean(countries[which(countries$region == "Cameroon"),1])
country_cords[which(country_cords$Country == 'Camerun'),4] <- round(median(countries[which(countries$region == "Cameroon"),3]))
#country_cords$Country[country_cords$Country == "Camerun"] <- "Cameroon"


country_cords[which(country_cords$Country == 'State of Palestine'),2] <- mean(countries[which(countries$region == "Palestine"),2])
country_cords[which(country_cords$Country == 'State of Palestine'),3] <- mean(countries[which(countries$region == "Palestine"),1])
country_cords[which(country_cords$Country == 'State of Palestine'),4] <- round(median(countries[which(countries$region == "Palestine"),3]))
#country_cords$Country[country_cords$Country == "State of Palestine"] <- "Palestine"

country_cords[which(country_cords$Country == 'Congo'),2] <- mean(countries[which(countries$region == "Republic of Congo"),2])
country_cords[which(country_cords$Country == 'Congo'),3] <- mean(countries[which(countries$region == "Republic of Congo"),1])
country_cords[which(country_cords$Country == 'Congo'),4] <- round(median(countries[which(countries$region == "Republic of Congo"),3]))
#country_cords$Country[country_cords$Country == "Congo"] <- "Republic of Congo"

country_cords[which(country_cords$Country == 'Cambodja'),2] <- mean(countries[which(countries$region == "Cambodia"),2])
country_cords[which(country_cords$Country == 'Cambodja'),3] <- mean(countries[which(countries$region == "Cambodia"),1])
country_cords[which(country_cords$Country == 'Cambodja'),4] <- round(median(countries[which(countries$region == "Cambodia"),3]))
#country_cords$Country[country_cords$Country == "Cambodja"] <- "Cambodia"


country_cords[which(country_cords$Country == 'The Bahamas'),2] <- mean(countries[which(countries$region == "Bahamas"),2])
country_cords[which(country_cords$Country == 'The Bahamas'),3] <- mean(countries[which(countries$region == "Bahamas"),1])
country_cords[which(country_cords$Country == 'The Bahamas'),4] <- round(median(countries[which(countries$region == "Bahamas"),3]))
#country_cords$Country[country_cords$Country == "The Bahama"] <- "Bahamas"

country_cords[which(country_cords$Country == 'East Timor'),2] <- mean(countries[which(countries$region == "Timor-Leste"),2])
country_cords[which(country_cords$Country == 'East Timor'),3] <- mean(countries[which(countries$region == "Timor-Leste"),1])
country_cords[which(country_cords$Country == 'East Timor'),4] <- round(median(countries[which(countries$region == "Timor-Leste"),3]))
#country_cords$Country[country_cords$Country == "East Timor"] <- "Timor-Leste"


country_cords[which(country_cords$Country == 'Trinidad and Tobago'),2] <- mean(countries[which(countries$region == "Trinidad" | countries$region == "Tobago"),2])
country_cords[which(country_cords$Country == 'Trinidad and Tobago'),3] <- mean(countries[which(countries$region == "Trinidad" | countries$region == "Tobago"),1])
country_cords[which(country_cords$Country == 'Trinidad and Tobago'),4] <- round(median(countries[which(countries$region == "Trinidad" | countries$region == "Tobago"),3]))
#country_cords$Country[country_cords$Country == "Trinidad and Tobago"] <- "Trinidad"


country_cords[which(country_cords$Country == 'Saint Kitts and Nevis'),2] <- mean(countries[which(countries$region == "Saint Kitts" | countries$region == "Nevis"),2])
country_cords[which(country_cords$Country == 'Saint Kitts and Nevis'),3] <- mean(countries[which(countries$region == "Saint Kitts" | countries$region == "Nevis"),1])
country_cords[which(country_cords$Country == 'Saint Kitts and Nevis'),4] <- round(median(countries[which(countries$region == "Saint Kitts" | countries$region == "Nevis"),3]))
#country_cords$Country[country_cords$Country == "Saint Kitts and Nevis"] <- "Saint Kitts"


country_cords[which(country_cords$Country == 'Antigua and Barbuda'),2] <- mean(countries[which(countries$region == "Antigua" | countries$region == "Barbuda"),2])
country_cords[which(country_cords$Country == 'Antigua and Barbuda'),3] <- mean(countries[which(countries$region == "Antigua" | countries$region == "Barbuda"),1])
country_cords[which(country_cords$Country == 'Antigua and Barbuda'),4] <- round(median(countries[which(countries$region == "Antigua" | countries$region == "Barbuda"),3]))
#country_cords$Country[country_cords$Country == "Antigua and Barbuda"] <- "Antigua"


# Define server logic required to draw a histogram
server <- function(input, output) {
  # sort countries 
  sort_nation <- function(nation_list, cont_input) {
    new_df <- data.frame(Nationality = character())
    cont_nation_list <- c()

    if(cont_input %in% c("Asia")) {
      # print("In Asia")
      cont_nation_list <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain",
                            "Bangladesh", "Bhutan", "Brunei", "Burma", "Cambodja",
                            "China", "Cyprus", "Georgia", "Hong Kong", "India",
                            "Indonesia", "Iran", "Iraq", "Israel",
                            "Japan", "Jordan", "Kazakhstan", "South Korea",
                            "South Korea", "Kuwait", "Kyrgyzstan", "Laos",
                            "Lebanon", "Macau", "Malaysia", "Maldives", "Mongolia",
                            "Nepal", "Oman", "Pakistan", "Palestine", "Phillipines",
                            "Qatar", "Saudi Arabia", "Singapore", "Sri Lanka",
                            "Syria", "Taiwan", "Tajikistan", "Turkey",
                            "Turkmenistan", "United Arab Emirates", "Uzbekistan",
                            "Vietnam", "Yemen")
    } else if(cont_input %in% c("Europe")) {
      cont_nation_list <- c("Russia", "Germany", "United Kingdom", "France",
                            "Italy", "Spain", "Ukraine", "Poland", "Romania",
                            "Netherlands", "Belgium", "Greece", "Czech Republic", "Portugal",
                            "Sweden", "Hungary", "Belarus", "Austria",
                            "Serbia", "Switzerland", "Bulgaria", "Denmark",
                            "Finland", "Slovakia", "Norway", "Ireland",
                            "Croatia", "Moldova", "Bosnia and Herzegovina", "Albania", "Lithuania",
                            "Macedonia", "Slovenia", "Latvia", "Estonia", "Montenegro",
                            "Luxembourg", "Malta", "Iceland", "Andorra",
                            "Monaco", "Liechtenstein", "San Marino", "Holy See")
    } else if(cont_input %in% c("Africa")) {
      cont_nation_list <- c("Nigeria", "Ethiopia", "Egypt", "Congo",
                            "South Africa", "Tanzania", "Kenya", "Sudan", "Algeria",
                            "Uganda", "Morocco", "Mozambique", "Ghana", "Angola",
                            "Ivory Coast", "Madagascar", "Camerun", "Niger",
                            "Burkina Faso", "Mali", "Malawi", "Zambia",
                            "Somalia", "Senegal", "Chad", "Zimbabwe",
                            "Rwanda", "Tunisia", "Guinea", "Benin", "Burundi",
                            "South Sudan", "Togo", "Eritrea", "Sierra Leone", "Central African Republic",
                            "Liberia", "Mauritania", "Namibia", "Botswana",
                            "Lesotho", "Gambia", "Gabon", "Guinea-Bissau",
                            "Mauritius", "Equational Guinea", "Eswatini", "Djibouti",
                            "Réunion", "Comoros", "Cape Verde", "Western Sahara",
                            "Mayotte", "São Tomé and Príncipe", "Seychelles", "Saint Helena")
    } else if(cont_input %in% c("North America")) {
      cont_nation_list <- c("United States", "Mexico", "Canada", "Guatemala",
                            "Cuba", "Haiti", "Dominican Republic", "Honduras", "El Salvador",
                            "Costa Rica", "Panama", "Puerto Rico", "Jamaica", "Trinidad and Tobago",
                            "Guadeloupe", "Martinique", "Bahamas", "Belize",
                            "Barbados", "Saint Lucia", "Saint Vincent and the Grenadines", "United States Virgin Islands",
                            "Grenada", "Antigua and Barbuda", "Dominica", "Bermuda",
                            "Rwanda", "Tunisia", "Guinea", "Benin", "Burundi",
                            "Cayman Islands", "Greenland", "Saint Kitts and Nevis", "Sint Maarten", "Turks and Caicos Islands",
                            "Saint Martin", "British Virgin Islands", "Carribbean Netherlands", "Anguilla",
                            "Saint Barthélemy", "Saint Pierre and Miquelon", "Monserrat")
    } else if (cont_input %in% c("South America")) {
      cont_nation_list <- c("Argentina", "Bolivia", "Brasil", "Chile",
                            "Colombia", "Ecuador", "French Guiana", "Guyana",
                            "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
    } else if (cont_input %in% c("Oceania")) {
      cont_nation_list <- c("Austrailia", "New Zealand", "Norfolk Island", "Fiji",
                            "New Caledonia", "Papua", "West Papua", "Papua New Guinea",
                            "Solomon Islands", "Vanuatu", "Federal States of Micronesia", "Kiribati", "Marshall Islands",
                            "Nauru", "Palau", "Wake Island", "Samoa", "Tonga", "Tuvalu")
    } else {
      return(nation_list)
    }
    #print(cont_nation_list)
    for (i in 1:length(nation_list)) {
      curr.country <- nation_list[i]
      #print(curr.country)
      if(curr.country %in% cont_nation_list) {
        #print(curr.country)
        new_df <- rbind(new_df, data.frame(Nationality = curr.country))
        #new_df$Nationality[nrow(new_df)+1] <- curr.country
      }
    }
    #print(new_df)
    return(new_df)
    
  }
  
  
  
  # put into continents
  put_continents <- function(nation_list, immigrants_nat) {
    new_df <- data.frame()
    #print(immigrants_nat)
    if(typeof(nation_list) == "list") {
      compare <- nation_list$Nationality
    }
    else {
      compare <- nation_list
    }
    for (i in 1:length(compare)){
      curr.country <- compare[i]
      curr.country.data <- immigrants_nat[which(immigrants_nat$Nationality == curr.country),]
      curr.population.sum <-  sum(curr.country.data$Number)
      curr.data.frame <- data.frame(Country = curr.country, population = curr.population.sum)
      new_df <- rbind(new_df,curr.data.frame)
    }
    
    return(new_df)
  }
  make_dataset <- function(year_input, cont_input) {
    # pull out nationalities from the dataset of corresponding year
    immigrants_nat <- immigrants_nationality[which(immigrants_nationality$Year == year_input),]
    nation_list <- sort_nation(unique(immigrants_nat$Nationality), cont_input)
    # make a new dataframe with those inputs
    #print(nation_list)
    nation.immigrants_nat <- put_continents(nation_list, immigrants_nat)
    # setup country names to data
    all.immigrants.data <- merge(nation.immigrants_nat,country_cords, by = intersect("Country", "Country"))
    
    all.immigrants.data$Country <- as.character(all.immigrants.data$Country)
    all.immigrants.data$Country[all.immigrants.data$Country == "United States"] <- "USA"
    all.immigrants.data$Country[all.immigrants.data$Country == "Brasil"] <- "Brazil"
    all.immigrants.data$Country[all.immigrants.data$Country == "United Kingdom"] <- "UK"
    all.immigrants.data$Country[all.immigrants.data$Country == "Camerun"] <- "Cameroon"
    all.immigrants.data$Country[all.immigrants.data$Country == "State of Palestine"] <- "Palestine"
    all.immigrants.data$Country[all.immigrants.data$Country == "Cambodja"] <- "Cambodia"
    all.immigrants.data$Country[all.immigrants.data$Country == "The Bahamas"] <- "Bahamas"
    all.immigrants.data$Country[all.immigrants.data$Country == "East Timor"] <- "Timor-Leste"
    all.immigrants.data$Country[all.immigrants.data$Country == "Trinidad and Tobago"] <- "Trinidad"
    all.immigrants.data$Country[all.immigrants.data$Country == "Saint Kitts and Nevis"] <- "Saint Kitts"
    all.immigrants.data$Country[all.immigrants.data$Country == "Antigua and Barbuda"] <- "Antigua"
    
    colnames(countries)[which(names(countries) == "region")] <- "Country"
    
    # return only population number
    temp <- data.frame(Country = all.immigrants.data$Country, population = all.immigrants.data$population)
    new_data <- left_join(countries, temp, by = "Country")
    
    # Calculate quartiles to break into cuts
    quartile_calculation <- temp[order(temp$population), ]
    temp_median <- median(quartile_calculation$population)
    lower <- quartile_calculation[1:round(nrow(quartile_calculation)/2),] 
    upper <- quartile_calculation[round((nrow(quartile_calculation)/2)+1):nrow(quartile_calculation),] 
    lower_median <- median(lower$population)
    upper_median <- median(upper$population)
    cut_range <- c(0, lower_median, temp_median, upper_median, max(temp$population))
    new_data$pop.cut <- factor(
      cut(new_data$population, cut_range),
      labels = c(sprintf("Under %s", toString(lower_median)),
                 sprintf("%s to %s ", toString(lower_median), toString(temp_median)),
                 sprintf("%s to %s ", toString(temp_median), toString(upper_median)),
                 sprintf("Over %s", toString(upper_median))
      )
    )
    
    
    return(make_graph(new_data))
    
  }
  
  make_graph <- function(dataset) {
    choro_map <- ggplot(dataset, 
                        aes(x = long,
                            y = lat,
                            group = group,
                            fill = pop.cut)) +
      geom_polygon(aes(x = long + 0.005,
                       y = lat - 0.002),
                   color = "grey50", size = 0.2, fill = "grey50") +
      geom_polygon(color = "gray10", size = 0.2) +
      coord_equal() +
      labs(fill = "Population") +
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
      scale_fill_manual(values = c("#FFFF00", "#FFCC00", "#FF9933", "#FF0000")) +
      theme(axis.text = element_text(size = 8, colour = "#333333")) +
      ggsn::north(dataset, location = "bottomright", symbol = 10)
    return(choro_map)
  }
  
  # Converts a numeric array to an array of latitude labels
  latitude <- function(x)
    paste0(format(x, digits = 3), "° ", ifelse(x > 0, "N", "S"))
  
  # Converts a numeric array to an array of longitude labels
  longitude <- function(x) 
    paste0(format(x, digits = 3), "° ", ifelse(x > 0, "E", "W"))
  
  lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = accidents)})
  # histograms
  output$hist_Dep <- renderPlot({
    ggplot(accidents, aes(x = accidents[,input$DepVar])) +
      geom_histogram() + labs(x = input$DepVar)
  })
  output$Ind <- renderPlot({
    ggplot()+ aes(x = accidents[,input$IndVar]) + 
      geom_bar() + labs(x = input$IndVar)
  })
  
  # scatterplot
  output$scatterplot <- renderPlot({
    plot(x = accidents[,input$IndVar], y = accidents[,input$DepVar],
         main = "Scatterplot", xlab = input$IndVar,
         ylab = input$DepVar)
    abline(coef(lm1())[1], coef(lm1())[2], col = "red")
  }, height = 400)
  
  output$RegSum <- renderPrint({summary(lm1())})
  
  make_df <- function(year_input, cont_input) {
    WorldCountry <-geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
    # pull out nationalities from the dataset of corresponding year
    immigrants_nat <- immigrants_nationality[which(immigrants_nationality$Year == year_input),]
    nation_list <- sort_nation(unique(immigrants_nat$Nationality), cont_input)
    # make a new dataframe with those inputs
    #print(nation_list)
    all.immigrants.data <- put_continents(nation_list, immigrants_nat)
    # setup country names to data

    all.immigrants.data$Country <- as.character(all.immigrants.data$Country)
    all.immigrants.data$Country[all.immigrants.data$Country == "United States"] <- "United States of America"
    all.immigrants.data$Country[all.immigrants.data$Country == "Brasil"] <- "Brazil"
    all.immigrants.data$Country[all.immigrants.data$Country == "Camerun"] <- "Cameroon"
    all.immigrants.data$Country[all.immigrants.data$Country == "Cambodja"] <- "Cambodia"
    all.immigrants.data$Country[all.immigrants.data$Country == "Guinea-Bissau"] <- "Guinea Bissau"
    all.immigrants.data$Country[all.immigrants.data$Country == "Dominica"] <- "Dominican Republic"
    all.immigrants.data$Country[all.immigrants.data$Country == "Congo"] <- "Democratic Republic of the Congo"
    all.immigrants.data$Country[all.immigrants.data$Country == "Bahamas"] <- "The Bahamas"
    all.immigrants.data$Country[all.immigrants.data$Country == "Serbia"] <- "Republic of Serbia"
    all.immigrants.data <- all.immigrants.data %>% distinct(Country, .keep_all = TRUE)

    newobj <- merge(WorldCountry, all.immigrants.data, by.x="name", by.y="Country")
    return(newobj)
  }
  
  make_bins <- function(map_df) {
    # Calculate quartiles to break into cuts
    quartile_calculation <- na.omit(map_df[order(map_df$population), ])
    temp_median <- median(quartile_calculation$population)
    lower <- quartile_calculation[1:round(nrow(quartile_calculation)/2),] 
    upper <- quartile_calculation[round((nrow(quartile_calculation)/2)+1):nrow(quartile_calculation),] 
    lower_median <- median(lower$population)
    upper_median <- median(upper$population)
    return(c(0, lower_median, temp_median, upper_median, max(quartile_calculation$population)))
  }
  
  # choropleth maps
  output$chorop_map <-renderPlotly({
    ggplotly(make_dataset(input$yr, input$cont), tooltip = "All") 
  })
  
  output$leaflet_map <- renderLeaflet({
    df <- make_df(input$yr, input$cont)
    bins <- make_bins(df@data)
    pal <- colorBin("YlOrRd", domain = df@data$population, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>Population: %g",
      df@data$name, df@data$population) %>% lapply(htmltools::HTML)
    leaflet(df) %>% addTiles() %>% 
      addPolygons(
        fillColor = ~pal(df@data$population),
        weight = 1,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto") 
      ) %>%
      addLegend(pal = pal, values = df@data$population, opacity = 0.7, 
                title = "Population Range",  position = "bottomright")
  })

  lm2 <- reactive({lm(reformulate(input$DepVariable), data = accidents)})
  
  output$timeseries <- renderPlot({
    ggplot(accidents, aes(x = Date, y = accidents[,input$DepVariable])) +
      scale_x_date() + 
      geom_point() + 
      geom_line(aes(color = input$DepVariable)) +
      facet_wrap(~facet, ncol = 5, scales = "free_x") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(color = "Dependent Variable")
  })
  #output$RegSumTime <- renderPrint({summary(lm2())})


  #BabyName
  output$wordc <- renderPlot({
    wordcloud(baby.names[which(baby.names$Year== input$year & baby.names$Gender == input$sex),]$Name,
              baby.names[which(baby.names$Year== input$year & baby.names$Gender == input$sex),]$Frequency,
              random.color = TRUE, 
              max.words = 20,colors=brewer.pal(8, "Dark2"), 
              main = paste("Top 20", input$Gender, "Baby Names in", as.character(input$Year)))
  })
  
  output$barp <- renderPlotly({
    plot_ly(
      x = top_n(baby.names[which(baby.names$Year == input$year & baby.names$Gender == input$sex),], 
                5, 'Frequency')[1:20,]$Name ,
      y = top_n(baby.names[which(baby.names$Year == input$year & baby.names$Gender == input$sex),], 
                5, 'Frequency')[1:20,]$Frequency,
      name = "SF Zoo",
      type = "bar"
    )
  })
  
  output$Dendy <- renderPlot({
    plot(collated.data.rates[,c(input$var1,input$var2)] %>% scale %>% dist %>% hclust %>% as.dendrogram %>%
           set("labels_col", value = c("skyblue", "orange"), k=2) %>%
           set("branches_k_color", value = c("skyblue", "orange"), k = 2) %>%
           set("leaves_pch", 19)  %>% set("nodes_cex", 0.7) %>% set("nodes_col", c("forestgreen", "light blue")) %>%
           set("labels_cex", c(0.9)), main=paste("Hierarchical Clustering of",input$var1, "rates and", input$var2, "rates"),
         axes=FALSE)})
  
  
}




