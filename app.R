## this is final
library(tidyverse)
library(jsonlite)
library(httr)
library(rvest)
library(ggplot2)
library(gganimate)
library(ggpubr)
library(DT)
library(googleCharts)
library(dplyr)
library(shiny)
library(leaflet)
library(geojsonio)
library(countrycode)

###############
# get typical countries data

r3 <- RETRY("GET", "https://api.covid19api.com/total/country/united-states", pause_min = 2, times = 10)
json <- content(r3, as = "text", encoding = "UTF-8")
cumul_us <- fromJSON(json)
us_slug <- replicate(nrow(cumul_us), "US")

r4 <- RETRY("GET", "https://api.covid19api.com/total/country/canada", pause_min = 2, times = 10)
json <- content(r4, as = "text", encoding = "UTF-8")
cumul_ca <- fromJSON(json)
ca_slug <- replicate(nrow(cumul_ca), "CA")

r5 <- RETRY("GET", "https://api.covid19api.com/total/country/brazil", pause_min = 2, times = 10)
json <- content(r5, as = "text", encoding = "UTF-8")
cumul_br <- fromJSON(json)
br_slug <- replicate(nrow(cumul_br), "BR")

r6 <- RETRY("GET", "https://api.covid19api.com/total/country/italy", pause_min = 2, times = 10)
json <- content(r6, as = "text", encoding = "UTF-8")
cumul_it <- fromJSON(json)
it_slug <- replicate(nrow(cumul_it), "IT")

r7 <- RETRY("GET", "https://api.covid19api.com/total/country/russia", pause_min = 2, times = 10)
json <- content(r7, as = "text", encoding = "UTF-8")
cumul_ru <- fromJSON(json)
ru_slug <- replicate(nrow(cumul_ru), "RU")

r8 <- RETRY("GET", "https://api.covid19api.com/total/country/united-kingdom", pause_min = 2, times = 10)
json <- content(r8, as = "text", encoding = "UTF-8")
cumul_uk <- fromJSON(json)
uk_slug <- replicate(nrow(cumul_uk), "UK")

r9 <- RETRY("GET", "https://api.covid19api.com/total/country/china", pause_min = 2, times = 10)
json <- content(r9, as = "text", encoding = "UTF-8")
cumul_cn <- fromJSON(json)
cn_slug <- replicate(nrow(cumul_cn), "CN")

# get coutries abbre
Abbrev <- data.frame(Abbrev = c(us_slug, ca_slug, br_slug, it_slug, ru_slug, uk_slug, cn_slug))

data_total <- cumul_us %>%
    bind_rows(cumul_ca) %>%
    bind_rows(cumul_br) %>%
    bind_rows(cumul_it) %>%
    bind_rows(cumul_ru) %>%
    bind_rows(cumul_uk) %>%
    bind_rows(cumul_cn) %>%
    select(-CountryCode:-Lon)
data_total <- data_total %>% bind_cols(Abbrev)


time <- data_total$Date
time1 <- str_extract(time, "(\\d{4}-\\d{2}-\\d{2})")
time_date1 <- as.Date(time1, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

# all typical contries data in one dataframe
data_total <- data_total %>%
    bind_cols(time_date = time_date1) %>%
    select(-Date)
data_total$Country <- as.factor(data_total$Country)
data_total$Abbrev <- as.factor(data_total$Abbrev)


xlim <- list(
    min = 0,
    max = max(data_total$Deaths) + max(data_total$Deaths) / 10 # set x axis boundary
)
ylim <- list(
    min = 0,
    max = max(data_total$Confirmed) + max(data_total$Confirmed) / 10 # set y axis boundary
)


# UI

ui <- fluidPage(
    title = "Covid-19",
    tabsetPanel(
        tabPanel(
            title = "Global Trend", # tab name
            h2(textOutput("summary_title")), ## summary title 1-1
            h5("The time is ", textOutput("currentTime", container = span)), # 1-2
            h3(textOutput("summary_table_title")), # 1-4
            h5(textOutput("summary_world1")), ## summary world status   #1-5
            h5(textOutput("summary_world2")),
            h5(textOutput("summary_world3")),
            h3(textOutput("daily_change_title")), # 1-6
            h5(textOutput("summary_new_world1")), # 1-7
            h5(textOutput("summary_new_world2")),
            h5(textOutput("summary_new_world3")),
            leafletOutput("mymap"), # map    # 1-3      ### Reference:https://shiny.rstudio.com/gallery/covid19-tracker.html###
            plotOutput("ggplot_global_case") # 1-8
        ),
        
        tabPanel(
            title = "Countries", # tab name
            ### Reference: https://yihui.shinyapps.io/DT-rows/###
            h2(textOutput("countries_title")), # 2-1
            DT::dataTableOutput("summary_countries") # 2-2
        ),
        
        tabPanel(
            title = "Comparison", # tab name
            h2("Country Comparison"),
            sidebarPanel(
                uiOutput(outputId = "selection_a"), # 3-1
                selectInput("country2", "Country B:", choices = c("-", NULL))
            ), # 3-2
            mainPanel(
                plotOutput("country_cumu"), # 3-4
                plotOutput("country_cumu_new")  # 3-5
            )
        ),
        
        ### Reference: https://shiny.rstudio.com/gallery/google-charts.html###
        tabPanel(
            title = "Typical Country", # 4-1
            # This line loads the Google Charts JS library
            googleChartsInit(),
            h2("Typical Country COVID-19 Trend From Day 1"),
            googleBubbleChart("chart",
                              width = "100%", height = "475px",
                              
                              options = list(
                                  fontSize = 10,
                                  # Set axis labels and ranges
                                  hAxis = list(
                                      title = "Number of Death Cases",
                                      viewWindow = xlim
                                  ),
                                  vAxis = list(
                                      title = "Number of Confirmed Cases",
                                      viewWindow = ylim
                                  ),
                                  
                                  chartArea = list(
                                      top = 50, left = 75,
                                      height = "75%", width = "75%"
                                  ),
                                  # Allow pan/zoom
                                  explorer = list(),
                                  # Set bubble visual props
                                  bubble = list(
                                      opacity = 0.4, stroke = "none",
                                      # Hide bubble label
                                      textStyle = list(
                                          color = "none"
                                      )
                                  ),
                                  # Set fonts
                                  titleTextStyle = list(
                                      fontSize = 16
                                  ),
                                  tooltip = list(
                                      textStyle = list(
                                          fontSize = 12
                                      )
                                  )
                              )
            ),
            fluidRow(
                shiny::column(4,
                              offset = 4,
                              sliderInput("date", "Date",
                                          min = min(data_total$time_date), max = max(data_total$time_date),
                                          value = min(data_total$time_date), animate =
                                              animationOptions(interval = 250, loop = FALSE)
                              ) # make the animation faster
                )
            )
        ),
        
        ### Reference:https://shiny.rstudio.com/gallery/widgets.html###
        # 5-1
        tabPanel(
            title = "U.S. states",
            titlePanel("U.S. state Status"),
            sidebarLayout(
                # Sidebar panel for inputs
                sidebarPanel(
                    # Input: Select a state
                    uiOutput(outputId = "selection_state"),
                    # Input: Specify the number of observations to view /different states have different total number of available observations
                    numericInput("num_day", "Number of observations to view: (Please Enter Integer Number Greater Than 1)", NULL),
                    actionButton("go", "Go")
                ),
                mainPanel(
                    # Output: Header + plot
                    h3(textOutput("summary_5_title")),
                    plotOutput("summary_5"),
                    # Output: Header + table
                    h3(textOutput("observations")),
                    dataTableOutput("view")
                )
            )
        )
    )
)



server <- function(input, output, session) {
    ###
    
    # get entire country list and abbre
    r0 <- RETRY("GET", "https://api.covid19api.com/countries", pause_min = 2, times = 10) # API would show error message because of the rate limit.
    json <- content(r0, as = "text", encoding = "UTF-8")
    country <- fromJSON(json)
    country <- country %>% arrange(Country)
    
    # newest daily country update for covid
    r1 <- RETRY("GET", "https://api.covid19api.com/summary", pause_min = 2, times = 10)
    json <- content(r1, as = "text", encoding = "UTF-8")
    summary <- fromJSON(json)
    
    # list of all countries have data in api
    country_a_selection <- summary$Countries %>%
        arrange(Country) %>%
        pull(Country)
    
    # render country list to ui 3-1 selection
    output$selection_a <- renderUI({
        selectInput("country1", "Country A:", choices = country_a_selection)
    })
    
    # get number of deaths and cases in last 30 days
    r2 <- RETRY("GET", "https://corona.lmao.ninja/v2/historical/all", pause_min = 2, times = 10)
    json <- content(r2, as = "text")
    world_j <- fromJSON(json, flatten = TRUE)
    world <- world_j$cases[1:sum(sapply(world_j$cases, function(x) !is.null(x)))] # sometimes the newest day value is NULL. No real data is inputted.
    world_death <- world_j$deaths[1:sum(sapply(world_j$cases, function(x) !is.null(x)))]
    
    # make a dataframe contains date
    world_df <- data.frame(num = c(1:length(world)), date = as.Date(str_replace(names(world), "[0-9]{2}$", as.character(format(Sys.Date(), "%Y"))), format = "%m/%d/%Y"), stringsAsFactors = FALSE)
    
    
    global_df <- data.frame(global_data = integer())
    global_death_df <- data.frame(global_death = integer())
    
    # move the data from list to dataframe
    for (i in 1:length(world)) {
        global_df <- global_df %>% bind_rows(tibble(global_data = world[[i]]))
        global_death_df <- global_death_df %>% bind_rows(tibble(global_death = world_death[[i]]))
    }
    
    world_df <- world_df %>%
        bind_cols(global_df) %>%
        bind_cols(global_death_df) %>% # dataframe with total global cases and deaths on that day
        mutate(daily_increse = lead(global_data) - global_data) %>% # case daily increase amount
        mutate(daily_increse_death = lead(global_death) - global_death) # death daily increase amount
    
    
    
    # This covid API does not update the newest data daily.
    r10 <- RETRY("GET", "https://corona.lmao.ninja/v2/nyt/states?state", pause_min = 2, times = 10)
    json <- content(r10, as = "text", encoding = "UTF-8")
    us <- fromJSON(json)
    us$date <- as.Date(us$date)
    yesterday_date <- us %>%
        drop_na(date) %>%
        arrange(desc(date)) %>%
        pull(date) %>%
        head(1) # get the newest update.
    
    
    cov_us_data <- us %>%
        filter(date == yesterday_date)
    selection_state <- cov_us_data %>%
        distinct(state) %>%
        arrange(state) %>%
        pull(state)
    max_length <- nrow(us %>% filter(state == "Washington"))
    
    #5-1 selection
    output$selection_state <- renderUI({
        selectInput("state", "Choose a U.S. state:", choices = selection_state)
    })
    
  
    
    # save iput country 1 as rv_data and corresponding data
    rv <- reactiveValues(country1_data = NULL, country2_data = NULL)
    
    # set initial value for country1 data status
    rv$country1_data_exist <- FALSE
    rv$country1_data_new_exist <- FALSE
    
    observeEvent(input$country1, if (!is.null(input$country1)) {{ # when ui successfully read the list of countries, change the status.
        rv$country1_data_exist <- TRUE
        rv$country1_data_new_exist <- TRUE
        
        rv$country1_data <- input$country1
        
        slug1 <- country %>%
            filter(Country == input$country1) %>%
            pull(Slug)
        r3_country1 <- RETRY("GET", str_glue("https://api.covid19api.com/total/country/{slug}", slug = slug1), pause_min = 2, times = 10)
        json <- content(r3_country1, as = "text", encoding = "UTF-8")
        country1_data <- fromJSON(json)
        
        time <- country1_data$Date
        time <- str_extract(time, "(\\d{4}-\\d{2}-\\d{2})")
        time_date1 <- as.Date(time, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
        rv$country1_data <- country1_data %>% bind_cols(time_date = time_date1)
        rv$country1_data_new <- rv$country1_data %>%
            mutate(daily_increse_confirmed = lead(Confirmed) - Confirmed, daily_increse_deaths = lead(Deaths) - Deaths, daily_increse_recovered = lead(Recovered) - Recovered) %>%
            drop_na() }})
    
    
    # save country 2 corresponding data
    observeEvent(c(input$country1, input$country2), {
        rv$country2_data <- input$country2
        
        if (!(rv$country2_data == "-" | is.na(rv$country2_data))) {
            slug2 <- country %>%
                filter(Country == rv$country2_data) %>%
                pull(Slug)
            r3_country2 <- RETRY("GET", str_glue("https://api.covid19api.com/total/country/{slug}", slug = slug2), pause_min = 2, times = 10)
            json <- content(r3_country2, as = "text", encoding = "UTF-8")
            country2_data <- fromJSON(json)
            
            time2 <- country2_data$Date
            time2 <- str_extract(time2, "(\\d{4}-\\d{2}-\\d{2})")
            time_date2 <- as.Date(time2, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
            rv$country2_data <- country2_data %>% bind_cols(time_date = time_date2)
        }
    })
    
    
    # 1-1 display title
    output$summary_title <- renderText({
        texts <- "COVID-19 CORONAVIRUS PANDEMIC"
    })
    
    # 1-2 display real time
    output$currentTime <- renderText({
        invalidateLater(as.integer(1000), session)
        format(Sys.time())
    })
    
    # 1-3 draw map
    worldcountry <- geojson_read("world_maps.geojson", what = "sp")
    
    plot_map <- worldcountry
    
    data_country <- summary$Countries
    
    
    # country code "XK" cannot be found in countrycode package
    data_country <- data_country %>% filter(CountryCode != "XK")
    
    new_data_country <- data_country %>%  mutate(iso3c = countrycode(data_country$CountryCode, "iso2c", "iso3c"))
    
    new_data_country1 <- new_data_country %>%
        arrange(iso3c) %>%
        drop_na()
    
    new_data_country1 %>% select(Country, CountryCode, iso3c)
    worldcountry$ADM0_A3
    
    # choose available country in ADM0_A3
    new_data_country2 <- new_data_country1[new_data_country1$iso3c %in% worldcountry$ADM0_A3, ]
    
    selected_countries <- new_data_country2$iso3c
    map_polygon <- worldcountry[worldcountry$ADM0_A3 %in% selected_countries, ]
    
    
    cv_pal <- colorBin("Reds", domain = 0:100, bins = c(0, 2000, 10000, 50000, 250000, 1250000, Inf))
    cv_num <- new_data_country2$TotalConfirmed
    
    basemap <- leaflet(plot_map) %>%
        addTiles() %>%
        clearMarkers() %>%
        clearShapes() %>%
        addPolygons(data = map_polygon, stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~ cv_pal(cv_num)) %>%
        addLegend("bottomright",
                  pal = cv_pal, values = ~ new_data_country2$TotalConfirmed,
                  title = "<small>Total Confirmed Cases</small>"
        )
    
    output$mymap <- renderLeaflet({
        basemap
    })
    
    
    # 1-4 display title
    output$summary_table_title <- renderText({
        texts <- "Global Status Summary:"
    })
    
    # 1-5 display number of total cases
    output$summary_world1 <- renderText({
        str_glue("Global Total Confirmed: {a}", a = summary$Global$TotalConfirmed)
    })
    
    # 1-5 display number of total deaths
    output$summary_world2 <- renderText({
        str_glue("Global Total Deaths: {b}", b = summary$Global$TotalDeaths)
    })
    
    # 1-5 display number of total recovered
    output$summary_world3 <- renderText({
        str_glue("Global Total Recovered: {c}", c = summary$Global$TotalRecovered)
    })
    
    # 1-6 display title
    output$daily_change_title <- renderText({
        texts <- "Today's Global Changes:"
    })
    
    # 1-7 display number of new cases
    output$summary_new_world1 <- renderText({
        str_glue("Global New Confirmed: {a}", a = summary$Global$NewConfirmed)
    })
    
    # 1-7 display number of new deaths
    output$summary_new_world2 <- renderText({
        str_glue("Global New Deaths: {b} ", b = summary$Global$NewDeaths)
    })
    
    # 1-7 display number of new recovered
    output$summary_new_world3 <- renderText({
        str_glue("Global New Recovered: {c}", c = summary$Global$NewRecovered)
    })
    
    # 1-8 histogram of last n days cases and deaths
    output$ggplot_global_case <- renderPlot({
        world_df_increase <- world_df %>% drop_na()
        
        p <- ggplot(data = world_df_increase, aes(x = date, y = daily_increse)) +
            geom_bar(stat = "identity", colour = "#000000") +
            ggtitle(str_glue("Daily New Cases in past {day} days", day = length(world))) +
            xlab("Date") +
            ylab("Daily Case Number")
        q <- ggplot(data = world_df_increase, aes(x = date, y = daily_increse_death)) +
            geom_bar(stat = "identity", colour = "black", fill = "#FF3333") +
            ggtitle(str_glue("Daily Deaths in past {day} days", day = length(world))) +
            xlab("Date") +
            ylab("Daily Death Number")
        
        ggarrange(p, q, ncol = 2, nrow = 1)
    })
    
    # 2-1 display title
    output$countries_title <- renderText({
        texts <- "Confirmed Cases and Deaths by Country/Region"
    })
    
    # 2-2 a table of covid 19 cases in different cotries
    output$summary_countries <- DT::renderDataTable({
        countries_df <- summary$Countries %>% select(-CountryCode, -Slug, -Date)
    })
    
    # 3-2 country B selections
    observe({
        x <- input$country1
        # get selection B only if we have selection for country A
        if (!is.null(x)) {
            country_b_selection <- summary$Countries %>%
                filter(Country != x) %>%
                arrange(Country) %>%
                pull(Country)
            updateSelectInput(session, "country2",
                              label = "Country B:",
                              choices <- c("-", country_b_selection)
            )
        }
    })
    
    # 3-4 3 cumulative plots
    # only run plot when country1 data exist
    observe({
        if (rv$country1_data_exist) {
            output$country_cumu <- renderPlot({
                
                # plot 1 country cumulative date
                if (input$country2 == "-") {
                    o <- ggplot(data = rv$country1_data, mapping = aes(x = rv$country1_data$time_date, y = rv$country1_data$Confirmed)) +
                        geom_line(size = 1, color = "#3399ff") +
                        ggtitle("Cumulative Confirmed Cases") +
                        xlab("Date") +
                        ylab("Confirmed Case Number")
                    
                    p <- ggplot(data = rv$country1_data, mapping = aes(x = rv$country1_data$time_date, y = rv$country1_data$Deaths)) +
                        geom_line(size = 1, color = "#FF0033") +
                        ggtitle("Cumulative Death Cases") +
                        xlab("Date") +
                        ylab("Death Case Number")
                    
                    q <- ggplot(data = rv$country1_data, mapping = aes(x = rv$country1_data$time_date, y = rv$country1_data$Recovered)) +
                        geom_line(size = 1, color = "#33FF33") +
                        ggtitle("Cumulative Recovered Cases") +
                        xlab("Date") +
                        ylab("Recovered Case Number")
                    
                    ggarrange(o, p, q, ncol = 3, nrow = 1)
                }
                
                # plot 2 countries cumulative date
                else {
                    rv$country2_data
                    rv$countries <- rv$country1_data %>% bind_rows(rv$country2_data)
                    o <- ggplot(data = rv$countries, mapping = aes(x = rv$countries$time_date, y = rv$countries$Confirmed, color = Country)) +
                        geom_line(size = 1) +
                        ggtitle("Cumulative Confirmed Cases") +
                        xlab("Date") +
                        ylab("Confirmed Case Number") +
                        theme(legend.position = "bottom")
                    
                    p <- ggplot(data = rv$countries, mapping = aes(x = rv$countries$time_date, y = rv$countries$Deaths, color = Country)) +
                        geom_line(size = 1) +
                        ggtitle("Cumulative Death Cases") +
                        xlab("Date") +
                        ylab("Death Case Number") +
                        theme(legend.position = "bottom")
                    
                    q <- ggplot(data = rv$countries, mapping = aes(x = rv$countries$time_date, y = rv$countries$Recovered, color = Country)) +
                        geom_line(size = 1) +
                        ggtitle("Cumulative Recovered Cases") +
                        xlab("Date") +
                        ylab("Recovered Case Number") +
                        theme(legend.position = "bottom")
                    
                    ggarrange(o, p, q, ncol = 3, nrow = 1)
                }
            })
        }
    })
    # 3-5
    # only run when country1 data daily exist
    observe({
        if (rv$country1_data_new_exist) {
            output$country_cumu_new <- renderPlot({
                
                # plot 1 country daily data
                if (input$country2 == "-") {
                    o <- ggplot(data = rv$country1_data_new, mapping = aes(x = time_date, y = daily_increse_confirmed)) +
                        geom_line(size = 1, color = "#3399ff") +
                        ggtitle("Daily New Confirmed Cases") +
                        xlab("Date") +
                        ylab("New Confirmed Case Number")
                    
                    p <- ggplot(data = rv$country1_data_new, mapping = aes(x = time_date, y = daily_increse_deaths)) +
                        geom_line(size = 1, color = "#FF0033") +
                        ggtitle("Daily New Death Cases") +
                        xlab("Date") +
                        ylab("New Death Case Number")
                    
                    q <- ggplot(data = rv$country1_data_new, mapping = aes(x = time_date, y = daily_increse_recovered)) +
                        geom_line(size = 1, color = "#33FF33") +
                        ggtitle("Daily New Recovered Cases") +
                        xlab("Date") +
                        ylab("New Recovered Case Number")
                    
                    ggarrange(o, p, q, ncol = 3, nrow = 1)
                }
                
                # plot 2 countries daily data
                else {
                    rv$country2_data
                    rv$country2_data_new <- rv$country2_data %>%
                        mutate(daily_increse_confirmed = lead(Confirmed) - Confirmed, daily_increse_deaths = lead(Deaths) - Deaths, daily_increse_recovered = lead(Recovered) - Recovered) %>%
                        drop_na()
                    rv$countries_data_new <- rv$country1_data_new %>% bind_rows(rv$country2_data_new)
                    
                    o <- ggplot(data = rv$countries_data_new, mapping = aes(x = time_date, y = daily_increse_confirmed, color = Country)) +
                        geom_line(size = 1) +
                        ggtitle("Daily New Confirmed Cases") +
                        xlab("Date") +
                        ylab("New Confirmed Case Number") +
                        theme(legend.position = "bottom")
                    
                    p <- ggplot(data = rv$countries_data_new, mapping = aes(x = time_date, y = daily_increse_deaths, color = Country)) +
                        geom_line(size = 1) +
                        ggtitle("Daily New Death Cases") +
                        xlab("Date") +
                        ylab("New Death Case Number") +
                        theme(legend.position = "bottom")
                    
                    q <- ggplot(data = rv$countries_data_new, mapping = aes(x = time_date, y = daily_increse_recovered, color = Country)) +
                        geom_line(size = 1) +
                        ggtitle("Daily New Recovered Cases") +
                        xlab("Date") +
                        ylab("New Recovered Case Number") +
                        theme(legend.position = "bottom")
                    
                    ggarrange(o, p, q, ncol = 3, nrow = 1)
                }
            })
        }
    })
    
    
    # 4-1
    defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
    series <- structure(
        lapply(defaultColors, function(color) {
            list(color = color)
        }),
        names = levels(data_total$Abbrev)
    )
    
    dateData <- reactive({
        # Filter to the desired date, and put the columns
        # in the order that Google's Bubble Chart expects
        # them (name, x, y, color, size). Also sort by Countries
        # so that Google Charts orders and colors the Countries
        # consistently.
        df <- data_total %>%
            filter(time_date == input$date) %>%
            select(
                Country, Deaths, Confirmed,
                Abbrev, Active
            ) %>%
            arrange(Country)
    })
    
    output$chart <- reactive({
        # Return the data and options
        list(
            data = googleDataTable(dateData()),
            options = list(
                title = sprintf(
                    "Number of Confirmed Cases vs. Number of Deaths Cases in Typical Countries ",
                    input$date
                ),
                series = series
            )
        )
    })
    
    # 5-1
    
    # show summary header only when rv5$data_length is numeric
    output$summary_5_title <- renderText({
        if (is.numeric(rv5$data_length)) {
            texts <- "Summary"
        }
    })
    
    # show Observations header only when rv5$data_length is numeric
    output$observations <- renderText({
        if (is.numeric(rv5$data_length)) {
            texts <- "Observations"
        }
    })
    
    # save selected state data after user hitting button
    datasetInput_state <- eventReactive(input$go, {
        us %>%
            filter(state == input$state) %>%
            select(-fips)
    })
    
    # save how many days of observations user want to see
    data_length <- eventReactive(input$go, {
        input$num_day
    })
    
    rv5 <- reactiveValues(data_length = NULL, diag_data = NULL)
    
    # save user's input in order to check if input is valid
    observeEvent(input$go, {
        rv5$data_length <- input$num_day
    })
    
    # display plot of covid-19 cases and deaths in selected state and selected range
    output$summary_5 <- renderPlot({
        diag_data <- datasetInput_state()
        data_length <- data_length()
        
        # input must be numeric
        if (is.numeric(data_length())) {
            
            # in case
            if (is.na(data_length)) {
                data_length <- nrow(diag_data)
            }
            
            # if user's input is larger than the maximum observations
            # use the maximum observations
            if (data_length >= nrow(diag_data)) {
                data_length <- nrow(diag_data)
            }
            
            # if user's input is less than 2
            # show maximum observations
            if (data_length < 2) {
                data_length <- nrow(diag_data)
            }
            
            # if user's input is less than maximum observations
            # keep user's input
            if (data_length < nrow(diag_data)) {
                data_length <- as.integer(data_length)
            }
            
            # save in order to check if input is vaild
            rv5$data_length <- data_length
            rv5$diag_data <- diag_data
            
            diag_data_trim <- tail(diag_data, n = isolate(data_length))
            daig_data1 <- reshape2::melt(diag_data_trim, id.var = c("date", "state"))
            ggplot(daig_data1, aes(x = date, y = value, color = variable, linetype = variable)) +
                xlab("Date") +
                ylab("Number of cases") +
                scale_y_continuous(labels = scales::comma) +
                geom_line(size = 2) +
                ggtitle(str_glue("{number} Days Of Obsevations In {state} Between {date_v1} and {date_v2}", number = data_length, state = isolate(input$state), date_v1 = as.Date(yesterday_date) - data_length, date_v2 = yesterday_date))
        }
    })
    
    # display table
    output$view <- renderDataTable({
        if (is.numeric(rv5$data_length)) {
            tail(rv5$diag_data, n = isolate(rv5$data_length))
        }
    })
}

shinyApp(server = server, ui = ui)
### Reference: https://yihui.shinyapps.io/DT-rows/###
### Reference: https://shiny.rstudio.com/gallery/google-charts.html###
### Reference:https://shiny.rstudio.com/gallery/widgets.html###
### Reference:https://shiny.rstudio.com/gallery/covid19-tracker.html###
