library(shiny)
library(highcharter)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(rsconnect)
library(png)
library(htmltools)


#high chart options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- " "
options(highcharter.lang = hcoptslang)

thm <- source("hc_theme.R")

crops <- c("Total cereals", "Spring barley", "Winter barley", "Wheat", "Oats", "Oilseed rape")
PAY_type <- c("Production", "Area", "Yield")



ui <- fluidPage( 
 
  tags$head(tags$title("SG - Cereal and Oilseed Rape  Harvest")),
  
    tags$head(includeHTML(("google-analytics.html")),
        tags$link(rel = "stylesheet", type = "text/css", href = c("style.css")),
        
        
        titlePanel(
            div(column(width = 2, tags$img(src = "RESAS Logo.png", align = "left",  width = "60%")),
                column(width = 7, h1("Cereal and Oilseed Rape Harvest in Scotland",tags$img(src = "NS logo.jpg",  width = "5%") )),
                column(width = 2, tags$img(src = "sg.png", align = "right",  width = "100%")))
           
            ),
        
        br(), br(),br(), br(),
        sidebarLayout(position = "left",
            div(sidebarPanel(width = 2,
                checkboxGroupInput(
                    "crop",
                    "Choose crop to add to chart",
                    choiceNames= crops,
                    choiceValues = crops,
                    selected = "Total cereals")
                    ,
                
                actionButton("selectAll", label = "Select All"),
                    actionButton("deselectAll", label = "Deselect All")
                    ,
                
                p(radioButtons(
                    "pay", "Select variable", 
                    choiceNames = PAY_type,
                    choiceValues = PAY_type))
                    ,
                
                 div(chooseSliderSkin("Flat"),
                     sliderInput( 
                   "year",
                   "Select year range", 
                   value = c(2003, 2022), 
                   min = 2003, max = 2022, step = 1, sep = "", ticks = TRUE
                 ))
                )),
            
            div(mainPanel(width = 10,
               #########TO IMPROVE: MOVE THE TEXT TO EXTERNAL FILE- CLEANER CODE#####
                 tabsetPanel(type = "tabs", 
                    tabPanel("Information", h2("Notes on the data"),p("These interactive charts are based on the final estimates of area, yield and production for spring and winter barley, wheat, oats and oilseed rape in Scotland. Data cover the period from 2003 to the latest 2022 harvest."), 
                                                                      p("Data are available in the", tags$a(href="https://www.gov.scot/collections/scottish-cereal-harvest-estimates", target ="_blank", "Scottish cereal harvest: estimates (link opens in a new window).")), 
                                                                      p("Information about data sources is provided in the", tags$a(href="https://www.gov.scot/publications/cereal-and-oilseed-rape-harvest-2022-final-estimates/pages/data-sources-and-more-information", target = "_blank", "methodology (link opens in a new window).")),
                             br(), 
                                           h2("How to plot the data"), p("The sidebar on the left displays options for you to choose from. An interactive chart will display the data for the options you have selected in the 'Plot' tab."),
                                           p("The 'Summary' tab displays a general overview of the latest harvest year, along with tables of the latest figures and percentage changes from the last year to the current."),
                                           p("The 'Plot' tab displays the interactive chart."),
                                           p("The 'Data Table' tab displays the table of data used in the interactive chart. There is an option to download a 'csv' file of the data, by clicking on the 'Download the data' link below the table.")),
                                           
                                  
                    tabPanel("Summary", h1("Key findings for the 2022 Scottish harvest", size = "20px"),
                              
                             p("Total cereal production from the 2022 harvest was 3.1 million tonnes. This is a 10% increase compared to 2021 and the highest level since 2014."), 
                             p("Total cereal yield was 7.6 tonnes per hectare, the highest record of the last 20 years. Winter barley, wheat and oilseed rape yields were also their highest in the past two decades."),
                             p("Dry and hot weather conditions in spring and summer resulted in a remarkably early harvest and reduced the need for drying."),
                             p("Production increased despite decreases in the sown area of spring barley and oats. The record yields of 2022 offset reductions in area."),  
                             br(), 
                             (tags$img(src = "vs.png", align = "center",  width = "100%"))),
                             
                    tabPanel("Plot",  h1(textOutput("title")),
                                          h2(align = "center", highchartOutput("line_chart")),
                                           h2("Note:"),
                                           p("To remove a crop from the chart, either deselect from the sidebar menu or click on its legend."),
                                           p("You can see data values for a specific year by hovering your mouse over the line.")),
                    tabPanel("Data Table", (tableOutput("pay_table")), 
                                           downloadLink("downloadData", "Download the data"))
                        )
                )
            ))
    
        )
    )

server <- function(input, output, session) { 
   
    load("ch_data.RData")

    #unit for production, area or yield 
    pay_unit <- reactive({if(input$pay == "Production"){" (Tonnes)"}
        else if (input$pay == "Area"){" (Hectares)"}
        else {"(Tonnes per hectare)"}
    })
    
    #user selections of crop(s), year(s) and one of production, area or yield
    #thousand tonnes units in production graph 
    crop_pay <- reactive({
      if(input$pay == "Production"){ch_data_prod <- filter(ch_data, PAY == "Production"& crop%in% input$crop)
      ch_data_prod$Value <- ch_data_prod$Value*0.001
      ch_data_prod %>% 
        filter(Year>= min(input$year) & Year<= max(input$year))
      }
      else if(input$pay == "Area"){filter(ch_data, PAY == "Area" & crop%in% input$crop) %>% 
          filter(Year>= min(input$year) & Year<= max(input$year))
      }
      else{filter(ch_data, PAY== "Yield" & crop%in% input$crop) %>% 
          filter(Year>= min(input$year) & Year<= max(input$year))} 
    })
    
   #user selection of one of: production area or yield. This is for instance when "select all" is chosen or all crops are ticked. 
    #may not be necessary to split user responses this way
    #thousand tonnes units in production graph
  all_crops_pay_only <- reactive({
      if(input$pay == "Production"){ch_data_prod <- filter(ch_data, PAY == "Production"& crop%in% input$crop)
     ch_data_prod$Value <- ch_data_prod$Value*0.001
     ch_data_prod %>% 
    filter(Year>= min(input$year) & Year<= max(input$year))
         }
    else if(input$pay == "Area"){filter(ch_data, PAY == "Area"& crop%in% input$crop) %>% 
        filter(Year>= min(input$year) & Year<= max(input$year))
            }
    else{filter(ch_data, PAY== "Yield") %>% 
      filter(Year>= min(input$year) & Year<= max(input$year))} 
   
      
    })
  
 #for the table displayed in the "Data Table" tab - pivot wider and add units to the headings
  #This is for when the user has selected <6 crops 
  pretty_data_table_sub_6<- reactive({
        crop_pay_selection <-   filter(ch_data, PAY == input$pay & crop%in% input$crop) %>% 
            filter(Year>= min(input$year) & Year<= max(input$year)) 
        crop_pay_selection$PAY <- as.character(crop_pay_selection$PAY)
        crop_pay_selection$PAY[crop_pay_selection$PAY == "Production"] <- "Production (Tonnes)" 
        crop_pay_selection$PAY[crop_pay_selection$PAY == "Area"] <- "Area (Hectares)" 
        crop_pay_selection$PAY[crop_pay_selection$PAY == "Yield"] <- "Yield (Tonnes per hectare)"

        pivot_wider(crop_pay_selection, names_from = c(crop, PAY), values_from = Value, names_sep = " ")

    })
    
  #Title of data download csv in "Data Table" tab
    # table_choice <- reactive({
    #     if(length(input$crop)==6){"Total cereals and oilseed rape"}
    #     else{ "Cereals and oilseed rape harvest estimates for "
    #     }
        
    #})
  
    #for the table displayed in the "Data Table" tab - pivot wider and add units to the headings
    #This is for when the user has selected all 6 crops
    pretty_data_table_all_6 <- reactive({
        all_crops_pay <- filter(ch_data, PAY == input$pay) %>% 
          filter(Year>= min(input$year) & Year<= max(input$year))   
        all_crops_pay$PAY <- as.character(all_crops_pay$PAY)
        all_crops_pay$PAY[all_crops_pay$PAY == "Production"] <- "Production (Tonnes)" 
        all_crops_pay$PAY[all_crops_pay$PAY == "Area"] <- "Area (Hectares)" 
        all_crops_pay$PAY[all_crops_pay$PAY == "Yield"] <- "Yield (Tonnes per hectare)"
        pivot_wider(all_crops_pay, names_from = c(crop, PAY), values_from = Value, names_sep = " ")
    })
    
    #Event when user presses "Select All": ticking all boxes to select all crops. 
    observeEvent(
        eventExpr = input$selectAll,
        handlerExpr = 
            {input$crop
                updateCheckboxInput(session = session, 
                                    inputId = "crop", 
                                    value = crops)
            })
    
    #Event when user presses "Deselect All": unticking all boxes to deselect all crops. 

    observeEvent(
        eventExpr = input$deselectAll,
        handlerExpr = 
            {input$crop
                updateCheckboxInput(session = session, 
                                    inputId = "crop", 
                                    value= 0)
                
            }
    )  
    
    #slider
   
    
    
    #plot 
    output$line_chart <- renderHighchart({ 
       #when user selects all crops 
         if ((length(input$crop)== 6)){
            hchart(all_crops_pay_only(), "line", 
                   hcaes(x= Year, y = Value, group = crop), style = list(fontFamily = "Roboto"))%>%   
                hc_yAxis( 
                    labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto"), 
                                  format = if (input$pay == "Production"| input$pay == "Area"){"{value:,.0f}"}
                                  else {"{value: 1.f}"} ),
                    title = list(text = if (input$pay == "Production"){"Thousand tonnes"} 
                                 else if (input$pay == "Area"){"Hectares"} 
                                 else {"Tonnes per hectare"},
                                  style = list(color = "#000000", fontSize = "20px", fontFamily = "Roboto"))) %>% 
                hc_xAxis(
                    labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto")),
                    title = list(text = "", style = list(color = "#000000", fontSize = "20px", fontFamily = "Roboto")),
                    type = "category") %>% 
                hc_legend(align = "left",
                          alignColums = FALSE,
                          layout = "horizontal") %>% 
             hc_tooltip(
               pointFormat = "{point.y:,.1f}")  %>% 
                hc_add_theme(theme)
         }
        
        #when user selects less than 6 crops
        else{  
            hchart(crop_pay(), "line", 
                   hcaes(x= Year, y = Value, group = crop), style = list(fontFamily = "Roboto"))%>%   
                hc_yAxis( 
                    labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto"), 
                                  format = if (input$pay == "Production" | input$pay == "Area"){"{value:,.0f}"}
                                  else {"{value: 1.f}"} ),
                    title = list(text = if (input$pay == "Production"){"Thousand tonnes"} 
                                 else if (input$pay == "Area"){"Hectares"} 
                                 else {"Tonnes per hectare"},
                                 style = list(color = "#000000", fontSize = "20px", fontFamily = "Roboto"))) %>% 
                hc_xAxis(
                    labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto")),
                    title = list(text = "", style = list(color = "#000000", fontSize = "20px")),
                    type = "category") %>% 
                hc_legend(align = "left",
                          verticalAlign = "top",
                          layout = "horizontal") %>%
            hc_tooltip(
              pointFormat = "{point.y:,.1f}")  %>% 
                hc_add_theme(theme)
        }
    })
    
    # title of plot
    output$title <- renderText({
        
        a_title <- if(length(input$crop)==6){paste0("Total Cereals and Oilseed Rape ",
                                                    ifelse(input$pay == "Area", " Sown Area", input$pay),
                                                    " in Scotland, ", min(input$year), " to ", max(input$year)) } 
        
        else {paste0(ifelse(input$pay == "Area", " Sown Area", input$pay),
                     " in Scotland, ", min(input$year), " to ", max(input$year))} 
    })
    
    #Rendering the Data Table
    output$pay_table <- renderTable(bordered = TRUE, striped = TRUE, 
                                    {
                                         if (length(input$crop)==6 & input$pay != "Yield"){
                                             pretty_data_table_all_6 <-format.data.frame(pretty_data_table_all_6(), digits = 5, nsmall = 0, big.mark = ",", big.interval = 3L)   
                                             pretty_data_table_all_6$Year <- pretty_data_table_all_6()$Year
                                             as_tibble(pretty_data_table_all_6)
                                         }
                                         
                                         else if (length(input$crop)==6 & input$pay == "Yield")
                                         {
                                             as_tibble(pretty_data_table_all_6())
                                             
                                         }
                                         
                                         else if (length(input$crop) <6 & input$pay == "Yield"){
                                             as_tibble(pretty_data_table_sub_6())
                                             
                                         }
                                         else 
                                         {
                                             pretty_data_table_sub_6 <- format.data.frame(pretty_data_table_sub_6(), digits = 5, nsmall = 0, big.mark = ",", big.interval = 3L)  
                                             pretty_data_table_sub_6$Year <- pretty_data_table_sub_6()$Year
                                             as_tibble(pretty_data_table_sub_6)
                                         }

                                     })
    
    #Download the data link beneath data table in "Data Table" tab
    output$downloadData <- downloadHandler(
        
        
        filename = function(){
            paste("Cereal and oilseed rape harvest estimates for", input$pay,  min(input$year), "to", max(input$year), ".csv", sep=" ")
        },
        
        content = function(file) { 
            if(length(input$crop)==6){
                write.csv( pretty_data_table_all_6(), file)     
            }
            else
            {write.csv(pretty_data_table_sub_6(), file)}
        }
    )

    
}
shinyApp(ui, server)