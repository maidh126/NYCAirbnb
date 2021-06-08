# Define UI 

ui <- shinyUI(
    navbarPage(title = "", selected ="Home",
               theme = shinytheme("flatly"), 
               useShinyjs(), 
               
               #### Home ####
               tabPanel("Home",
                        tags$head(tags$style(HTML('
                        .modal.in .modal-dialog {
                        filter: alpha(opacity=10); opacity: 0.8;border-color: 
                        height:100%; width:100%; position:absolute;
                        padding:5px;top:30%;left:0%;
                        }
                                                    
                        .modal-content {
                        filter: alpha(opacity=10); 
                        opacity: 0.9;
                        background-color: black;
                        height:100%; width:100%;
                        }
                                
                        .modal-body {
                        filter: alpha(opacity=10); opacity: 1;
                        float: none; margin: 0;
                        height:100%; width:100%; font-size: 42px;
                        color: #fff; font-weight: 300;
                        text-shadow: none;font-opacity: 1;
                        }
                                           '))),
                        br(),
                        
                        absolutePanel(fixed = T,draggable = F, bottom = "auto", top = 70, 
                                      right = "auto", left = 50, width = 400, height = "auto",
                                      style = "opacity: .8 ; z-index: 1000",
                                      HTML('<p style="font-size:5em; line-height: 1.2em"><strong>New York<br>AirBnB<br>Data<br>Analysis</strong></p>'),
                                      HTML('<div style="width:340px;height:10px;background-color:black;opacity:0.8"></div>')
                        ),
                        
                        absolutePanel( 
                            fixed = T, draggable = F, 
                            top = 70, bottom = "auto", left = 480, right = "auto", 
                            width = "auto", height = "auto",
                            style = "opacity: .7 ; z-index: 1000",
                            HTML('<h3><strong><left style="font-size:2.0em";>Are you ready to travel?</left></strong></h3>'),
                            HTML('<h2><strong><left style="font-size:1.0em";>This shiny app uses the Airbnb dataset provided by InsideAirbnb.com to explore New York neighbourhoods</strong> </left></h2>'),
                            br(),
                            HTML('<p id="maptext"; style="line-height:2.0em; font-size:1.1em; text-align:left"><strong> Below is a guide using this app. </strong>
                          <ul>
                        <li>Map Tab: How neighbourhoods compare by the average price and the average rating. </li>
                        <li>Neighbourhood Tab: The most cost-effective neighbourhood.</li>
                        <li>Compare Tab: Compare any two neighbourhoods side by side.</li>
                        <ul>
                          </left></p>')
                        ),
                        
                        
                        absolutePanel( 
                            fixed = T, draggable = F, bottom = "auto", top = 400, right = "auto", left = 480, 
                            style = "opacity: 1 ; z-index: 1000", width = 800, height = "auto",
                            fluidRow(valueBox("Number of Listing", 49530, icon = icon("hotel",class = NULL, lib = "font-awesome")),
                                     valueBox("Mean Price", 137, icon = icon("dollar-sign",class = NULL, lib = "font-awesome")),
                                     valueBox("Number of Hosts", 37207, icon = icon("user",class = NULL, lib = "font-awesome"))
                            )
                        ),
                        
                        
                        absolutePanel( 
                            fixed = T, draggable = F, right = "auto", left = 480, top = 580, bottom = "auto",
                            style = "opacity: .7 ; z-index: 1000", width = 1000, height = "auto",
                            HTML('<p style="font-size:1em"><strong>Data sources:  http://insideairbnb.com</strong></p>')
                        )
               ),
               
               #### Map ####
               tabPanel("Map",
                        leafletOutput(outputId = "mymap", width = "100%", height = 900),
                        absolutePanel(id = "controls", class = "panel panel-default", 
                                      fixed = F, draggable = F, 
                                      style = "opacity: .9 ; z-index: 1000",
                                      bottom = 40, top = 200, right = "auto", left = 20, 
                                      width = 300, height = 600, 
                                      radioButtons("MapType","Please select", 
                                                   choices = c("Average Review","Average Price")),
                                      checkboxGroupInput(inputId = "select_room", label = h4("Room Type"), 
                                                         choices = room_type, selected = room_type),
                                      sliderInput(inputId = "slider_price", label = h4("Listing Price"), min = 0, max = 1000, step = 50,
                                                  pre = "$", sep = ",", value = c(30, 1000)),
                                      sliderInput(inputId = "slider_rating", label = h4("Average Rating"), min = 0, max = 10, step = 0.2,
                                                  value = c(7, 10)),
                                      sliderInput(inputId = "slider_review", label = h4("Number of Reviews"), min = 0, max = 800, step = 20,
                                                  value = c(10, 350))
                        ),
                        
                        absolutePanel(
                            fixed = T, draggable = T, top = "auto", bottom = 40, left = "auto", right = 40, 
                            style = "opacity: .7 ; z-index: 1000",width = 800, height = 100,
                            HTML('<h3><strong><left style="color: #000000;">Map insight</left></strong></h3>'),
                            uiOutput("ui") 
                        )
               ),
               
               #### Neighbourhood ####
               tabPanel("Neighbourhood",
                        fluidRow(
                            column(4,"Please select your preference using the options below",HTML('<br></br>'),
                                   selectizeInput(inputId ="boroughtab3",label = "Borough",choices = unique (ny$neighbourhood_group_cleansed)),
                                   selectizeInput(inputId ="roomtypetab3",label = "Room Type",choices = unique (ny$room_type)),
                                   sliderInput("numoptionstab3","Number of Neighbourhood:",min = 2,max = 222,value = 5),
                                   sliderInput("pricerangetab3","Average Price Range:",min = 0,max = 1000,value = c(75,300))
                            ),
                            column(8,
                                   plotlyOutput("leBubblePlot"))
                        ),
                        HTML('<br></br>'),
                        HTML('<br></br>'),
                        "Note: Top 5 (or as specified) number of budgeted  Neighbourhoods displays those with the lowest average price that satisfy the user input criteria. The size of the bubble reflects the number of listing in that neighbourhood.",
                        HTML('<br></br><br></br>')),
               
               #### Compare ####
               tabPanel('Compare',
                        fluidRow(
                            HTML('<p style="line-height:1.6em; font-size:1.1em; text-align:center"><strong> Lets compare any two neighbourhoods </strong></center></p>'),
                            column(6,
                                   selectizeInput(inputId ="SelectNeighbourhood1",
                                                  label = "Neighbourhood",
                                                  choices = unique(ny$neighbourhood_cleansed)),
                                   plotOutput("comparePlot1"),
                                   plotOutput("comparePlot3")
                            ),
                            column(6,
                                   selectizeInput(inputId ="SelectNeighbourhood2",
                                                  label = "Neighbourhood",
                                                  choices = unique (ny$neighbourhood_cleansed)),
                                   plotOutput("comparePlot2"),
                                   plotOutput("comparePlot4")
                            )
                        )
               )
    )
)


# Define server
server <- shinyServer(function(input, output) {
    set.seed(1)
    
    #### Home ####
    observeEvent(once = TRUE, ignoreInit = FALSE, ignoreNULL = FALSE, eventExpr = rnorm(500), 
                 {
                     showModal(
                         modalDialog(easyClose = T, title=HTML('<br>'), size = 'l',
                                     tags$head(tags$style('body,  { font-family: "Open Sans"; background-color: black;}')),
                                     HTML('<h1><strong><center>Which is the cost-effective neighbourhood of New York for AirBnB?</center></strong></h1>')
                         ))
                 })
    
    #### Map ####
    mapdf <- reactive({
        updated_listing = ny %>% filter(
            room_type %in% input$select_room & 
                price >= input$slider_price[1] &
                price <= input$slider_price[2] &
                number_of_reviews >= input$slider_review[1] &
                number_of_reviews <= input$slider_review[2] &
                review_scores_location >= input$slider_rating[1] &
                review_scores_location <= input$slider_rating[2]) 
        
        loc.review <- updated_listing %>% 
            group_by(neighbourhood_cleansed) %>% 
            summarise(avg_loc_review = round(mean(review_scores_location, na.rm = TRUE),2))
        
        colnames(loc.review) <- c("neighbourhood","avg_review")
        
        neighpriceavg <- updated_listing %>% 
            group_by(neighbourhood_cleansed) %>% 
            summarise(avg_price = round(mean(price, na.rm = TRUE),2))
        
        colnames(neighpriceavg) <- c("neighbourhood","avg_price")
        
        temp <- merge(neighpriceavg, loc.review, by="neighbourhood")
        
        airbnb_neigh
    })
    
    # Leaflet
    output$mymap <- renderLeaflet({
        pal <- colorBin(review.palette,domain = airbnb_neigh$avg_review, bin=review.bins)
        leaflet(airbnb_neigh) %>%
            setView(lng = -73.90, lat = 40.51, zoom = 10)%>%
            addProviderTiles("CartoDB.Positron")%>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, 
                        fillColor = ~pal(avg_review), fillOpacity = .9,
                        label = ~paste(neighbourhood,',',avg_review)) %>%
            addLegend(pal = pal, values = ~avg_review, opacity = 1.0, 
                      title = "Avg Location Review", layerId = "foo")
    })
    
    # Update leaflet 
    observe({
        if(input$MapType=='Average Price')
        {
            pal <- colorBin(price.palette, domain = mapdf()$avg_price, 
                            pretty = FALSE, bin = price.bins)
            
            proxy <- leafletProxy("mymap", data = mapdf()) %>%
                clearShapes() %>%
                clearControls() %>%
                addPolygons (stroke = FALSE, 
                             smoothFactor = 0.3, 
                             fillOpacity = .8, fillColor = ~pal(avg_price),
                             label = ~paste(neighbourhood,',',avg_price)) %>%
                addLegend(pal = pal, values =~avg_price, opacity = 1.0, 
                          title = "Average Price",layerId = "foo")
        }
        else
        {
            pal <- colorBin(review.palette, domain = mapdf()$avg_review, bin=review.bins)
            proxy <- leafletProxy("mymap", data = mapdf()) %>%
                clearShapes()%>%
                clearControls%>%
                addPolygons(stroke = FALSE, smoothFactor = 0.3, 
                            fillOpacity = .9 ,fillColor = ~pal(avg_review),
                            label = ~paste(neighbourhood,',',avg_review)) %>%
                addLegend(pal = pal, values = ~avg_review, opacity = 1.0, 
                          title = "Avg Location Review",layerId = "foo")
        }
    })
    
    # Change the text
    output$ui <- renderUI({
        if(input$MapType=='Average Review')
        {
            HTML('<p style="font-size: 16px;color: #000000;font-weight: 700;font-style: italic;font-variant: normal;">
    <strong> Manhattan and the nearby area have high location reviews compared to other boroughs, mainly because it hosts iconic attractions like the Empire State Building, Statue of Liberty, and Central Park. Check out how the Average Price varies or apply a filter to explore more.</strong></left></p>')
        }
        else{
            HTML('<p style="font-size: 16px;color: #000000;font-weight: 700;font-style: italic;font-variant: normal;">
    <strong> Manhattan is the most expensive borough, which is quite reasonable since it is the tourist hub. Brooklyn is also costly since it is also a popular hub for multiple attraction. Check out how avg location rating varies or apply a filter to explore more.</strong></left></p>')
        }
    })
    
    #### Neighbourhood ####
    price_data <- reactive({
        ny %>% 
            filter(room_type==input$roomtypetab3,neighbourhood_group_cleansed==input$boroughtab3) %>% 
            group_by(neighbourhood_cleansed,neighbourhood_group_cleansed) %>% 
            summarise(
                avg_price=round(mean(price,na.rm = TRUE),0),
                avg_reviews=round(mean(review_scores_location,na.rm = TRUE),2),
                num_listings=n()) %>%
            filter(avg_price<=input$pricerangetab3[2] & avg_price>=input$pricerangetab3[1]) %>%
            mutate(text = paste(neighbourhood_cleansed,", ", neighbourhood_group_cleansed, 
                                "\nAverage Price: ", avg_price, 
                                "\nAverage Review: ", avg_reviews, 
                                "\nTotal Listing: ", num_listings, sep="")) %>%  
            arrange(desc(avg_price))
    })
    
    # Bubble plot
    output$leBubblePlot <- renderPlotly({
        p <- ggplot(
            price_data()[1:input$numoptionstab3,], 
            aes(x=avg_price, y=avg_reviews, text=text,
                color = neighbourhood_cleansed,size = num_listings)) +
            geom_point(alpha=0.7) + theme_bw() +
            scale_size(range = c(5, 15)) +
            scale_color_viridis(discrete=TRUE, guide=FALSE) +
            xlab("Average Price") + ylab("Average Review") + ylim(NA, 10)+
            ggtitle("Most budgeted Neighbourhoods") +
            theme(legend.title=element_blank(),legend.position="topright",
                  plot.title = element_text(hjust = 0.5))
        # turn ggplot interactive with plotly
        ggplotly(p,tooltip="text")
    })
    
    
    #### Compare ####
    # Data for left selected neighbourhood
    comparedata1 <- reactive({
        ny %>% 
            filter(neighbourhood_cleansed==input$SelectNeighbourhood1) 
    })
    
    # Data for right selected neighbourhood
    comparedata2<- reactive({
        ny %>% 
            filter(neighbourhood_cleansed==input$SelectNeighbourhood2) 
    })
    
    # 1st plot
    output$comparePlot1 <- renderPlot({
        ggplot(
            comparedata1() %>% mutate(n=1) %>% 
                group_by(room_type) %>% 
                summarise(count=sum(n)) %>% 
                mutate(percent=round(count/sum(count)*100,0.0)), 
            aes(x=reorder(room_type,desc(count)), y=count, fill=room_type)) + 
            geom_bar(stat="identity") +  
            geom_text(aes(label=paste0(percent, "%\n(", count, ")"))) +
            theme_bw() +
            xlab("") + ylab("Number of listing") +
            ggtitle("Frequency of listing type")
    })
    
    
    # 2nd plot 
    output$comparePlot2 <- renderPlot({
        ggplot(
            comparedata2() %>% mutate(n=1) %>% 
                group_by(room_type) %>% 
                summarise(count=sum(n)) %>% mutate(percent=round(count/sum(count)*100,0.0)), 
            aes(x=reorder(room_type,desc(count)), y=count, fill=room_type)) + 
            geom_bar(stat="identity") +  
            geom_text(aes(label=paste0(percent, "%\n(", count, ")"))) +
            theme_bw() +
            ylab("Number of listing") +
            xlab("") +
            ggtitle("Frequency of listing type")
    })
    
    # 3rd plot 
    output$comparePlot3 <- renderPlot({
        ggplot(
            comparedata1(), aes(x=room_type,y=price,fill=room_type)) +
            geom_violin() +
            theme_bw() +
            ylab("Price") +
            xlab("") +
            ylim(NA, 500)+
            ggtitle("Price per night for different listing type")
    })
    
    
    # 4th plot 
    output$comparePlot4 <- renderPlot({
        ggplot(
            comparedata2(), aes(x=room_type,y=price,fill=room_type)) +
            geom_violin() +
            theme_bw() +
            ylab("Price") +
            xlab("") +
            ylim(NA, 500)+
            ggtitle("Price per night for different listing type")
    })
    
})


# Run the application 
shinyApp(ui = ui, server = server)
