## Postcode loader ##

# Source: ONS Open Geography Portal
# Publisher URL: https://geoportal.statistics.gov.uk/datasets/ons::onspd-online-latest-centroids/about
# Licence: Open Government Licence 3.0

library(shiny) ; library(tidyverse); library(sf) ; library(jsonlite) ; library(DT) ; library(shinycssloaders)


# Create a lookup for wards to Local Authorities within Greater Manchester
gm <- URLencode("LAD23CD = 'E08000001' OR LAD23CD = 'E08000002' OR LAD23CD = 'E08000003' OR LAD23CD = 'E08000004' OR LAD23CD = 'E08000005' OR LAD23CD = 'E08000006' OR LAD23CD = 'E08000007' OR LAD23CD = 'E08000008' OR LAD23CD = 'E08000009' OR LAD23CD = 'E08000010'", reserved = TRUE)

lookup <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD23_LAD23_UK_LU/FeatureServer/0/query?outFields=*&where=", gm, "&f=geojson")) %>% 
  st_drop_geometry() %>%
  select(lad_code = LAD23CD, lad_name = LAD23NM,
         ward_code = WD23CD, ward_name = WD23NM)

ui <- fluidPage(title = "Postcode loader",
   # Set the language of the page - important for accessibility
   tags$html(lang = "en-GB"),
   tags$head(includeCSS("styles.css")),
   br(),
     sidebarLayout(
       tags$header(
        sidebarPanel(
            tags$a(tags$img(src = "trafforddatalab_logo.png", width = "25%", alt = "Trafford Data Lab"), href = "https://www.trafforddatalab.io/", target="_blank"),
            tags$h1("Postcode loader"),
            tags$p("This application allows you to download the ",
                    tags$a("latest postcode centroids", href = "https://geoportal.statistics.gov.uk/datasets/ons::onspd-online-latest-centroids/about", target="_blank"),
                    "from the ", tags$a("ONS", href = "https://www.ons.gov.uk/", target="_blank"), "' ", 
                    tags$a("Open Geography Portal", href = "https://geoportal.statistics.gov.uk/", target="_blank"), 
                    " for your chosen ward in Greater Manchester."),
            tags$br(),
            selectizeInput(
              inputId = "filter_lad",
              label = "Choose a local authority",
              choices = sort(unique(lookup$lad_name))),
            uiOutput("filter_ward_container"),
            tags$br()
          )
        ),
        tags$main(
          mainPanel(uiOutput("text"),
                  withSpinner(plotOutput("map"), type = 4, color = "#046dc3"),
                  DT::dataTableOutput("table"),
                  br(),
                  textOutput("attribution"),
                  uiOutput("code")
        )
      )
   ),
   HTML("
        <script>
          // Receive call from Shiny server with the alt text for the dynamic plot img.
          Shiny.addCustomMessageHandler('altTextHandler', function(altText) {
              // Setup a call to the update function every 100 milliseconds in case the plot has not been created yet
              var altTextCallback = setInterval(function() {
                  // The plot img element does not have an id itself so we can only identify it from the parent node
                  try {
                      var plotContainer = document.getElementById('map');
                      plotContainer.firstChild.setAttribute('alt', altText);
                      clearInterval(altTextCallback); // Cancel the callback as we have updated the alt text
                  }
                  catch(e) {
                      // An error occurred, likely the img tag hasn't been created/replaced yet. Function will run again in 100 milliseconds
                  }
              }, 100);
          });
          
          // Add label to the hidden select element for the LA choice
          var cb_selectLabelLa = setInterval(function() {
              try {
                  // create a new label tag to hold the hidden select tag
                  var label = document.createElement('label');
                  label.setAttribute('style', 'display: none;');
                  
                  // get the references to the hidden select tag and its parent
                  var selection = document.getElementById('filter_lad');
                  var parent = selection.parentNode;
                  
                  // add the select to the label and then the label to the parent div
                  label.appendChild(selection);
                  parent.appendChild(label);
                  
                  clearInterval(cb_selectLabelLa); // cancel further calls to this fn
              }
              catch(e) {
                  // do nothing, wait until function is called again next interval
              }
          }, 500);
          
          // Add label to the hidden select element for the ward choice
          var cb_selectLabelWard = setInterval(function() {
              try {
                  // create a new label tag to hold the hidden select tag
                  var label = document.createElement('label');
                  label.setAttribute('style', 'display: none;');
                  
                  // get the references to the hidden select tag and its parent
                  var selection = document.getElementById('filter_ward');
                  var parent = selection.parentNode;
                  
                  // add the select to the label and then the label to the parent div
                  label.appendChild(selection);
                  parent.appendChild(label);
                  
                  clearInterval(cb_selectLabelWard); // cancel further calls to this fn
              }
              catch(e) {
                  // do nothing, wait until function is called again next interval
              }
          }, 500);
        </script>
    ")
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    filter(lookup, lad_name == input$filter_lad)
  })
  
  
  output$filter_ward_container <- renderUI({
    selectizeInput("filter_ward", "and a ward", sort(unique(filtered_data()$ward_name)))
  })
  
  ward_code <- reactive({pull(select(filter(filtered_data(), ward_name %in% input$filter_ward), ward_code))})
  lad_layer <- reactive({
    st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2023_Boundaries_UK_BGC/FeatureServer/0/query?where=", 
                   URLencode(paste0("lad23nm = '", input$filter_lad, "'"), reserved = TRUE), 
                   "&outFields=lad23cd,lad23nm,long,lat&outSR=4326&f=geojson"))
  })
  
  ward_layer <- reactive({
    req(ward_code())
    st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD_MAY_2023_UK_BGC/FeatureServer/0/query?where=", 
                  URLencode(paste0("wd23cd = '", ward_code(), "'"), reserved = TRUE), 
                  "&outFields=wd23cd,wd23nm,long,lat&outSR=4326&f=geojson"))
  })
  
  postcodes <- reactive({
    req(ward_code())
    
    resultOffset<-0
    df_total = data.frame()
    repeat{
      response<-fromJSON(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ONSPD_Online_Latest_Centroids/FeatureServer/0/query?where=",
                               URLencode(paste0("osward = '", ward_code(), "'")),
                               "&outFields=pcds,osward,oslaua,lat,long&outSR=4326&resultOffset=",resultOffset,"&f=json"), flatten = T) 
      df<-response %>%
        pluck("features") %>%
        select(postcode = attributes.PCDS,
               ward_code = attributes.OSWARD,
               lad_code = attributes.OSLAUA,
               lon = attributes.LONG,
               lat = attributes.LAT) %>%
        left_join(select(lookup, ward_code, ward_name), by = "ward_code") %>%
        select(postcode, ward_code, ward_name, lon, lat)
      
      df_total <- rbind(df_total,df)
      
      if (exists('exceededTransferLimit', where=response)){
        resultOffset<-resultOffset+1000
      } else break
    }
    df_total
  })
  
  output$map = renderPlot({
    req(ward_code())
    plot(st_geometry(lad_layer()), col = "#DDDDCC", border = "#212121")
    plot(st_geometry(ward_layer()), add = T, col = "#046dc3", border = "#212121")
})
  
  output$table <- DT::renderDataTable(server=FALSE,{
    postcodes()
    }, 
    extensions= c('Buttons', "Scroller"), 
    rownames = FALSE,  
    options = list(
      dom = 'Blfrtip',
      deferRender = TRUE,
      scroller = TRUE, 
      scrollX = TRUE,
      scrollY = "220px",
      buttons = list('csv')),
    colnames = c(
      "Postcode" = "postcode",
      "Ward code" = "ward_code",
      "Ward name" = "ward_name",
      "Longitude" = "lon",
      "Latitude" = "lat")
    )
  
  output$text <- renderUI({
    req(ward_code())
    text <- paste0(pull(select(filter(filtered_data(), ward_name %in% input$filter_ward), ward_name)),
           " ward in ", input$filter_lad) 
    tags$h2(text)
  })
  
  output$attribution <- renderText({
    req(ward_code())
    "Contains National Statistics and OS data © Crown copyright and database right 2023"
  })
  
  output$code <- renderUI({
    req(ward_code())
    tags$p(a("View code ", icon("github"), href = "https://github.com/trafforddatalab/postcode_loader", target = "_blank"))
  })
  
  observe({
    # construct alt text string for the map plot and send to Javascript handler to add
    altText <- paste0("Map showing location of ", pull(select(filter(filtered_data(), ward_name %in% input$filter_ward), ward_name)),
                      " ward in ", input$filter_lad) 
    session$sendCustomMessage("altTextHandler", altText)
  })
  
}

shinyApp(ui = ui, server = server)

