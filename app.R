## Postcode loader ##

# Source: ONS Open Geography Portal
# Publisher URL: http://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-latest-centroids
# Licence: Open Government Licence 3.0

library(shiny) ; library(tidyverse); library(sf) ; library(jsonlite) ; library(DT)

gm <- URLencode("cty18nm = 'Greater Manchester'", reserved = TRUE)

lookup <- fromJSON(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD18_LAD18_CTY18_OTH_UK_LU/FeatureServer/0/query?where=", gm, "&outFields=WD18CD,WD18NM,LAD18CD,LAD18NM,CTY18CD,CTY18NM&outSR=4326&f=json"), flatten = T) %>% 
  pluck("features") %>% 
  as_tibble() %>% 
  select(lad_code = attributes.LAD18CD, lad_name = attributes.LAD18NM,
         ward_code = attributes.WD18CD, ward_name = attributes.WD18NM)

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
                    tags$a("latest postcode centroids", href = "https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-latest-centroids", target="_blank"),
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
                  plotOutput("map"),
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
    st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2018_Boundaries_UK_BGC/MapServer/0/query?where=", 
                   URLencode(paste0("lad18nm = '", input$filter_lad, "'"), reserved = TRUE), 
                   "&outFields=lad18cd,lad18nm,long,lat&outSR=4326&f=geojson"))
  })
  
  ward_layer <- reactive({
    req(ward_code())
    st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2018_Boundaries_V3/MapServer/2/query?where=", 
                   URLencode(paste0("wd18cd = '", ward_code(), "'"), reserved = TRUE), 
                   "&outFields=wd18cd,wd18nm,long,lat&outSR=4326&f=geojson"))
  })
  
  postcodes <- reactive({
    req(ward_code())
    
    resultOffset<-0
    df_total = data.frame()
    repeat{
      response<-fromJSON(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Postcodes/ONS_Postcode_Directory_Latest_Centroids/MapServer/0/query?where=",
                               URLencode(paste0("osward = '", ward_code(), "'")),
                               "&outFields=pcds,osward,oslaua,lat,long&outSR=4326&resultOffset=",resultOffset,"&f=json"), flatten = T) 
      df<-response %>%
        pluck("features") %>%
        select(postcode = attributes.pcds,
               ward_code = attributes.osward,
               lad_code = attributes.oslaua,
               lon = attributes.long,
               lat = attributes.lat) %>%
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
    "Contains National Statistics and OS data © Crown copyright and database right 2019"
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

