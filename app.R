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

ui <- fluidPage(tags$head(includeCSS("styles.css")),
   titlePanel(title = NULL, windowTitle = "Postcode loader"),
   sidebarLayout(
      sidebarPanel(
        tags$a(tags$img(src = "trafforddatalab_logo.png", width = "25%"), href = "https://www.trafforddatalab.io/", target="_blank"),
        tags$h3("Postcode loader"),
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
        uiOutput("filter_ward"),
        tags$br()),
        mainPanel(uiOutput("text"),
                plotOutput("map"),
                DT::dataTableOutput("table"),
                br(),
                textOutput("attribution"),
                uiOutput("code")
      )
   )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    filter(lookup, lad_name == input$filter_lad)
  })
  
  
  output$filter_ward <- renderUI({
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
    plot(st_geometry(ward_layer()), add = T, col = "#fc6721", border = "#212121")
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
    tags$h4(text)
  })
  
  output$attribution <- renderText({
    req(ward_code())
    "Contains National Statistics and OS data © Crown copyright and database right 2019"
  })
  
  output$code <- renderUI({
    req(ward_code())
    tags$p("View code", a(icon("github"), href = "https://github.com/trafforddatalab/postcode_loader", target = "_blank"))
  })
  
}

shinyApp(ui = ui, server = server)

