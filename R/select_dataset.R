A = data.frame(x = 1:5, y = 1:5)
B = data.frame(x = 1:6, y = 1:6)
C = data.frame(x = 1:7, y = 1:7)
data_sets <- list(A, B, C)
library(highcharter)
ui <- fluidPage(
  selectInput('dataset', 'Choose a dataset:', choices = c("A" = "1", "B" = "2", "C" = "3")),
  highchartOutput('my_plot')
)

server <- function(input, output, session) {


  datasetInput <- reactive({
    temp <- data.frame(data_sets[[as.numeric(input$dataset)]])
  })

  output$my_plot <- renderHighchart({
    mydata <- datasetInput()

    hc <- highchart() %>%
      highcharter::hc_add_series(mydata, 'bar', hcaes(x = x, y = y))

    hc
  })
}

shinyApp(ui, server)

make_cog <-  function(in_file, out_file){
  sf::gdal_utils("translate",
                 source       = in_file,
                 destination  = out_file,
                 options = c("-co", "TILED=YES",
                             "-co",  "COPY_SRC_OVERVIEWS=YES",
                             "-co",  "COMPRESS=DEFLATE"))

}
library(here)
here("mp2023_commercial_viability.rds")
here("mp2023_commercial_viability_cog.xml")
t <- make_cog(in_file = here("mp2023_commercial_viability.rds"), out_file = "mp2023_commercial_viability_cog.xml")

sf::gdal_utils("translate",
               source       = here("mp2023_commercial_viability.rds"),
               destination  = "mp2023_commercial_viability_cog.xml",
               options = c("-co", "TILED=YES",
                           "-co",  "COPY_SRC_OVERVIEWS=YES",
                           "-co",  "COMPRESS=DEFLATE"))

remotes::install_github("r-spatial/leafem")


