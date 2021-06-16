library(shiny)
library(plotly)

#https://stackoverflow.com/questions/38749087/ggplotly-no-applicable-method-for-plotly-build-applied-to-an-object-of-class
#https://stackoverflow.com/questions/60294046/how-to-use-plotly-in-r-shiny/60296836
#https://plotly-r.com/linking-views-with-shiny.html
#plotly_type_plan
load("G:/Mi unidad/Alvacast/SISTRAT 2019 (github)/mult_state_15.RData")

library(shiny)
library(plotly)
library(dplyr)

ui <- fluidPage(
  plotlyOutput("graph")
)

server <- function(input, output, session){
  
  output$graph <- renderPlotly({
    fitted_flexsurvreg_plot$data %>% 
      dplyr::mutate(years=round(time/365.25,0)) %>% 
      dplyr::mutate(trans2=trans) %>% 
      dplyr::left_join(cbind.data.frame(transition_label,trans2=1:15), by="trans2") %>% 
      group_by(trans) %>%
      group_map(~ plot_ly(data=., x = ~time, y = ~est, color = ~dist, type = "scatter", mode="lines", linetype = ~residential, text=~text) %>% 
                  add_trace(y = ~surv, type = 'scatter', mode = 'lines', line = list(color = 'red', width = 1, linetype = ~residential), text=~text2)%>% 
                  layout(annotations = list(list(x = 0.5 , y = 1.03, text = ~unique(transition_label), showarrow = F, xref='paper', yref='paper'))) %>% 
                  layout(
                    xaxis = list(
                      ticktext = list(1, 3, 5, 7, 9, 11), 
                      textangle = 0,
                      tickvals = list(365.25*1, 365.25*3, 365.25*5, 365.25*7, 365.25*9, 365.25*11),
                      tickmode = "array"
                    )), keep=TRUE) %>%    
      subplot(nrows = 3, shareX = T, shareY=T, titleX = F, titleY = F, margin = .05)%>% layout(showlegend = F) %>% #, margin = 0.05) %>% 
      layout(annotations = list(
        list(x = -0.03, y = 0.5, text = "Survival",
             font = list(color = "black",size = 15),
             textangle = 270,
             showarrow = F, xref='paper', yref='paper', size=48,margin =m)))
  })
}

shinyApp(ui, server)