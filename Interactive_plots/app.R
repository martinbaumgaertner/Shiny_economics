library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)



header <- dashboardHeader(
  title = "Shiny_economics"
)
body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               plotlyOutput("p",height="92vh")
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               h3("Demand"),
               splitLayout(
                 numericInput("intercept_d","Intercept",10),
                 numericInput("slope_d","Slope",-0.5)),
               h3("Supply"),
               splitLayout(
                 numericInput("intercept_s","Intercept",5),
                 numericInput("slope_s","Slope",0.5)),
               sliderInput("range", h3("x limit"),
                           min = 20, max = 10000, value = 20, step = 10),
               actionButton("draw", "Draw")
           )
    )
  )
)

ui<-dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
rm(y_demand,y_supply)
server <- function(input, output, session) {
  define_parameter<-function(intercept,slope){
    return(list(intercept=intercept,
                slope=slope))
  }
  gleichwicht_x<-function(list1,list2){
    gg_x=(list1$intercept-list2$intercept)/(list2$slope-list1$slope)
  }
  gleichgewicht_p<-function(gg_x,parameter_data){
    gg_p=parameter_data$intercept+parameter_data$slope*gg_x
    return(gg_p)
  }
  price_function<-function(parameter,x){
    intercept=parameter$intercept
    slope=parameter$slope
    price=intercept+slope*x
    return(price)
  }
  function_data<-function(parameter_list,quantity,name){
    return(tibble(quantity=quantity,
                  !!name:=price_function(parameter_list,quantity)))
  }
  
  observeEvent(input$draw,{
    demand_intercept<-input$intercept_d
    demand_slope<-input$slope_d
    supply_intercept<-input$intercept_s
    supply_slope<-input$slope_s
    range<-input$range
    
    supply_start<-function_data(define_parameter(supply_intercept,supply_slope),c(0:range),"supply")
    demand_start<-function_data(define_parameter(demand_intercept,demand_slope),c(0:range),"demand")
    supply<-function_data(define_parameter(supply_intercept,supply_slope),c(0:range),"supply")
    demand<-function_data(define_parameter(demand_intercept,demand_slope),c(0:range),"demand")

    
    output$p <- renderPlotly({
      d <- event_data("plotly_relayout", source = "trajectory")
      
      move_demand <- if (!is.null(d[["shapes[0].yanchor"]])) {
        y_demand <<- round(d[["shapes[0].yanchor"]],0)
        demand<<-function_data(define_parameter(y_demand,demand_slope),c(0:range),"demand")
      } else {
        if(!exists("y_demand")){
          y_demand<<-demand_intercept
          demand<<-demand_start
        }
      }
      
      move_supply <- if (!is.null(d[["shapes[1].yanchor"]])) {
        y_supply <<- round(d[["shapes[1].yanchor"]],0)
        supply<<-function_data(define_parameter(y_supply,supply_slope),c(0:range),"supply")
      } else {
        if(!exists("y_supply")){
          y_supply<<-supply_intercept
          supply<<-supply_start
        }
      }
      
      points<-data.frame(x=c(0,0),y=c(y_demand,y_supply))
      
      intercepts<-map2(points$x,points$y, 
                       ~list(
                         type = "circle",
                         xanchor = .x,
                         yanchor = .y,
                         x0 = -4, x1 = 4,
                         y0 = -4, y1 = 4,
                         xsizemode = "pixel", 
                         ysizemode = "pixel",
                         fillcolor = "blue",
                         line = list(color = "transparent")
                       )
      )
      
      
      x_gg=gleichwicht_x(define_parameter(y_supply,supply_slope),define_parameter(y_demand,demand_slope))
      p_gg=gleichgewicht_p(x_gg,define_parameter(y_supply,supply_slope))
      gg<-data.frame(x_gg,p_gg)
      
      
      
      plot_ly( source = "trajectory") %>%
        add_trace(x = demand_start$quantity, y = demand_start$demand, name = 'Demand_old', mode = 'lines', line=list(color='#9696a3', dash="dash"), type = "scatter") %>%
        add_trace(x = supply_start$quantity, y = supply_start$supply, name = 'Supply_old', mode = 'lines', line=list(color='#9696a3', dash="dash"), type = 'scatter') %>%
        add_trace(x = demand$quantity, y = demand$demand, name = 'Demand', mode = 'lines', type = "scatter") %>%
        add_trace(x = supply$quantity, y = supply$supply, name = 'Supply', mode = 'lines', type = "scatter") %>%
        add_segments(x = gg$x_gg, xend = gg$x_gg, y = 0, yend = gg$p_gg,name="quantity", line = list(color = "#a4a4b0", dash="dot")) %>%
        add_segments(x = 0, xend = gg$x_gg, y = gg$p_gg, yend = gg$p_gg,name="price", line = list(color = "#a4a4b0", dash="dot")) %>% 
        layout(shapes = intercepts) %>%
        config(editable = list(shapePosition = TRUE))
  })
  }
  )
  
}

shinyApp(ui, server)

?add_segments
