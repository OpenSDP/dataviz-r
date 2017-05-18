# This is an exercise! File will not run. Fill in the "?" starting at line 112.

# Required packages
packages <- c("ggplot2","plotly", "shiny")
lapply(packages, library, character.only = TRUE)

# Load data frames
schoolData <- structure(list(last_hs_name = c("River ", "West Spring Hill ", "Upland ", "North ", "South ", "Peak ", "City ", "White Sands ", "Central ", "Camino ", "Orchard "), hs_diploma = c(258L, 435L, 263L, 205L, 435L, 257L, 194L, 392L, 337L, 453L, 529L), pct_4yr = c(27.1317829457364, 28.0459770114943, 41.4448669201521, 41.4634146341463, 44.1379310344828, 44.3579766536965, 51.0309278350515, 52.8061224489796, 53.4124629080119, 65.121412803532, 73.1568998109641), pct_2yr = c(32.5581395348837, 26.6666666666667, 18.6311787072243, 25.3658536585366, 27.816091954023, 29.5719844357977, 16.4948453608247, 26.2755102040816, 24.6290801186944, 21.1920529801325, 14.1776937618147), pct_4yr_delayed = c(0, 1.60919540229885, 1.14068441064639, 0.48780487804878, 1.14942528735632, 0, 0.515463917525773, 1.02040816326531, 1.48367952522255, 1.32450331125828, 1.13421550094518), pct_2yr_delayed = c(3.87596899224806, 5.97701149425287, 4.94296577946768, 5.85365853658537, 4.13793103448276, 4.28015564202335, 3.09278350515464, 3.31632653061224, 2.67062314540059, 0.662251655629139, 1.32325141776938 ), avg_test_math_8_std = c(-0.0572368162790698, -0.17006474045977, 0.0620150684410646, 0.168237465365854, 0.320300586206897, 0.122428973929961, 0.21138548814433, 0.492871761734694, 0.46508424925816, 0.713709099337748, 0.951112215879017), order_2yr = c(11L, 8L, 3L, 6L, 9L, 10L, 2L, 7L, 5L, 4L, 1L), order_4yr = 1:11), .Names = c("last_hs_name", "hs_diploma", "pct_4yr", "pct_2yr", "pct_4yr_delayed", "pct_2yr_delayed", "avg_test_math_8_std", "order_2yr", "order_4yr"), row.names = c(NA, -11L), class = c("tbl_df", "tbl", "data.frame"))
agencyData <- structure(list(last_hs_name = "Agency Average", hs_diploma = 3758L, pct_4yr = 49.4944119212347, pct_2yr = 23.6029803086748, pct_4yr_delayed = 1.0111761575306, pct_2yr_delayed = 3.40606705694518, avg_test_math_8_std = 0.359298796035125), .Names = c("last_hs_name", "hs_diploma", "pct_4yr", "pct_2yr", "pct_4yr_delayed", "pct_2yr_delayed", "avg_test_math_8_std"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))

# Bar function from function exercise
bar_function <- function(college_type){
  
  if (college_type == 2){
    school_seamless <- schoolData$pct_2yr
    agency_seamless <- agencyData$pct_2yr
    school_delayed <- schoolData$pct_2yr_delayed
    order <- schoolData$order_2yr
    tooltip_title_seamless <- "Seamless 2-yr Enrollment: "
    tooltip_title_delayed <- "Delayed 2-yr Enrollment: "
    avg_label <- "Average 2-yr Enrollment"
    title <- "Average 2-Year Seamless and Delayed Enrollment by High School"
  } 
  else if (college_type==4){
    school_seamless <- schoolData$pct_4yr
    agency_seamless <- agencyData$pct_4yr
    school_delayed <- schoolData$pct_4yr_delayed
    order <- schoolData$order_4yr
    tooltip_title_seamless <- "Seamless 4-yr Enrollment: "
    tooltip_title_delayed <- "Delayed 4-yr Enrollment: "
    avg_label <- "Average 4-yr Enrollment"
    title <- "Average 4-Year Seamless and Delayed Enrollment by High School"
  }
  else{
    stop("college_type must be 2 or 4")
  }
  
  bar_1 <- plot_ly(data = schoolData, x = order, y = school_seamless, type="bar", name="Seamless",
                   text = ~paste(
                     "School: ", schoolData$last_hs_name, "<br>",
                     "HS Graduates (N): ", schoolData$hs_diploma, "<br>",
                     tooltip_title_seamless , format(school_seamless, digits = 2), "%", "<br>",
                     tooltip_title_delayed, format(school_delayed, digits = 2), "%",
                     sep=""
                   ),
                   hoverinfo = "text",
                   color = I("navy")
  )
  bar_1
  
  # Add delayed enrollment to graph - turn the hover off using hoverinfo = none
  bar_2 <- add_trace(bar_1, x = order, y = school_delayed, 2, name = "Delayed", 
                     color = I("orange"),
                     hoverinfo = "none"
  )
  bar_2
  
  # Stack them up! Change the barmode to stack in layout
  bar_3 <- layout(bar_2, barmode = "stack")
  bar_3
  
  # Add line and line label for average seamless rate and format the plot with style()
  # Average enrollment line
  avg_line <- list(
    type = 'line',
    line = list(color = "black", dash = 'dash'),
    x0 = 0,
    x1 = 11,
    y0 = agency_seamless,
    y1 = agency_seamless
  )
  # Average enrollment line label
  # Hint: look at the number of schools and divide that in half for the x position 
  avg_line_label <- list(
    text=avg_label,
    x=5.5,
    y=agency_seamless,
    showarrow = FALSE,
    xanchor = 'center',
    yanchor = 'bottom'
  )
  
  bar_3_with_opts <- layout(bar_3, 
                            title = title,
                            xaxis = list(
                              title = "High School",
                              tickvals = 1:length(schoolData$last_hs_name),
                              ticktext = schoolData$last_hs_name,
                              zeroline = TRUE,
                              showline = TRUE
                            ),
                            yaxis = list(
                              title = "High School Graduates (%)",
                              range = c(0,100)
                            ),
                            margin = list(b = 100, t = 100),
                            shapes = list(avg_line),
                            annotations = list(avg_line_label)
  )
  bar_3_with_opts
  
  return(bar_3_with_opts)
  
} # End function

# Shiny App template
# 1 library(shiny)
# 2 ui <- fluidPage()
# 3 server <- function(input, output, session) {}
# 4 shinyApp(ui = ui, server = server)

ui <- ?(
  
  titlePanel("College Enrollment Bar"),
  
  sidebarLayout(
    ?(
      ?("?", label = "Select Enrollment Type:",
                   choices = list("2-year" = ?, "4-year" = ?)
      )
    ),
    ?(plotlyOutput("bar"))
  )
  
)

server <- function(input, output) {
  
  ?$bar <- ?({
    
    bar_function(college_type=?$buttons) 
    
  }) # End render plotly
  
}

shinyApp(?, ?)
