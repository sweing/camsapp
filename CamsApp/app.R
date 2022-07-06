rm(list=ls())

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggiraph)
library(scales)
library(data.table)
library(zoo)
library(ggtext)



load("prepared.rData") 
data = data[order(name, variable, basetime)]
#data = data[basetime <= "2020-12-31"]
data = data[, group := .GRP, c("city_id", "name", "name_adv")]
data[, `:=`(month = as.integer(format(basetime, "%m")),
            monthstr = format(basetime, "%b"),
            year = as.integer(format(basetime, "%Y")),
            day = as.integer(format(basetime, "%d")))]

vars = c('Composite moving average',
         '365d moving average')

#s_var = 'no2_rmean_c'
#variable = '365d mean'

#data = data[variable %in% vars & !is.na(value)]

monthlyRaceName = "Monthly index race"
yearlyRaceName = "Yearly index race"

set.seed(872436)

color_lty_cross = data.frame(group = 1:60,
                             ltypes = 1:6,
                             colors = sample(rainbow(60, start = 1/6)))

data = merge(data, color_lty_cross, all.x = TRUE, by = "group")
data[, monyear := paste(monthstr, year)]

cities = sort(unique(data$name))

ui = fluidPage(
  fluidRow(
    column(1),
    column(2,
           pickerInput("cities", NULL, choices = cities, options = list(`actions-box` = TRUE), multiple = TRUE, selected = cities)),
    column(2,
           selectInput("vars", NULL, vars)),
    column(2,
           selectInput("ind", NULL, c(monthlyRaceName,
                                      yearlyRaceName,
                                      "Time series", 
                                      "Rank",
                                      "Rank change from Jan 2018 = 0",
                                      "Index (Jan 2018 = 1.0)"))),
    column(2,
           selectInput("monyear", NULL, rev(unique(data$monyear)))),
    column(1, checkboxInput("islog", "log", FALSE),
           verbatimTextOutput("value"))
    # column(1,
    #        selectInput("raceyear", NULL, rev(unique(data$year)))),
  ),
  fluidRow(
    column(1),
    column(10, girafeOutput("plot2int", width = "100%", height = "600px"))
  ),
  fluidRow(
    column(1),
    column(8, div(tags$p(a("@decarbnow", href = "https://twitter.com/DecarbNow", target = "_blank"), "| data from ", 
                         a("@CopernicusECMWF", href = "https://twitter.com/CopernicusECMWf", target = "_blank"), "- ", 
                         a("Copernicus air quality COVID19 response on GitHub", href = "https://github.com/CopernicusAtmosphere/air-quality-covid19-response", target = "_blank"), "- ",
                         a("Project on GitHub", href = "https://github.com/sweing/camsapp", target = "_blank")),
                  style = "text-align: right;"))
  ),
  
  
  
)


server = function(input, output, session) {
  cityInput = reactive({
    input$cities
  })
  
  indicatorInput = reactive({
    input$ind
  })
  
  logInput = reactive({
    input$islog
  })
  
  varsInput = reactive({
    input$vars
  })
  
  monYearInput = reactive({
    input$monyear
  })
  
  raceYearInput= reactive({
    substr(input$monyear, 5, 8)
  })
  
  
  
  plot.label = reactive({
    if (indicatorInput() == "Index (Jan 2018 = 1.0)"){
      "surface concentration, Jan 2018 = 1.0"
    } else if (indicatorInput() == "Rank"){
      "surface concentration rank"
    } else if (indicatorInput() == "Rank change from Jan 2018 = 0"){
      "surface concentration rank change"
    } else if (indicatorInput() == monthlyRaceName){
      "surface concentration, 1st month day = 1.0"
    } else if (indicatorInput() == yearlyRaceName){
      "surface concentration, 1st year day = 1.0"
    } else {
      "surface concentration [µg/m³]"
    }
  })
  
  select.df = reactive({
    
    if(indicatorInput() == monthlyRaceName)
      data[basetime <= max(data[monyear == monYearInput()]$basetime)]
    if(indicatorInput() == yearlyRaceName)
      data[basetime <= max(data[year == raceYearInput()]$basetime)]
    else
      data
  })
  
  plot.df = reactive({
    if (indicatorInput() == "Index (Jan 2018 = 1.0)"){
      tmp = select.df()[variable == varsInput()]
      
      if(logInput() == TRUE){
        tmp[, value := log(value)]
      }
      
      tmp = tmp[, .(value = value/value[basetime == "2018-01-01"], 
                    basetime = basetime), 
                by = c("city_id", "name", "name_adv", "variable", "ltypes", "colors")]
      
      tmp[order(name, variable, basetime)]
    } else if (indicatorInput() == "Rank"){
      tmp = select.df()[variable == varsInput()]
      
      if(logInput() == TRUE){
        tmp[, value := log(value)]
      }
      
      tmp = tmp[, value := rank(value), by = c("basetime", "variable")]
      tmp[order(name, variable, basetime)]
    } else if (indicatorInput() == "Rank change from Jan 2018 = 0"){
      tmp = select.df()[variable == varsInput()]
      
      if(logInput() == TRUE){
        tmp[, value := log(value)]
      }
      
      tmp = tmp[, value := rank(value), by = c("basetime", "variable")]
      tmp = tmp[, .(value = value - value[basetime == "2018-01-01"], 
                    basetime = basetime), 
                by = c("city_id", "name", "name_adv", "variable", "ltypes", "colors")]
      tmp[order(name, variable, basetime)]
    } else if (indicatorInput() == monthlyRaceName){
      tmp = select.df()[variable == varsInput() & year == raceYearInput()]
      
      if(logInput() == TRUE){
        tmp[, value := log(value)]
      }
      
      tmp = tmp[monyear == monYearInput()][, .(value = value/value[1], 
                                               basetime = basetime,
                                               day = day), 
                                           by = c("city_id", "name", "name_adv", "variable", "ltypes", "colors")]
      
      tmp[order(name, variable, basetime)]
    } else if (indicatorInput() == yearlyRaceName){
      tmp = select.df()[variable == varsInput()]
      
      if(logInput() == TRUE){
        tmp[, value := log(value)]
      }
      
      tmp = tmp [year == raceYearInput()][, .(value = value/value[1], 
                                              basetime = basetime,
                                              day = day,
                                              year = year,
                                              month = month
      ), 
      by = c("city_id", "name", "name_adv", "variable", "ltypes", "colors")]
      tmp[order(name, variable, basetime)]
    } else {
      tmp = select.df()[variable == varsInput()]
      
      if(logInput() == TRUE){
        tmp[, value := log(value)]
      }
      
      tmp[order(name, variable, basetime)]
    }
  })
  
  outputPlot2 = reactive({
    if(is.null(cityInput())){
      
    } else {
      expand_days = 33 - max(select.df()[year == max(year)][variable == varsInput()][monyear == monYearInput()]$day)
      colLines = unique(data.frame(colors = plot.df()[basetime >= "2018-01-01" & variable == varsInput() &
                                                        name %in% cityInput()]$colors,
                                   ltypes = plot.df()[basetime >= "2018-01-01" & variable == varsInput() &
                                                        name %in% cityInput()]$ltypes))
      
      gg = ggplot(plot.df()[basetime >= "2018-01-01" & variable == varsInput() & 
                              name %in% cityInput()], 
                  aes(x = basetime, y = value, color = name, linetype = name, tooltip = name, data_id = name)) +
        geom_line_interactive(size = 1) +
        scale_color_manual(values = colLines$colors) +
        scale_linetype_manual(values = colLines$ltypes) +
        #geom_vline(aes(xintercept = as.Date("2020-03-01")), linetype = "dashed", size = 0.2) +
        # annotate("text", x = as.Date("2020-03-01"), 
        #          y = max(plot.df()[basetime >= "2018-01-01" & variable %in% c("365d mean") & 
        #                            name %in% cityInput()]$value), 
        #          label = "Mar 2020",
        #          angle = 90,
        #          fontface = "plain",
        #          size = 5, color = "black", vjust = -0.5, hjust = "inward") +
        scale_x_date(labels = date_format("%b %Y"), 
                     date_breaks = "1 month", 
                     expand = expansion(mult = c(0, 0), 
                                        add = c(0, expand_days)))  + 
        theme_grey() + 
        theme(panel.grid.minor = element_blank(),
              plot.title= element_text(face='bold'),
              plot.subtitle = element_text(size = 16),
              plot.caption = element_text(size = 16),
              text = element_text(size = 16),
              plot.margin = unit(c(1,1,4,1), "lines"),
              legend.background = element_rect(fill=alpha("lightgray", 0.4), color=NA),
              legend.key.width = unit(1, "cm"),
              legend.text = element_text(size = 16),
              axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
              axis.text.y = element_text(size = 16),
              legend.position = "top",
              legend.title = element_blank()) +
        ylab(plot.label()) +
        xlab(NULL) +
        labs(title = "CAMS NO2 daily mean analysis", 
             #caption = "@decarbnow; data from @CopernicusECMWF"
             subtitle = "Comparison of 365 days/composite moving average (log)")
      
      
      if(indicatorInput() == monthlyRaceName){
        maxDay = unique(plot.df()[day == max(day)]$basetime)
        vLineColor = "gray"
        vLineSize = 1
        vLineType = "dashed"
        
        if(maxDay == as.Date(as.yearmon(maxDay, "%b%Y"), frac = 1)){
          vLineColor = "red"
          vLineType = "solid"
          winner = unique(plot.df()[basetime == maxDay & variable == varsInput()][value == min(value)]$name)
          #print(winner)
          
          gg = gg +
            annotate("label", x = maxDay-15, 
                     y = plot.df()[basetime == maxDay & variable == varsInput()][value == min(value)]$value+0.005,
                     label = winner,
                     label.padding = unit(1, "lines"),
                     size = 10,
                     color = plot.df()[basetime == maxDay & variable == varsInput()][value == min(value)]$colors,
                     hjust = 0.5)
          # annotate("text", x = maxDay, 
          #          y = plot.df()[basetime == maxDay & variable %in% c("365d mean")][value == min(value)]$value,
          #          label = winner,
          #          color = plot.df()[basetime == maxDay & variable %in% c("365d mean")][value == min(value)]$colors,
          #          hjust = "outward")
          
          
        } else {
          gg = gg + geom_vline(aes(xintercept = as.Date(as.yearmon(maxDay, "%b%Y"), frac = 1)), 
                               linetype = vLineType, 
                               size = vLineSize,
                               color = "red")
        }
        
        
        gg = gg +
          geom_vline(aes(xintercept = as.Date(maxDay)), 
                     linetype = vLineType, 
                     size = vLineSize,
                     color = vLineColor) +
          scale_x_date(labels = date_format("%d %b %Y"), 
                       date_breaks = "1 day", 
                       expand = expansion(mult = c(0, 0), 
                                          add = c(0, expand_days)))
      } else if (indicatorInput() == yearlyRaceName){
        maxDay = unique(plot.df()[month == max(month)][day == max(day)]$basetime)
        vLineColor = "gray"
        vLineSize = 1
        vLineType = "dashed"
        #print(maxDay)
        if(maxDay == as.Date(paste0(year(maxDay),"-12-31"))){
          vLineColor = "red"
          vLineType = "solid"
          winner = unique(plot.df()[basetime == maxDay & variable == varsInput()][value == min(value)]$name)
          #print(winner)
          
          gg = gg +
            annotate("label", x = maxDay-150, 
                     y = plot.df()[basetime == maxDay & variable == varsInput()][value == min(value)]$value+0.005,
                     label = winner,
                     label.padding = unit(1, "lines"),
                     size = 10,
                     color = plot.df()[basetime == maxDay & variable == varsInput()][value == min(value)]$colors,
                     hjust = 0.5)
          # annotate("text", x = maxDay, 
          #          y = plot.df()[basetime == maxDay & variable %in% c("365d mean")][value == min(value)]$value,
          #          label = winner,
          #          color = plot.df()[basetime == maxDay & variable %in% c("365d mean")][value == min(value)]$colors,
          #          hjust = "outward")
          
          
        } else {
          gg = gg + geom_vline(aes(xintercept = as.Date(paste0(year(maxDay),"-12-31"))), 
                               linetype = vLineType, 
                               size = vLineSize,
                               color = "red")
        }
        
        
        gg = gg +
          geom_vline(aes(xintercept = as.Date(maxDay)), 
                     linetype = vLineType, 
                     size = vLineSize,
                     color = vLineColor) +
          scale_x_date(labels = date_format("%b %Y"), 
                       date_breaks = "1 month", 
                       expand = expansion(mult = c(0, 0), 
                                          add = c(0, expand_days)))
      }
      
      return(gg)
    }
    
    
  })
  
  output$plot2int = renderGirafe({
    
    if(is.null(cityInput())){
      
    } else {
      
      
      girafe(ggobj = outputPlot2(),
             width_svg = 16,
             height_svg = 10,
             options = list(
               opts_zoom(max = 5),
               opts_hover_inv(css = "opacity:0.3;"),
               opts_hover(css = "stroke-width:5;"),
               opts_selection(type = "none")
             ))
    }
  })
}

shinyApp(ui = ui, server = server)
