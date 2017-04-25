if(!"shiny" %in% rownames(installed.packages())){
  install.packages("shiny", dependencies = T)
}
library("shiny")
if(!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2", dependencies = T)
}
library("ggplot2")
if(!"reshape2" %in% rownames(installed.packages())){
  install.packages("reshape2", dependencies = T)
}
library("reshape2")
if(!"lubridate" %in% rownames(installed.packages())){
  install.packages("lubridate", dependencies = T)
}
library("lubridate")
if(!"GGally" %in% rownames(installed.packages())){
  install.packages("GGally", dependencies = T)
}
library("GGally")

facebook <- read.csv("dataset_Facebook.csv", sep = ";", stringsAsFactors = F)
facebook <- facebook[complete.cases(facebook),]
facebook$Type <- factor(facebook$Type)
facebook$Category <- factor(facebook$Category)
facebook$Paid <- factor(facebook$Paid)
facebook$Post.Month <- factor(facebook$Post.Month, levels = 1:12, labels = month.abb, ordered = T)
facebook$Post.Weekday <- factor(facebook$Post.Weekday, levels = 1:7,
                                labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                           "Saturday", "Sunday"), ordered = T)
facebook$Post.Hour <- factor(facebook$Post.Hour, levels = 1:23, labels = 1:23, ordered = T)
factor_names <- names(sapply(facebook, is.factor))[sapply(facebook, is.factor)]
numeric_names <- names(sapply(facebook, is.numeric))[sapply(facebook, is.numeric)]

# scatterplot matrix
# https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}


ui <- fluidPage(
  headerPanel("Multivariate"),
  
  conditionalPanel("input.conditionedPanels==1",
                   sidebarPanel(width = 3,
                                selectInput("x_axis", "Select X Axis", colnames(facebook), selected = "Post.Hour"),
                                selectInput("y_axis", "Select Y Axis", colnames(facebook)),
                                selectInput("color", "Select color variable", factor_names, selected = "Category"),
                                selectInput("size", "Select size variable", factor_names, selected = "Type"),
                                sliderInput("bubble_size", "Change size of bubbles", 1, 15, c(2, 10), 1, ticks = F),
                                sliderInput("transparency", "Change transparency of bubbles", 0, 1, .75, .05, ticks = T))
  ),
  
  conditionalPanel("input.conditionedPanels==2",
                   sidebarPanel(width = 3,
                                selectizeInput("scatter_cols", "Select variables to plot", numeric_names,
                                               multiple = T, selected = c("Page.total.likes", "Lifetime.Post.Total.Reach")),
                                selectInput("scatter_color", "Select color variable", factor_names))
  ),
  
  conditionalPanel("input.conditionedPanels==3",
                   sidebarPanel(width = 3,
                                selectizeInput("parallel_cols", "Select parallel variables to plot",
                                               colnames(facebook), multiple = T,
                                               selected = c("Page.total.likes", "Lifetime.Post.Total.Reach")),
                                strong("Do not select the color variable within the plot variables."),
                                br(),br(),
                                selectInput("parallel_color", "Select color variable", factor_names))
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Bubble plot",
               plotOutput("bubble_plot", hover = hoverOpts(id ="bubble_plot_hover")),
               verbatimTextOutput("bubble_hover_info"),
               value = 1),
      tabPanel("Scatter plot",
               plotOutput("scatter_plot"),
               value = 2),
      tabPanel("Parallel coordinates plot",
               plotOutput("parallel_plot"),
               value = 3),
      id = "conditionedPanels"
    )
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ###############
  # BUBBLE PLOT #
  ###############
  output$bubble_plot <- renderPlot({
    ggplot(facebook, aes_string(x=input$x_axis,
                                y=input$y_axis,
                                color=input$color,
                                size=input$size)) +
      geom_point(alpha = input$transparency) +
      scale_size_discrete(range = input$bubble_size) +
      theme_bw()
  })
  
  output$bubble_hover_info <- renderPrint({
    if(!is.null(input$bubble_plot_hover)){
      hover=input$bubble_plot_hover
      points <- nearPoints(facebook, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
      if(nrow(points)>0){
        for(col in colnames(facebook)){
          cat(col, ": ", points[1,col], "\n", sep="")
        }
      }
    } else{
      cat("Hover on a point to see additional information.")
    }
  })
  
  ################
  # SCATTER PLOT #
  ################
  
  output$scatter_plot <- renderPlot({
    validate(
      need(length(input$scatter_cols) >= 2, label = "At least 2 variables")
    )
    gg1 = makePairs(facebook[,input$scatter_cols])
    
    # new data frame mega iris
    mega_facebook = data.frame(gg1$all, Type=rep(facebook[,input$scatter_color], length=nrow(gg1$all)))
    
    # pairs plot
    ggplot(mega_facebook, aes_string(x = "x", y = "y")) + 
      facet_grid(xvar ~ yvar, scales = "free") + 
      geom_point(aes(colour=Type), na.rm = TRUE, alpha=0.8) + 
      stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                   data = gg1$densities, position = "identity", 
                   colour = "grey20", geom = "line") +
      theme_bw() +
      xlab("") + ylab("")
  })
  
  ##################
  # PARALLEL LINES #
  ##################
  
  output$parallel_plot <- renderPlot({
    validate(
      need(length(input$parallel_cols) >= 2, label = "At least 2 variables")
    )
    
    ggparcoord(facebook, columns = which(colnames(facebook) %in% input$parallel_cols),
               groupColumn = which(colnames(facebook) == input$parallel_color),
               scale = "uniminmax", showPoints = T) +
      theme_bw() +
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
