## Use R shiny to create interactive dashboard with art dataset
### R-Shiny
library(shiny)

### Define UI for application that draws a histogram

 ui <- fluidPage(
       titlePanel("ACME Art Company Dashboard"),
       
       sidebarLayout(
           sidebarPanel(tags$style(".well {background-color: #528888;"),
                        plotOutput("yearlyReceipts"),
                        selectInput("store","Select Store:", choices = c("None","Portland",
                                                                         "Davenport","Syracuse","Dublin")),
                        selectInput("year","Select Year:",choices=c("All","2012","2013","2014","2015"))),
       
    
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("storePaper"),
           plotOutput("storeEmployee")
        )
    )
)

### Define server logic required to draw a histogram

 server <- function(input, output) {
    art<-read.csv("art.csv")
    
    watercolor.col<-"cadetblue1"
    drawing.col<-"antiquewhite"
    
    output$yearlyReceipts<-renderPlot({
        my.title<-"Number of receipts per year"
        barplot(table(art$year),main=my.title,border = "white",col = "chartreuse4")
    })
    
    output$storePaper <- renderPlot({
        if(input$store != "None") {
            sub.index <- which(art$store == input$store)
            tmp.data <- art[sub.index,]
            if(input$year != "All") {
                sub.index.2<-which(tmp.data$year == as.numeric(input$year))
                tmp.data<-tmp.data[sub.index.2,]
            }
            par(mfrow=c(1,2))
            sales.by.paper<-tapply(tmp.data$total.sale,list(tmp.data$paper),sum)
            
            barplot(sales.by.paper,beside = T,main = "Revenue by paper type",col=c(watercolor.col,drawing.col),
                    legend.text = rownames(sales.by.paper),border = NA)
            
            sales.by.rep<-tapply(tmp.data$total.sale, list(tmp.data$rep),sum)
            sales.by.rep<-sales.by.rep[which(!is.na(sales.by.rep))]
            pie(sales.by.rep,border = NA,main="Total Sale by Rep",col=terrain.colors(length(sales.by.rep)))
        }
    })

    
    output$storeEmployee <- renderPlot({
        if(input$store != "None") {
            sub.index <- which(art$store == input$store)
            tmp.data <- art[sub.index,]
            if(input$year != "All") {
                sub.index.2<-which(tmp.data$year == as.numeric(input$year))
                tmp.data<-tmp.data[sub.index.2,]
            }
            par(mfrow=c(1,2))
            sales.by.paper<-tapply(tmp.data$total.sale,list(tmp.data$paper,tmp.data$rep),sum)
            
            barplot(sales.by.paper,beside = T,main = "Revenue by paper type",col=c(watercolor.col,drawing.col),
                    legend.text = rownames(sales.by.paper),border = NA)
            
            
            par(mar=c(5,8,4,2),bty="n")
            reps=unique(tmp.data$rep)
            boxplot(tmp.data$units.sold~tmp.data$rep,horizontal=T,yaxt="n",main="Distribution of units sold",
                    col=terrain.colors(length(reps)))
            axis(side = 2,at=1:length(reps),reps,las=2)
        }
    })
 }


### Run the application 
shinyApp(ui = ui, server = server)
