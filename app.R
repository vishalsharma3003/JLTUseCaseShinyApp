#devtools::install_github("jcheng5/googleCharts")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(DT)
library(ggplot2)
library(plotly)
library(threejs)
library(rbokeh)
library(googleVis)
library(plyr)
library(rpart.plot)
library(e1071)
library(caret)
library(randomForest)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(RSentiment)
library(r2d3)
library(networkD3)
#library(googleCharts)
#library(devtools)

wb2<- "WC_Data_Science Case Study.csv"

#1. Missing values
# Blank values instead of NA so reload with NA, Commas and hyphen in claim amount
# Now read with NA
compensationsWithNA <- read.csv(wb2,header = T, na.strings = c("","NA"),encoding="UTF-8")
# Remove the commans and hyphen from claim amount
compensationsWithNA$Claim.Cost <- as.numeric(gsub("[,-]","",compensationsWithNA$Claim.Cost))
n<-dim(compensationsWithNA)[1]
na_count <-sapply(compensationsWithNA, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count$percent <- (na_count$na_count/n)*100
compensationsWithNA<-compensationsWithNA[1:(n-9),]

# NA can be removed by replacing with following values:
# 1. Mean
# 2. Median
# 3. Most probably Bayesian value
# 4. k-nearest neighbour
# Let's replace all the cost values with mean 
compensationsWithNA$Claim.Cost[is.na(compensationsWithNA$Claim.Cost)] <- mean(compensationsWithNA$Claim.Cost, na.rm = T)

compensationsWithNA$Litigation <- toupper(compensationsWithNA$Litigation)
# Recheck the na's
n<-dim(compensationsWithNA)[1]
na_count <-sapply(compensationsWithNA, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count$percent <- (na_count$na_count/n)*100

objective3 <- read.csv(wb2,header = T, na.strings = c("","NA"),encoding="UTF-8")
objective3 <- objective3[,c('Cause.Description','High.Cost')]

# Remove all the null rows from dataset
objective3 <- na.omit(objective3)

data2 <- objective3[,'Cause.Description']

# Load the data as a corpus
docs <- Corpus(VectorSource(data2))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm1 <- TermDocumentMatrix(docs[1:20000])

m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)

# dtm2 <- TermDocumentMatrix(docs[28403:38403])
# 
# m2 <- as.matrix(dtm2)
# v2 <- sort(rowSums(m2),decreasing=TRUE)
# d2 <- data.frame(word = names(v2),freq=v2)
# 
dtm3 <- TermDocumentMatrix(docs[38404:48404])

m3 <- as.matrix(dtm3)
v3 <- sort(rowSums(m3),decreasing=TRUE)
d3 <- data.frame(word = names(v3),freq=v3)

#  dtm4 <- TermDocumentMatrix(docs[48405:56803])
# 
# m4 <- as.matrix(dtm4)
# v4 <- sort(rowSums(m4),decreasing=TRUE)
# d4 <- data.frame(word = names(v4),freq=v4)

d1$rn <- rownames(d1)
# d2$rn <- rownames(d2)
d3$rn <- rownames(d3)
# d4$rn <- rownames(d4)
# 
# res <- aggregate(cbind(freq) ~ rn, rbind(d1,d2,d3,d4), sum)
res <- aggregate(cbind(freq) ~ rn, rbind(d1,d3), sum)

# assign the rownames again
rownames(res) <- res$rn

findFreqTerms(dtm1, lowfreq = 40)
# We can also find association of terms on the basis of correlation
findAssocs(dtm1, terms = "pain", corlimit = 0.3)

# One analysis that we can do here is that, on the basis of which body part the case goes to litigtion
# For now I will just create some new variables
objective3$hasStrain <- str_detect(objective3$Cause.Description,pattern = "strain|Strain|STRAIN")
objective3$hasPain <- str_detect(objective3$Cause.Description,pattern = "pain|Pain|PAIN")
objective3$hasShoulderInjury <- str_detect(objective3$Cause.Description,pattern = "(shoulder|Shoulder|SHOULDER)&(injured|Injured|INJURED|Injury|injury|INJURY)")
objective3$hasKneeProblem <- str_detect(objective3$Cause.Description,pattern = "knee|Knee|KNEE")
objective3$hasWristProblem <- str_detect(objective3$Cause.Description,pattern = "wrist|Wrist|WRIST")
objective3$hasSprain <- str_detect(objective3$Cause.Description,pattern = "sprain|Sprain|SPRAIN")
objective3$hasSlipped <- str_detect(objective3$Cause.Description,pattern = "slipped|Slipped|SLIPPED|slip|Slip|SLIP")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "JLT Use Case"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Objective 1", tabName = "objective1"),
      menuItem("Objective 2", tabName = "objective2"),
      menuItem("Objective 3", tabName = "objective3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview", 
              fluidRow(
                column(8,tags$h1("Jardine Lloyd Thompson")),
                column(4,tags$img(src="JLT Logo.png",width = "200px", height = "100px"))),
              fluidRow(
                column(12,tags$p("Jardine Lloyd Thompson Group plc, also known as JLT Group or simply JLT, is a British multinational corporation that has its headquarters in London, England. It provides insurance, reinsurance, employment benefits advice and brokerage services. It was a constituent of the FTSE 250 Index until it was acquired by Marsh & McLennan Companies in April 2019."))),
              fluidRow(
                column(12,tags$h3("Objective:"))),
              fluidRow(
                column(12,tags$p("The risk manager from a consultancy wants to analyse Workers Compensation risk in the United States. He has hired me as a consultant to identify the key factors of high cost workers compensation claim across the US. He provides historical loss experience data. There are 3 objectives to be met in this document.",
                                 tags$ol(tags$li("Explore the claims experience (using graphs/plots), Build a model to predict high cost workers compnesation claim and highlight the key drivers of high cost claims."),
                                         tags$li("Prove that litigation is the key driver of high cost claims using statistical analysis and tests. Predict if the case will go for litigation. Hence drive the objective 1."),
                                         tags$li("Use NLP techniques to engineer features from description.")))))),
      tabItem("objective1",
              fluidRow(
                column(6,
                       fluidRow(tags$p("After removing some missing values, following is the list of NA's left.")),
                       fluidRow(dataTableOutput(outputId = "na_count"))),
                column(6,
                       tags$p(tags$br(),tags$br(),tags$br(),tags$br(),"Further NA's can be removed by replacing with following values:",
                              tags$ol(tags$li("Mean"), tags$li("Median"), tags$li("Most probably Bayesian value"),tags$li("k-nearest neighbour"))))
              ),
              fluidRow(
                radioButtons("variable", "Variables to show:",
                             c("Loss Type" = "Loss.Type",
                               "Accident State" = "Accident.State",
                               "Litigation" = "Litigation",
                               "Carrier" = "Carrier",
                               "Industry Sector" = "Sector.Industry",
                               "Hight Cost" = "High.Cost"),inline = TRUE)
              ),
              fluidRow(
                plotlyOutput("variable_count")
              ),
              fluidRow(
                h3("Sector wise litigation Cases"),
                plotlyOutput("litigation_per_sector")
              ),
              fluidRow(
                h3("Industry wise claim cost and litigation"),
                rbokehOutput("scatter_plot", width = 1000, height = 540)
              ),
              fluidRow(
                h3("State wise high costs"),
                plotlyOutput("highcost_state")
              ),
              fluidRow(
                h3("Average claim cost per Loss type"),
                mainPanel(htmlOutput("gauge_plot"))
              )
              
              # fluidRow(
              #   # This line loads the Google Charts JS library
              #   googleChartsInit(),
              #   # Use the Google webfont "Source Sans Pro"
              #   tags$link(
              #     href=paste0("http://fonts.googleapis.com/css?",
              #                 "family=Source+Sans+Pro:300,600,300italic"),
              #     rel="stylesheet", type="text/css"),
              #   tags$style(type="text/css",
              #              "body {font-family: 'Source Sans Pro'}"
              #   ),
              #   h2("Sector wise litigation Cases"),
              #   googleBubbleChart("chart",
              #                     width="100%", height = "475px",
              #                     # Set the default options for this chart; they can be
              #                     # overridden in server.R on a per-update basis. See
              #                     # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
              #                     # for option documentation.
              #                     options = list(
              #                       fontName = "Source Sans Pro",
              #                       fontSize = 13,
              #                       # Set axis labels and ranges
              #                       hAxis = list(
              #                         title = "Litigation?",
              #                         viewWindow = xlim
              #                       ),
              #                       vAxis = list(
              #                         title = "Count",
              #                         viewWindow = ylim
              #                       ),
              #                       # The default padding is a little too spaced out
              #                       chartArea = list(
              #                         top = 50, left = 75,
              #                         height = "75%", width = "75%"
              #                       ),
              #                       # Allow pan/zoom
              #                       explorer = list(),
              #                       # Set bubble visual props
              #                       bubble = list(
              #                         opacity = 0.4, stroke = "none",
              #                         # Hide bubble label
              #                         textStyle = list(
              #                           color = "none"
              #                         )
              #                       ),
              #                       # Set fonts
              #                       titleTextStyle = list(
              #                         fontSize = 16
              #                       ),
              #                       tooltip = list(
              #                         textStyle = list(
              #                           fontSize = 12
              #                         )
              #                       )
              #                     ))),
      ),
      tabItem("objective2",
              h1("Coming soon")
      ),
      tabItem("objective3",
              fluidRow(
                h3("Bubbles graph"),
                d3Output("bubbles")
              ),
              fluidRow(
                h3("Word Cloud"),
                sliderInput("frequency",
                            "Minimum Frequency:",
                            min = 1,  max = 50, value = 15),
                sliderInput("maxWords",
                            "Maximum Number of Words:",
                            min = 1,  max = 300,  value = 100),
                mainPanel(plotOutput("wordCloud"))
              ),
              fluidRow(
                h3("Directed Graph showing relationship between words"),
                sliderInput("distance",
                            "Distance between nodes:",
                            min = 20,  max = 100, value = 50),
                sliderInput("fontsize",
                            "Font size of words:",
                            min = 1,  max = 15,  value = 10),
                sliderInput("charge",
                            "Charge between nodes:",
                            min = -50,  max = 0,  value = -10),
                mainPanel(simpleNetworkOutput("networkGraph"))
              ),
              fluidRow(
                h3("Resultant dataset after engineering additional features"),
                mainPanel(
                  numericInput(inputId = "rowNumbers",label = "Number of rows:",value = 500),
                  checkboxGroupInput(inputId = "variables", label = "New features to show:",
                                     choices = c("Has Strain?" = "hasStrain",
                                       "Has Pain?" = "hasPain",
                                       "Has Shoulder Injury?" = "hasShoulderInjury",
                                       "Has Knee Problem?" = "hasKneeProblem",
                                       "Has Wrist Problem?" = "hasWristProblem",
                                       "Has Sprain?" = "hasSprain",
                                       "Slipped?" = "hasSlipped"),selected = "hasPain",inline = TRUE)
                ),
                mainPanel(DT::dataTableOutput("finalTable"))
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$na_count <- DT::renderDataTable({
    DT::datatable(na_count, options = list( pageLength = 5))})
  
  gg <- ggplot(data=compensationsWithNA, width=10) +
    geom_bar(mapping =aes(x=compensationsWithNA[,input$variable]),
             position = "dodge")
  output$variable_count <- renderPlotly(ggplotly(gg))
  
  gg2 <- ggplot(data=compensationsWithNA) +
    geom_bar(mapping =aes(x=compensationsWithNA$Litigation, fill=compensationsWithNA$Sector.Industry),
             position = "dodge")+xlab("Litigation")+labs(fill="Sector Industry")
  output$litigation_per_sector <- renderPlotly(ggplotly(gg2))
  
  output$scatter_plot <- renderRbokeh(figure(xlab = "Sector Industry", ylab = "Claim cost",v_symmetry=FALSE,width = 1000, height=500,legend_location="top_left") %>%
                                        ly_points(compensationsWithNA$Sector.Industry, compensationsWithNA$Claim.Cost, data = compensationsWithNA,
                                                  color = compensationsWithNA$Litigation, glyph = compensationsWithNA$Litigation,
                                                  hover = list(compensationsWithNA$Sector.Industry, compensationsWithNA$Claim.Cost)))
  gg3 <- ggplot(data=compensationsWithNA) +
    geom_bar(mapping =aes(x=compensationsWithNA$High.Cost, fill=compensationsWithNA$Accident.State),
             position = "dodge")+xlab("High Cost (Yes or No)") + ylab("Count") + labs(fill="States")
  output$highcost_state <- renderPlotly(ggplotly(gg3))
  
  data <- aggregate( compensationsWithNA$Claim.Cost ~ compensationsWithNA$Loss.Type, compensationsWithNA, mean )
  Gauge <-  gvisGauge(data,
                      options=list(min=0, max=30000, greenFrom=0,
                                   greenTo=12000, yellowFrom=12000, yellowTo=22000,
                                   redFrom=22000, redTo=30000, width=400, height=300))
  output$gauge_plot <- renderGvis({Gauge})
  
  # Let's replace all missing values in litigation as no
  compensationsWithNA$Litigation[is.na(compensationsWithNA$Litigation)] <- 'NO'
  
  
  
  # defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477", "#fcf451")
  # series <- structure(
  #   lapply(defaultColors, function(color) { list(color=color) }),
  #   names = levels(compensationsWithNA$Sector.Industry)
  # )
  # 
  #   df <- compensationsWithNA[,c("Sector.Industry", "Litigation")]
  #   # abcd <- 
  #   #   list(
  #   #     data = googleDataTable(df),
  #   #     options = list(
  #   #       title = sprintf(
  #   #         "Sector wise litigation Cases"),
  #   #       series = series
  #   #     )
  #   #   )
  # 
  #   output$chart <- reactive({
  #   # Return the data and options
  #   list(
  #     data = googleDataTable(df),
  #     options = list(
  #       title = sprintf(
  #         "Sector wise litigation Cases"),
  #       series = series
  #     )
  #   )
  # })
  
  output$bubbles <- renderD3(r2d3(data = read.csv("data.csv"), d3_version = 4, script = "bubbles.js"))
  
  set.seed(1234)
  output$wordCloud <- renderPlot({wordcloud(words = res$rn, freq = res$freq, min.freq = input$frequency,
                                            max.words=input$maxWords, random.order=FALSE, rot.per=0.35, 
                                            colors=brewer.pal(8, "Dark2"))})
  
  # set.seed(1234)
  # wordcloud(words = d4$word, freq = d4$freq, min.freq = 1,
  #           max.words=200, random.order=FALSE, rot.per=0.35,
  #           colors=brewer.pal(8, "Dark2"))
  # 
  # set.seed(1234)
  # wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
  #           max.words=200, random.order=FALSE, rot.per=0.35,
  #           colors=brewer.pal(8, "Dark2"))
  # 
  # set.seed(1234)
  # wordcloud(words = d2$word, freq = d2$freq, min.freq = 1,
  #           max.words=200, random.order=FALSE, rot.per=0.35,
  #           colors=brewer.pal(8, "Dark2"))
  # 
  # set.seed(1234)
  # wordcloud(words = d3$word, freq = d3$freq, min.freq = 1,
  #           max.words=200, random.order=FALSE, rot.per=0.35,
  #           colors=brewer.pal(8, "Dark2"))
  
  directedGraph <- data.frame("Source"=d3[1:50,"word"],"Target"=d1[1:50,"word"])
  # Create graph
  output$networkGraph <- renderSimpleNetwork({simpleNetwork(directedGraph, fontFamily = "sans-serif",
                opacity = 0.6,linkDistance = input$distance,charge=input$charge,fontSize = input$fontsize)})

  output$finalTable <- DT::renderDataTable({
    DT::datatable(objective3[1:input$rowNumbers,c("Cause.Description",input$variables),drop=FALSE], options = list( pageLength = 10))})
  
  # dtm_xyz <- DocumentTermMatrix(docs[1:1000], control=list(wordLengths=c(4, 20)))
  # ttm_results <- t(as.matrix(dtm_xyz)) %*% as.matrix(dtm_xyz)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
