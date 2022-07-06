#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library(RColorBrewer)
options(max.print=100000)

load('rdata/signifdata.Rda')
namelist <- scan('wordlists/names.txt', character(), quote = "")

make_wordcloud <- function(loglikdata) {
    d <- data.frame(word = loglikdata$term, freq = loglikdata$loglik)
    pal <- colorRampPalette(c("#000000", "#BB0000"))(4)
    #wcdir <- '/Users/phb514/mygit/memo-keywords-across-time/wordclouds/'
    #filename <- paste0(wcdir, deparse(substitute(loglikdata)), '.png')
    #png(filename, width=1280,height=800, res=300)
    #wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal)
    wordcloud(d$word,d$freq, scale=c(1.5,.1), max.words=80, random.order=FALSE, rot.per=0, colors=pal)
    #dev.off()
}

targetbrowse <- function(df, refmax, refmin, trgmax, trgmin, remove_names, blacklist, whitelist) {
    df <- df[df$R.count <= refmax & df$R.count >= refmin & df$T.count <= trgmax & df$T.count >= trgmin, ]
    if (remove_names) { df <- df[!df$term %in% namelist, ] }
    if (length(blacklist) > 0) { df <- df[!df$term %in% blacklist, ] }
    if (length(whitelist) > 0) { df <- df[df$term %in% whitelist, ] }
    df
} 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MeMo Keyness Over Time"),
    
    fluidRow(
        column(3, sliderInput("slider2",
                              "Hyppighed i fokus-korpus (Log):",
                              min = 1,
                              max = 14,
                              value = c(1, 14)),
               textOutput("trgRange")),
        
        column(3, sliderInput("slider1",
                              "Hyppighed i ref.-korpus (Log):",
                              min = 1,
                              max = 14,
                              value = c(1, 14)),
               textOutput("refRange")),
        
        column(2, checkboxInput("checkbox1", "Fjern personnavne", value = TRUE, width = NULL)),
        
        column(4, textAreaInput("textarea1",
                              "Blacklist:",
                              rows = 2,
                              cols = 80),
               textAreaInput("textarea2",
                             "Whitelist:",
                             rows = 2,
                             cols = 80))
    ),

    fluidRow(
        column(4,
               fluidRow(column(12, h4('1870-1874'))),
               fluidRow(column(12, plotOutput("wordcloudPlot1", width = "300px", height="250px"))),
               fluidRow(column(12, h4('1875-1879'))),
               fluidRow(column(12, plotOutput("wordcloudPlot2", width = "300px", height="250px")))),
        column(4,
               fluidRow(column(12, h4('1880-1884'))),
               fluidRow(column(12, plotOutput("wordcloudPlot3", width = "300px", height="250px"))),
               fluidRow(column(12, h4('1885-1889'))),
               fluidRow(column(12, plotOutput("wordcloudPlot4", width = "300px", height="250px")))),
        column(4,
               fluidRow(column(12, h4('1890-1894'))),
               fluidRow(column(12, plotOutput("wordcloudPlot5", width = "300px", height="250px"))),
               fluidRow(column(12, h4('1895-1899'))),
               fluidRow(column(12, plotOutput("wordcloudPlot6", width = "300px", height="250px"))))
    ),
    
    hr(),
    h2('Rå data'),
    
    fluidRow(
        column(2, h4('1870-1874'), verbatimTextOutput("data1")),
        column(2, h4('1875-1879'), verbatimTextOutput("data2")),
        column(2, h4('1880-1884'), verbatimTextOutput("data3")),
        column(2, h4('1885-1889'), verbatimTextOutput("data4")),
        column(2, h4('1890-1894'), verbatimTextOutput("data5")),
        column(2, h4('1895-1899'), verbatimTextOutput("data6"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    refmin <- reactive({ round(exp(input$slider1[1])) })
    refmax <- reactive({ round(exp(input$slider1[2])) })
    trgmin <- reactive({ round(exp(input$slider2[1])) })
    trgmax <- reactive({ round(exp(input$slider2[2])) })

    blacklist <- reactive({ unlist(strsplit(input$textarea1, split = "\\s+", perl = TRUE)) })
    whitelist <- reactive({ unlist(strsplit(input$textarea2, split = "\\s+", perl = TRUE)) })
    
    remove_names <- reactive({ input$checkbox1 })
    
    output$refRange <- renderText({ paste0("Range: ", paste(c(refmin(), refmax()), collapse = " - ")) })
    output$trgRange <- renderText({ paste0("Range: ", paste(c(trgmin(), trgmax()), collapse = " - ")) })
    
    output$wordcloudPlot1 <- renderPlot({
        # draw wordcloud
        par(mar = rep(0, 4))
        make_wordcloud(targetbrowse(signifdata$`1870-1874`, refmax(), refmin(), trgmax(), trgmin(), remove_names(), blacklist(), whitelist()))
    })

    output$wordcloudPlot2 <- renderPlot({
        # draw wordcloud
        par(mar = rep(0, 4))
        make_wordcloud(targetbrowse(signifdata$`1875-1879`, refmax(), refmin(), trgmax(), trgmin(), remove_names(), blacklist(), whitelist()))
    })
    
    output$wordcloudPlot3 <- renderPlot({
        # draw wordcloud
        par(mar = rep(0, 4))
        make_wordcloud(targetbrowse(signifdata$`1880-1884`, refmax(), refmin(), trgmax(), trgmin(), remove_names(), blacklist(), whitelist()))
    })
    
    output$wordcloudPlot4 <- renderPlot({
        # draw wordcloud
        par(mar = rep(0, 4))
        make_wordcloud(targetbrowse(signifdata$`1885-1889`, refmax(), refmin(), trgmax(), trgmin(), remove_names(), blacklist(), whitelist()))
    })
    
    output$wordcloudPlot5 <- renderPlot({
        # draw wordcloud
        par(mar = rep(0, 4))
        make_wordcloud(targetbrowse(signifdata$`1890-1894`, refmax(), refmin(), trgmax(), trgmin(), remove_names(), blacklist(), whitelist()))
    })
    
    output$wordcloudPlot6 <- renderPlot({
        # draw wordcloud
        par(mar = rep(0, 4))
        make_wordcloud(targetbrowse(signifdata$`1895-1899`, refmax(), refmin(), trgmax(), trgmin(), remove_names(), blacklist(), whitelist()))
    })
    
    output$data1 <- renderPrint({signifdata$`1870-1874`})
    output$data2 <- renderPrint({signifdata$`1875-1879`})
    output$data3 <- renderPrint({signifdata$`1880-1884`})
    output$data4 <- renderPrint({signifdata$`1885-1889`})
    output$data5 <- renderPrint({signifdata$`1890-1894`})
    output$data6 <- renderPrint({signifdata$`1895-1899`})
    }

# Run the application 
shinyApp(ui = ui, server = server)
