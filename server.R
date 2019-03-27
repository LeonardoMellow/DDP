library(shiny)
library(plotly)
library(quantmod)

## Companies Tickers and Names
names <- matrix(c("AMD", "Adv Micro Devices",
                  "BAC", "Bank of America Corp",
                  "AAPL", "Apple Inc",
                  "F", "Ford Motor Company",
                  "GE", "General Electric Company",
                  "MSFT", "Microsoft Corp",
                  "T", "AT&T Inc",
                  "NEM", "Newmont Mining Corp",
                  "COTY", "Coty Inc",
                  "RF", "Regions Financial Corp",
                  "INTC", "Intel Corp",
                  "TWTR", "Twitter Inc",
                  "DIS", "Walt Disney Company",
                  "FB", "Facebook Inc",
                  "KO", "Coca-Cola Company",
                  "NKE", "Nike Inc",
                  "EBAY", "Ebay Inc",
                  "NFLX", "Netflix Inc",
                  "KHC", "Kraft Heinz CO",
                  "MDLZ", "Mondelez Intl Cmn A"), ncol = 2, byrow = T) %>% 
  as.data.table()

colnames(names) <- c("Ticker", "Company_Name")
names[, Ticker_and_Company_Name := paste(Ticker, Company_Name, sep = " - ")]


## server File
server <- function(input, output) {
  
  ## Reactive stock chosen from dropdown menu
  stock <- reactive({
    names[Ticker_and_Company_Name == input$name, Ticker]
  })
  
  ## The company name is also reactive
  CN <- reactive({
    names[Ticker_and_Company_Name == input$name, Company_Name]
  })
  
  ## Both stock and company name will be used as "functions" later
  
  ## Here we render a Plotly plot using the output from the ui file (candlestick)
  output$candlestick <- renderPlotly({
    
    ## First we prepare the dataset. Getting data from Yahoo Finance based
    ## on the chosen stock. Note the stock() will change because of the 
    ## reactive command from above
    foo <- getSymbols(stock(), src = 'yahoo', env = NULL, from = "2019-01-01")
    foo <- data.table(Date = index(foo), coredata(foo))
    
    colnames(foo) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    
    ## Same here for the stock(). Still preparing the database
    foo[, Ticker := stock()]
    foo <- left_join(foo, names, by = "Ticker") %>% as.data.table
    
    ## Adding the Bollinger Bands to the Candlestick plot
    bbands <- BBands(foo[, .(High, Low, Close)])
    
    ## Joining and subseting the data
    # foo <- subset(cbind(foo, data.frame(bbands[,1:3])), Date >= "2018-01-01")
    foo <- cbind(foo, data.frame(bbands[,1:3])) %>% as.data.table
    
    ## Adding diferent colors for the increasing and decreasing volumes
    for (i in 1:nrow(foo)) {
      if ( foo[i, Close] >= foo[i, Open] ) {
        foo$direction[i] = 'Increasing'
      } else {
        foo$direction[i] = 'Decreasing'
      }
    }
    
    i <- list(line = list(color = '#008000'))
    d <- list(line = list(color = '#ff0000'))
    
    ## First plot: Candlestick with BB and Mv Avg but without Volume
    p <- foo %>%
      plot_ly(x = ~Date, type="candlestick",
              open = ~Open, close = ~Close,
              high = ~High, low = ~Low, name = stock(),
              increasing = i, decreasing = d) %>%
      add_lines(x = ~Date, y = ~up , name = "Bollinger Bands",
                line = list(color = '#ccc', width = 0.5),
                legendgroup = "Bollinger Bands",
                hoverinfo = "none", inherit = F) %>%
      add_lines(x = ~Date, y = ~dn, name = "Bollinger Bands",
                line = list(color = '#ccc', width = 0.5),
                legendgroup = "Bollinger Bands", inherit = F,
                showlegend = FALSE, hoverinfo = "none") %>%
      add_lines(x = ~Date, y = ~mavg, name = "Moving Avg.",
                line = list(color = '#E377C2', width = 0.5),
                hoverinfo = "none", inherit = F) %>%
      layout(yaxis = list(title = "Price"))
    
    ## Adding Volume
    pp <- foo %>% plot_ly(x = ~Date, y = ~Volume, type = 'bar', name = "Volume",
                          color = ~direction, colors = c('#008000','#ff0000'), alpha = .4) %>%
      layout(yaxis = list(title = "Volume"))
    
    ## Adding Range Selector
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                 list(count = 1,
                      label = 'RESET',
                      step = 'all'),
                 list(count = 1,
                      label = '1 YR',
                      step = 'year',
                      stepmode = 'backward'),
                 list(count = 3,
                      label = '3 MO',
                      step = 'month',
                      stepmode = 'backward'),
                 list(count = 1,
                      label = '1 MO',
                      step = 'month',
                      stepmode = 'backward')
               ))
    
    ## Final plot!
    subplot(p, pp, heights = c(0.7, 0.2), nrows = 2,
            shareX = TRUE, titleY = TRUE) %>%
      layout(title = paste(CN(), ": 2019-01-02 -", Sys.Date()),
             xaxis = list(rangeselector = rs),
             legend = list(orientation = 'h', x = 0.5, y = 1,
                           xanchor = 'center', yref = 'paper',
                           font = list(size = 10),
                           bgcolor = 'transparent'))
    
  })
}
