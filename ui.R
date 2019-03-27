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


## ui File
ui <- fluidPage(
  
  ## Application title
  titlePanel("Financial Chart"),
  
  ## Bar Panel
  sidebarPanel(
    h3("Financial Chart"),
    
    # Select Stock here
    selectizeInput("name",
                   label = "Stock of Interest",
                   choices = names$Ticker_and_Company_Name,
                   selected = "AAPL - Apple Inc"),
    
    helpText("Data Source: Yahoo Finance")
  ),
  
  # Candlestick plot
  mainPanel( 
    plotlyOutput("candlestick") 
  ) 
)
