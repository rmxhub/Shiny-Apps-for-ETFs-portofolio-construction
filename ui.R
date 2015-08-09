

library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  titlePanel("Etfs Portfolio Construction"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  sidebarLayout(
    sidebarPanel(
      helpText("Note: If you change the dates, Please download ETFs again!!!"),
	dateRangeInput("ETFdates", label = h3("Date range"),start = "2005-01-01",end='2014-12-31'),      
	actionButton('download', 'Download ETFs'),
      actionButton('compute', 'Compute'),	
	selectInput("mplot", "Select plot:",
                  choices = c("ETFs","portfolio","IWD","DHS","RSP","SPY","IWF","QQQ","DVY","IWS","IWR","MDY","RFG","IWP","IJS","VBR","IWM","PRFZ","IWO","VBK","EEM","EFA","SCZ","EFG","EFV","DLS","GOLD","VNQ","AGG","BNDX")),



	sliderInput("goldp", label = h3("The Range of Alternatives"), min = 0, 
        		max = 1, value = c(0.05, 0.1)), 
	sliderInput("Bondsp", label = h3("The Range of Bonds"), min = 0, 
        		max = 1, value = c(0.15, 0.25)),     

      helpText("Please input the stock percentage:"),      
      numericInput("pdomestic", "Domestic:", 0.2),
      numericInput("largeDom", "Large Cap within Domestic:", 0.7),
      numericInput("smallDom", "Small Cap within Domestic:", 0.1),
      helpText("Please input the emerge market percentage for international stock:"),   
   	numericInput("emergMKT", "Emerge stock within international stock:", 0.25)

    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
	#progressInit(),
      h4("The diversified portfolio setting:"),
      htmlOutput("summary"),
	h4("Plot"),
      #conditionalPanel("input.showplot== TRUE", plotOutput("rockplot")),
	plotOutput("rockplot"),     
      h4("Portfolio Table"),
      tableOutput("view")
    )
  )
))



