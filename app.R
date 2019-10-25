#############################
##                         ##
## Lyft/Uber Profitability ##
##      Shiny Web App      ##
##                         ##
##     By: Drew Seewald    ##
##                         ##
#############################

# Libraries ####
library(shiny)
library(shinydashboard)
library(readr)
library(stringr)
library(rvest)

stt <- read_csv("stt.csv", col_names = FALSE)
vehicleCosts <- read_csv("vehicleCosts.csv", col_names = TRUE)

# Server Function ####
# where all the reactive goodness happens

server <- function(input, output) {
	
	# rip average gas price off aaa website with scraping
	# Needs to be modified to just find the numbers
	# Make adaptive to use selected state's average
	# AAAGasURL <- read_html(parse(paste0("https://gasprices.aaa.com/?state=", input$state)))
	# gasPrice <- AAAGasURL %>% html_nodes(".numb") %>% html_text
	# gasPrice
	
	output$realDollars <- renderValueBox({
		dollars <- ((input$tkHm) - (input$mlDrvn / input$MPG * input$cstGas) - (input$crPmnt / 30) - (input$insrnc / 30)) / (input$hrsWrkd)
		
		valueBox(
			value = paste0(if (dollars < 0) {
				"-"
			} else {
				""
			}, "$", formatC(
				abs(dollars), digits = 2, format = "f"
			)),
			subtitle = "Real Pay/Hour",
			icon = icon("credit-card-alt"),
			color = if (dollars > 0) {
				"green"
			} else {
				"red"
			}
		)
	})
	
	output$inMoney <- renderValueBox({
		
		dollars <- ((input$tkHm) - (input$mlDrvn / input$MPG * input$cstGas) - (input$crPmnt / 30) - (input$insrnc / 30)) / (input$hrsWrkd)
		
		valueBox(value = if(dollars <= 0) {"No"} else {"Yes"}, subtitle = "Making Money?", color = if(dollars <= 0) {"red"} else {"green"}
			
		)
	})
	
		
	output$betterWage <- renderValueBox({
		sttName <- str_which(stt$X1, input$state)
		sttWage <- stt[[2]][sttName]
		
		dollars <- ((input$tkHm) - (input$mlDrvn / input$MPG * input$cstGas) - (input$crPmnt / 30) - (input$insrnc / 30)) / (input$hrsWrkd)
		
		valueBox(value = if(dollars <= sttWage) {"No"} else {"Yes"}, subtitle = paste0("Making More Than Minimum Wage in ", input$state, "?", " ($", sttWage, ")"), color = if(dollars <= sttWage) {"red"} else {"green"})
	})
}


# Header ####
# Create the top bar
header <- dashboardHeader(
	title = "Rideshare Profitability",
	dropdownMenu(
		type = "tasks",
		taskItem(
			text = "Add Instructions tab on sidebar"
		),
		taskItem(
			text = "Add thinking when no input is present"
		),
		taskItem(
			text = "Add in scraped gas price functionality",
			value = 10
		),
		taskItem(
			text = "Add car type for aaa maintainence/depreciation cost to be factored in",
			value = 5
		),
		taskItem(
			text = "Add detailed summary tab on sidebar"
		)
	)
)


# Sidebar ####
sidebar <- dashboardSidebar(
	disable = TRUE
)


# Body ####
body <- dashboardBody(
	h1(textOutput("dollarNum")),
	h1("Is it Worth Being a Rideshare Driver?"),
	fluidRow(
		valueBoxOutput("inMoney"),
		valueBoxOutput("realDollars"),
		valueBoxOutput("betterWage")
		
	),
	fluidRow(box(
		width = 12,
		h4("Work Summary"),
		fluidRow(column(
			width = 12,
			box(
				width = 4,
				numericInput(
					inputId = "hrsWrkd",
					label = "Hours Worked",
					value = 8,
					min = 1,
					max = 24,
					step = .25
				)
			),
			box(
				width = 4,
				numericInput(
					inputId = "tkHm",
					label = "Take Home Pay (After Cut)",
					value = 170,
					min = 0,
					max = 2000,
					step = 1
				)
			),
			box(
				width = 4,
				numericInput(
					inputId = "mlDrvn",
					label = "Total Miles Driven",
					value = 130,
					min = 0,
					max = 500,
					step = 1
				)
			)
		)),
		h4("Car"),
		fluidRow(column(
			width = 12,
			box(
				width = 4,
				sliderInput(
					inputId = "MPG",
					label = "Miles Per Gallon (MPG)",
					min = 5,
					max = 100,
					value = 35,
					step = .5,
					ticks = TRUE,
					post = " MPG"
				)
			),
			box(
				width = 4,
				numericInput(
					inputId = "crPmnt",
					label = "Car Payment",
					value = 262,
					min = 0,
					max = 1000,
					step = 5
				)
			),
			box(
				width = 4,
				numericInput(
					inputId = "insrnc",
					label = "Insurance Cost",
					value = 100,
					min = 0,
					max = 1000,
					step = 5
				)
			)
		)),
		h4("Miscellaneous"),
		fluidRow(column(
			width = 12,
			box(
				width = 6,
				numericInput(
					inputId = "cstGas",
					label = "Cost of Gas ($)",
					value = 2.50,
					min = 0,
					max = 6,
					step = .01
				)
			),
			box(
				width = 6,
				selectInput(
					inputId = "state",
					label = "Select Your State",
					choices = stt$X1,
					selected = "Michigan",
					multiple = FALSE
				)
			)
		))
	))
)


# Define UI for application that draws a histogram
ui <- # UI ####
ui <- dashboardPage(header = header, body = body, sidebar = sidebar)

# Run the application 
shinyApp(ui = ui, server = server)

# Deploy to Shinyapps.io
# library(rsconnect)
# setwd("C:/Users/Drew/OneDrive/Data Analysis/Rideshare Profitability/rideshare")
# deployApp()