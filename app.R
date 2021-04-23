library(shiny)
library(shinyFeedback)
library(readr)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
    headerPanel("Information of your Identification Number"),
    
    sidebarPanel(
        shinyFeedback::useShinyFeedback(),
        textInput(inputId = "ic",
        label="Your Identification number:",
        placeholder = "Example: 961212015356"),
        textOutput("valid")
    ),
    
    
    mainPanel(
        p('Your Date of Birth:'),
        verbatimTextOutput("dob"),
        
        p('Your area:'),
        verbatimTextOutput('area'),
        
        p('Your Gender'),
        verbatimTextOutput('gender')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    valid <- reactive({
        req(input$ic)
        length <- nchar(input$ic)==12
        shinyFeedback::feedbackWarning("ic",!length,"Please input a valid IC number.","red")
    })
    
    output$valid <- renderText(valid())
    
    output$dob <- renderPrint({
        
        dob<-substr(input$ic,1,6)
        
        if (nchar(dob)==6){
            dob<-fast_strptime(dob,"%y%m%d",tz="",cutoff_2000 = 21L)
            dob
        } else "waiting valid input..."
        
    })
    
    output$area <- renderPrint({
        
        data <- read.csv("ic_state.csv")
        
        area<-substr(input$ic,7,8)
        
        if (nchar(area)==2){
            data[data$Code==as.numeric(area),2]
        }else "waiting valid input..."
        
    })
    
    output$gender <- renderPrint({

        gender<-substr(input$ic,9,12)
        
        if (nchar(gender)==4){
            gender<-as.numeric(gender)
            if (gender%%2==1){"Male"}else{"Female"}
        }else "waiting valid input..."
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


