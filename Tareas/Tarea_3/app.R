
library(shiny)

sex<-read.csv("sex.csv")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  img(src = "FI.png",height = 100, width = 70, align="left"),
  img(src = "UAQ.png",height = 99, width = 75, align="right"),
  h2("DIPLOMADO CIENCIA MATEMÁTICA DE DATOS", align="center", style="color:red"),
  h3("Salud sexual", align="center", style="color:purple"),
  br(),
  
  # Sidebar layout with input and output definitions ----
  navbarPage(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "pills",
                  tabPanel("Graficar",radioButtons("pri","Elegir:",
                                                   c("Inicio vida sexual"="rel",
                                                     "inicio métodos"="met")), plotOutput("plot")),
                  tabPanel("Resumen",radioButtons("gen","Generación:",
                                                              c("1980-1989"="gen80",
                                                                 "1965-1979"="gen70")),
                           
                                                      verbatimTextOutput("summary")),
                  tabPanel("Tabla", DT::dataTableOutput("table"))

  )
)
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  
 x<-subset(sex[,-(1:5)], sex$Generacion=="1980-1989")
 y<-subset(sex[,-(1:5)], sex$Generacion=="1965-1979")
 
 botons<-reactive(
                   { gen<-switch(input$gen,
                                  gen80 =x,
                                  gen70 =y,
                                   x)
                   
                    })
 r<-sex$Ed1aresex
 t<-sex$Ed1ruso
 botons2<-reactive(
   { pri<-switch(input$pri,
                 rel =r,
                 met =t,
                 r)
   
   })
 
  output$summary <- renderPrint({
    summary(botons())  })
  output$table <- DT::renderDataTable(DT::datatable
    ({sex},options=list(lengthMenu=list(c(10,25,66),c("10","25","Todas"))),
               selection="none"))  
  output$plot<-renderPlot ({
    dotchart(botons2(), labels=sex$Nom_Ent)}, width = 500, height = 1000)
  
}

# Create Shiny app ----
shinyApp(ui, server)
