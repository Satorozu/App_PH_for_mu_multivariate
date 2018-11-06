library(shiny)
library(markdown)
shinyUI(pageWithSidebar(
  headerPanel(title=HTML("Prueba de hipótesis sobre &mu; Normal multivariada"),
              windowTitle="PH media"),
  sidebarPanel(
    h5('Esta aplicación permite realizar una prueba de hipótesis sobre el vector de medias poblacionales &mu de una distribución normal multivariada.'),
    
    h6('La aplicación usa por defecto el ejemplo 5.2 página 214 de Jhonson & Wichern (2007) 
del libro guía Applied Multivariate Statistical Analysispero, aunque el usuario
       puede cargar su propia base de datos.'),
    
    fileInput(inputId='file1',
              label='Use el siguiente botón para cargar su base de datos.',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )),
    
    checkboxInput(inputId='header',
                  label='¿Tiene encabezado la base de datos?', 
                  value=TRUE),
    
    selectInput(inputId="sep",
                label = "¿Cuál es la separación de los datos?", 
                choices = list(Tab='\t', Comma=',',
                               Semicolon=';', 'space'=' '),
                selected = ';'),
    
    checkboxGroupInput(inputId="names", 
                       label = " Elija la variables cuantitativa para realizar la prueba de hipótesis.", 
                       choices = c("x1","x2","x3")),
                       
   p(strong(HTML("Ingrese el valor de referencia
                            &mu;<sub>0</sub> para probar
                H<sub>0</sub>: &mu; = &mu;<sub>0</sub>. Escriba los valores del vector separados por un espacio, así como se muestra a continuación:"))),
    tags$textarea(id="vectorIng", cols=20, rows=2, "4 50 10"),#cambiar para que sea varias filas
   

    sliderInput(inputId='alfa',
                label=HTML("Elija un nivel de confianza para realizar la prueba de hipótesis
                           para el vector de media &mu;"),
                min=0.90, max=0.99,
                value=0.95, step=0.005),
    
    img(src="unal.png", height = 60, width = 120),
    br(),
    br(),
    tags$a(href="https://srunal.github.io", "https://srunal.github.io"),
    br(),
    h6("Santiago Toro Z."),
    h6("Freddy Hernández B.")
   
    
    ),
  
  mainPanel(
    tabsetPanel(type = "pills",
                
                tabPanel("Verificación de Supuestos",
                         
                         h5("Prueba de normalidad multivariada Q-Q Plot"),
                         plotOutput("qqplot", width='500px'),
                         
                         h5(" Tabla de resumen prueba Royston"),
                         verbatimTextOutput("royston"),
                         h5(" Tabla de resumen prueba Mardía"),
                         verbatimTextOutput("mardia")),
                         
                         
                tabPanel("Resultados",
                         
                         h4("Valor-P de la prueba"),
                         textOutput("titleValorp"),
                         plotOutput("grafico1", width='500px'),
                         
                         h4("Vector de media muestral"),
                         verbatimTextOutput('med_muestra'),
                         h4("Media de media supuesto"),
                         verbatimTextOutput('med_ho'),
                         h4("Matriz de Varianza Covarianza"),
                         verbatimTextOutput('S_muestra'),
                         
                         
                         
                         h4("- Resultados de la prueba de hipótesis:"),
                         textOutput("resul1")),
                
                tabPanel("Datos", 
                         "A continuación los datos que está usando 
                         la aplicación, fueron tomados del libro guía Applied Multivariate Statistical Analysis. ", uiOutput('inputData')),
                
                tabPanel("Teoría", 
                         includeHTML("Include.html"))
                
                )
                )
  
    ))