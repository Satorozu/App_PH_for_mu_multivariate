library(shiny)

shinyServer(function(input,output,session){

#Base de datos------------
  observe({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header,
                        sep=input$sep)
   updateCheckboxGroupInput(session, "names",
                             choices = colnames(dt[,]),
                             selected = c("x1","x2","x3"))
                             
  })
  
  output$inputData <- renderTable({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    dt
  })
  
  #Esta.Prue-----
  
  To<-function(base,alpha,M0){
    Mmues<-c(unname(colMeans(base))) #varia segun la base sumistrada
    n<- nrow(base) ##DEBE VARIAR SEGUN LA SELECCIÓN
    S<-cov(base) # DEBE VARIAR SEGUN LA SELECCIÓN
    Scom<-solve(S)
   #obtencion del valor del EP-------
    T0<-n*t(Mmues-M0)%*%Scom%*%(Mmues-M0)
    p<-length(M0) # VARIA segÚn el tamaño de variables escogidas para el vector
    a<-((n-1)*p/(n-p))
    f<-(qf(alpha,p,n-p))*a
    res<-list(estadistico=T0,p=p,n=n,Mmues=Mmues,S=S,
              valor=f)
    return(res)
  }
  
#para mostrar-----------------
  output$med_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    #manera de entrar vector de medias--------------
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    Mprima<- To(base = dt[,vv], alpha=input$alfa, M0=vectorMed)
    round(Mprima$Mmues,2)
          })
  
  output$med_ho <- renderPrint({
    #manera de entrar vector de medias--------------
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    round(vectorMed,2)
    
    })
  
  output$S_muestra <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vv <- c(input$names)
    #manera de entrar vector de medias--------------
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    Mprima<- To(base = dt[,vv], alpha=input$alfa, M0=vectorMed)
    round(Mprima$S,2)
    })

  output$grafico1 <- renderPlot({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    ph <- To(base = dt[,vv], alpha=input$alfa, M0=vectorMed)
    curve(df(x, df1=ph$p, df2=ph$n-ph$p),
          from=0, to=10, ylab="Densidad",
          las=1, lwd=3, col="deepskyblue3")
    grid()
  })
  
  output$titleValorp<- renderText({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    #manera de entrar vector de medias--------------
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    ph <- To(base = dt[,vv], alpha=input$alfa, M0=vectorMed)
    paste0('Distribución F (', ph$p,",",ph$n-ph$p,")")
  })
  
  output$resul1<- renderText({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    #manera de entrar vector de medias--------------
    vectorMed <- input$vectorIng
    vectorMed <- as.numeric(unlist(strsplit(vectorMed, "[\n, \t]")))
    vectorMed <- vectorMed[!is.na(vectorMed)]
    
    ph <- To(base = dt[,vv], alpha=input$alfa, M0=vectorMed)
    conclusion <- ifelse(ph$estadistico > ph$valor, 'es RECHAZADA',
                         'NO ES RECHAZADA')
    paste0('El estadístico de prueba es T^2=', round(ph$estadistico, 2),
           ' y el valor en el percentil ' , round(input$alfa, 2)*100 , ' para la distribución F con p y n-p grados de libertad ' , round(ph$valor, 2), ', por esta razón
           se puede concluir que, dada la información de la muestra 
           la hipótesis nula ', conclusion, 
           ' a un nivel de significancia del ' , round((1-input$alfa)*100,1), "%")
  })
  
  output$qqplot <- renderPlot({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    
    require(car)
    dist<-mahalanobis(dt[,vv],center=colMeans(dt[,vv]),cov=var(dt[,vv]))
    qqPlot(dist, dist="chisq", df=length(dt[,vv]))
    
    grid()
  })

  
  output$royston <- renderPrint({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    # Para sacar los NA de la variable
    
    require(MVN)
      valorqq<-mvn(data= dt[,vv],mvnTest = "royston", multivariatePlot= "qq")
      valorqq$multivariateNormality
  })
  
  output$mardia <- renderPrint({
    vv <- c(input$names)
    inFile <- input$file1
    if(is.null(inFile)) 
      dt <- read.delim2('base.txt')
    else dt <- read.csv(inFile$datapath, header=input$header, 
                        sep=input$sep)
    # Para sacar los NA de la variable
    
    require(MVN)
    valorqq<-mvn(data= dt[,vv],mvnTest = "mardia", multivariatePlot= "qq")
    valorqq$multivariateNormality
  })
  

  })
