library(tidyverse)
library(ggplot2)
library(tseries)
library(lmtest)
library(knitr)
library(markdown)
library(rmarkdown)
library(googleVis)
library(plotly)
library(shiny)
library(DBI)
library(DT)
library("rio")




# UI
ui <- fluidPage(
  titlePanel("Projekt koncowy"),

 

    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Pobierz dane",
                           
                           br(),
                           
                           p("Pobierz dane ze strony GUS:"),
                           
                           actionButton(inputId = "dane1", 
                                        label = "Otworz polaczenie strony GUS",
                                        icon = icon("th"), 
                                        onclick ="window.open('https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/', '_blank')"),
                           
                           p("Po wejsciu na strone GUS pobierz dane tygodniowe umieralnosci w Polsce i umiesc je w folderze roboczym (w ktorym znajduje sie aplikacja) w nowym folderze data"),
                           
                           
                           p("Pobierz dane ze strony EUROSTAT:"),
                           
                           actionButton(inputId = "dane2", 
                                        label = "Otworz polaczenie strony EUROSTAT",
                                        icon = icon("th"), 
                                        onclick ="window.open('https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk', '_blank')"),
                           p("Po wejsciu na strone EUROSTAT pobierz dane umieralnosci w Europie i umiesc je w folderze roboczym (w ktorym znajduje sie aplikacja) w nowym folderze data")
                           ,
                           p("Nastepnie nacisnij ponizszy przycik w celu przetworzenia danych i poczekaj na powiadomienie o zakonczeniu przetwarzania danych:"),
                           actionButton(inputId = "dane", 
                                        label = "Przetworz dane"),
                           p("Po przetworzeniu danych przycisnij przycisk generujacy baze danych i poczekaj na powiadomienie o zakonczeniu tworzenia bazy:"),
                           actionButton(inputId = "baza", 
                                        label = "Wygeneruj baze danych")
                      ),
                  
                  tabPanel("SQL", textInput("Q", "Wpisz zapytanie:", "SELECT * FROM GUS LIMIT 10;"),
                           DT::dataTableOutput('tbl'), downloadButton("downloadData", "Pobierz tabele do formatu csv")),
                           
                  
                  tabPanel("Mapa GUS", column(6,htmlOutput("Geo")), column(6,htmlOutput("porown1")),
                                   selectInput("selectYear",
                                               label = "Rok danych",
                                               choices = as.vector(as.character(2021:2000),mode="list")),
                           selectInput("porown",
                                       label = "Rok danych do porownania",
                                       choices = as.vector(as.character(2021:2000),mode="list"))),
                  
                                               
                  tabPanel("Mapa EU",column(6,htmlOutput("eu")), column(6,htmlOutput("porown2")),
                                                        selectInput("selecteu",
                                                                    label = "Rok danych",
                                                                    choices = as.vector(as.character(2021:2000),mode="list")),
                           selectInput("porowneu",
                                       label = "Rok danych do porownania",
                                       choices = as.vector(as.character(2021:2000),mode="list"))),
                           
                           
                  tabPanel("Szeregi czasowe GUS", column(6, plotlyOutput("wykresg")), column(6, plotlyOutput("wykresg2")),
                           selectInput("wiek", label =  "Grupa wiekowa",
                                      choices = list("dla wszystkich grup", "0 - Inf", "00 - 04", "05 - 09", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39",
                                                  "	40 - 44", "45 - 49", "50 - 54", "	55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84",
                                                 "85 - 89", "90 - Inf")),
                           
                           selectInput("plec", label = "Plec",
                                       choices = list("Ogolem", "Kobiety", "Mezczyzni")),
                           
                            selectInput("woj", label = "Region",
                                                choices = list("Polska", "Dolnośląskie", "Kujawsko-Pomorskie", "Lubelskie", "Lubuskie", "Łódzkie", "Małopolskie", "Mazowieckie",
                                                       "Opolskie", "Podkarpackie", "Podlaskie", "Pomorskie", "Śląskie", "Świętokrzyskie", "Warmińsko-Mazurskie",
                                                       "Wielkopolskie", "Zachodniopomorskie")),
                           selectInput("szeregg",
                                       label = "Rok danych do porownania",
                                       choices = as.vector(as.character(2021:2000),mode="list"))),
                  
                           
                  tabPanel("Szeregi czasowe EUROSTAT", column(6, plotlyOutput("wykrese")), column(6, plotlyOutput("wykrese2")),
                                    
                                    selectInput("sex", label = "Plec",
                                                choices = list("Total", "Females", "Males")),
                                    
                                    selectInput("kraj", label = "Kraj",
                                                choices = list("Poland","Albania", "Andorra", "Armenia", "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
                                                               "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany",
                                                               "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta",
                                                               "Montenegro", "Netherlands", "Norway",  "Portugal", "Romania", "Serbia",
                                                               "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")),
                           selectInput("szerege",
                                       label = "Rok danych do porownania",
                                       choices = as.vector(as.character(2021:2000),mode="list"))),
                           
                  tabPanel("Raport HTML",
                                    p("Raport zostanie zapisany w folderze, w ktorym znajduje sie aplikacja."),
                                    actionButton(inputId = "Raport", 
                                                 label = "Wygeneruj raport"))
                           
  
      )
    )
  )



# Server
server <- function(input, output, session) { 
  
  observeEvent(input$dane,{  
    source("pobieranie.r")
    showNotification("Przetworzenie powiodlo sie")
    
    })
  
  
  observeEvent(input$baza,{ 
    
      output$tbl <- DT::renderDataTable({
      dataDir    <- file.path(getwd(),"data")
      dbName <- file.path(dataDir,"GUS_data.db")
      con <- dbConnect(RSQLite::SQLite(), dbName=dbName)
      try({
        dbWriteTable(con, "GUS", gus, overwrite = TRUE, row.names = FALSE)
        dbWriteTable(con, "EUROSTAT", euro, overwrite = TRUE, row.names = FALSE)
      })
      on.exit(dbDisconnect(con), add = TRUE)
      ret <- data.frame()
      query <- input$Q
      ret <- dbGetQuery(con, query)
      save(ret, file='tbl')
      return(ret)
      
    })
 
    showNotification("Baza zostala utworzona")
  })
  
 dataInsql <- reactive({
   try ({
  tabela <- get(load("tbl"))
  tabela <- as.data.frame(tabela)
  return(tabela)
  },silent=T)
  return(data.frame())
})
 

    
    output$downloadData <- downloadHandler(
    filename =  function() {
      paste("test.csv")
    }
    ,
    content = function(file) {
      
      write.csv(dataInsql, file, row.names = FALSE)
    }) 
      

  
  outVar <- reactiveValues(
    selectYearVar = "2021"
  )
  
  observeEvent(input$selectYear,{
    outVar$selectYearVar <- input$selectYear
  })
  
  outVar2 <- reactiveValues(
    selecteuVar = "2021"
  )
  
  observeEvent(input$selecteu,{
    outVar2$selecteuVar <- input$selecteu
  })
  
  outVar3 <- reactiveValues(
    wiekVar = ""
  )
  
  observeEvent(input$wiek,{
    outVar3$wiekVar <- input$wiek
  })
  
  outVar4 <- reactiveValues(
    plecVar = "Ogolem"
  )
  
  observeEvent(input$plec,{
    outVar4$plecVar <- input$plec
  })
  
  outVar5 <- reactiveValues(
    wojVar = "Polska"
  )
  
  observeEvent(input$woj,{
    outVar5$wojVar <- input$woj
  })
  
  outVar6 <- reactiveValues(
    sexVar = "Total"
  )
  
  observeEvent(input$sex,{
    outVar6$sexVar <- input$sex
  })
  
  outVar7 <- reactiveValues(
    krajVar = "Poland"
  )
  
  observeEvent(input$kraj,{
    outVar7$krajVar <- input$kraj
  })
  
  
  outVar8 <- reactiveValues(
    porownVar = "2021"
  )
  
  observeEvent(input$porown,{
    outVar8$porownVar <- input$porown
  })
  
  outVar9 <- reactiveValues(
    porowneuVar = "2021"
  )
  
  observeEvent(input$porowneu,{
    outVar9$porowneuVar <- input$porowneu
  })
  
  outVar10 <- reactiveValues(
    szereggVar = "2021"
  )
  
  observeEvent(input$szeregg,{
    outVar10$szereggVar <- input$szeregg
  })
  
  outVar11 <- reactiveValues(
    szeregeVar = "2021"
  )
  
  observeEvent(input$szerege,{
    outVar11$szeregeVar <- input$szerege
  })
  
  
  dataIn <- reactive({
    try({
      gus <- gus[(gus$Plec=='Ogolem'), ]
      gus <- gus[gus$Region_id %in% c("PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62") ,]
      gus <- aggregate(Liczba~rok+Region,data=gus,FUN="sum")
      gus <- gus[as.character(gus$rok)==as.character(outVar$selectYearVar),]

      return(gus)
    },silent=T)
    return(data.frame())
  })
  
  
  dataIn2 <- reactive({
    try({
      euro <- euro[(euro$SEX=='Total'), ]
      euro <- aggregate(Value~rok+GEO,data=euro,FUN="sum")
      euro <- euro[as.character(euro$rok)==as.character(outVar2$selecteuVar),]
      
      return(euro)
    },silent=T)
    return(data.frame())
  })
  
  dataIn3 <- reactive({
    try({
    
      if (outVar3$wiekVar=="dla wszystkich grup") {
        
        gus <- aggregate(Liczba~rok+Plec+Region,data=gus,FUN="sum")
        gus <- gus[as.character(gus$Plec) == as.character(outVar4$plecVar),]
        gus <- gus[as.character(gus$Region) == as.character(outVar5$wojVar),]
        return(gus)
        return(data.frame())
      } else {
      
      gus <- aggregate(Liczba~rok+Plec+Grupa_wiekowa+Region,data=gus,FUN="sum")
      gus <- gus[as.character(gus$Grupa_wiekowa) == as.character(outVar3$wiekVar),]
      gus <- gus[as.character(gus$Plec) == as.character(outVar4$plecVar),]
      gus <- gus[as.character(gus$Region) == as.character(outVar5$wojVar),]
      
      return(gus)
      return(data.frame())
      }
    })
    
  })

  dataIn4 <- reactive({
    try({
      euro <- aggregate(Value~rok+GEO+SEX,data=euro,FUN="sum")
      euro <- euro[as.character(euro$GEO)==as.character(outVar7$krajVar),]
      euro <- euro[as.character(euro$SEX)==as.character(outVar6$sexVar),]
      
      return(euro)
    },silent=T)
    return(data.frame())
  })
  
  
  dataIn5 <- reactive({
    try({
      gus <- gus[(gus$Plec=='Ogolem'), ]
      gus <- gus[gus$Region_id %in% c("PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62") ,]
      gus <- aggregate(Liczba~rok+Region,data=gus,FUN="sum")
      gus1 <- gus[as.character(gus$rok)==as.character(outVar$selectYearVar), ]
      gus2 <- gus[as.character(gus$rok)==as.character(outVar8$porownVar), ]
      dane <- merge(gus1, gus2, by="Region", all = T)
      dane <- na.omit(dane)
      dane$wzgledna_wartosc <- dane$Liczba.x/dane$Liczba.y
      
      
      return(dane)
    },silent=T)
    return(data.frame())
  })
  
  
  dataIn6 <- reactive({
    try({
      euro <- euro[(euro$SEX=='Total'), ]
      euro <- aggregate(Value~rok+GEO,data=euro,FUN="sum")
      euro1 <- euro[as.character(euro$rok)==as.character(outVar2$selecteuVar),]
      euro2 <- euro[as.character(euro$rok)==as.character(outVar9$porowneuVar),]
      daneeu <- merge(euro1, euro2, by="GEO", all = T)
      daneeu <- na.omit(daneeu)
      daneeu$wzgledna_wartosc <- daneeu$Value.x/daneeu$Value.y
      
      return(daneeu)
    },silent=T)
    return(data.frame())
  })
  
  dataIn7 <- reactive({
   
      
      if (outVar3$wiekVar=="dla wszystkich grup") {
        try({
        
     
   #     d3 <- d3 %>%
   #       group_by(rok, Plec, Region) %>%
   #       summarise(mean = mean(Liczba))
        d3 <- aggregate(Liczba~rok+Plec+Region,data=gus,FUN="sum")
        d3 <- d3[as.character(d3$rok) == as.character(outVar10$szereggVar),]
        d3 <- d3[as.character(d3$Plec) == as.character(outVar4$plecVar),]
        d3 <- d3[as.character(d3$Region) == as.character(outVar5$wojVar),]
  
        guss <- aggregate(Liczba~rok+Plec+Region,data=gus,FUN="sum")
        guss <- guss[as.character(guss$Plec) == as.character(outVar4$plecVar),]
        guss <- guss[as.character(guss$Region) == as.character(outVar5$wojVar),]
        guss$wartosc_wzgledna <- guss$Liczba/d3$Liczba
        return(guss)
        },silent=T)
        return(data.frame())
        
      } else {
        try({

        d5<- aggregate(Liczba~rok+Plec+Grupa_wiekowa+Region,data=gus,FUN="sum")
        d5 <- d5[as.character(d5$rok) == as.character(outVar10$szereggVar),]
        d5 <- d5[as.character(d5$Plec) == as.character(outVar4$plecVar),]
        d5 <- d5[as.character(d5$Region) == as.character(outVar5$wojVar),]
        d5 <- d5[as.character(d5$Grupa_wiekowa) == as.character(outVar3$wiekVar),]
   
        guss <- aggregate(Liczba~rok+Plec+Grupa_wiekowa+Region,data=gus,FUN="sum")
        guss <- guss[as.character(guss$Grupa_wiekowa) == as.character(outVar3$wiekVar),]
        guss <- guss[as.character(guss$Plec) == as.character(outVar4$plecVar),]
        guss <- guss[as.character(guss$Region) == as.character(outVar5$wojVar),]
        guss$wartosc_wzgledna <- guss$Liczba/d5$Liczba
        return(guss)
        },silent=T)
        return(data.frame())
      }
    })
    
  dataIn8 <- reactive({
    try({
      d6 <- aggregate(Value~rok+GEO+SEX,data=euro,FUN="sum")
      d6 <- d6[as.character(d6$rok)==as.character(outVar11$szeregeVar),]
      d6 <- d6[as.character(d6$GEO)==as.character(outVar7$krajVar),]
      d6 <- d6[as.character(d6$SEX)==as.character(outVar6$sexVar),]
      
      euroo <- aggregate(Value~rok+GEO+SEX,data=euro,FUN="sum")
      euroo <- euroo[as.character(euroo$GEO)==as.character(outVar7$krajVar),]
      euroo <- euroo[as.character(euroo$SEX)==as.character(outVar6$sexVar),]
      euroo$wartosc_wgledna <- euroo$Value/d6$Value
      
      return(euroo)
      
    },silent=T)
    return(data.frame())
  })
 
  
  output$Geo <- renderGvis({
    Geo <- gvisGeoChart(dataIn(),
                                         "Region",
                                         "Liczba", 
                                         options = list(region="PL", 
                                                        displayMode="regions",
                                                        resolution="provinces", as.is=T))
    save(Geo, file = 'Geo')
    return(Geo)
  })

  
  output$porown1 <- renderGvis({
    porown1 <- gvisGeoChart(dataIn5(),
                                         "Region",
                                         "wzgledna_wartosc", 
                                         options = list(region="PL", 
                                                        displayMode="regions",
                                                        resolution="provinces", as.is=T))
    
  save(porown1, file = 'porown1')
  return(porown1)
  })
  
  
  
  
  output$eu <- renderGvis({
    eu <- gvisGeoChart(dataIn2(),
                        locationvar="GEO",
                        colorvar="Value",
                        options=list(region='150'))
    
    save(eu, file = 'eu')
    return(eu)
    
  })
  
  output$porown2 <- renderGvis({
    porown2 <- gvisGeoChart(dataIn6(),
                                        locationvar="GEO",
                                        colorvar="wzgledna_wartosc",
                                        options=list(region='150'))
    
    save(porown2, file = 'porown2')
    return(porown2)
    
  })
  
  output$wykresg <- renderPlotly({ggplotly(
    wykresg <-  ggplot(dataIn3(), aes(x=rok, y=Liczba, group=1), show.legend=F) + 
        geom_line() + 
        labs(y="Umieralnosc", 
             x="rok") +
    ggtitle(paste0("Wykres umieralnosci dla wybranej plci ",outVar4$plecVar, " oraz wybranego regionu ", outVar5$wojVar, " oraz grupy wiekowej ", outVar3$wiekVar))
      )
    save(wykresg, file = 'wykresg')
    return(wykresg)
    
  })
  
  output$wykrese <- renderPlotly({ ggplotly(
      wykrese <-ggplot(dataIn4(), aes(x=rok, y=Value, group=1), show.legend=F) + 
        geom_line() + 
        labs(y="Umieralnosc", 
             x="rok") +
      ggtitle(paste0("Wykres umieralnosci dla wybranej plci ",outVar6$sexVar, " oraz wybranego kraju ", outVar7$krajVar))
       )
    save(wykrese, file = 'wykrese')
    return(wykrese)
    
  })
  
  
  output$wykresg2 <- renderPlotly({ggplotly(
    wykresg2 <-  ggplot(dataIn7(), aes(x=rok, y=wartosc_wzgledna, group=1), show.legend=F) + 
        geom_line() + 
        labs(y="Umieralnosc wzgledem wybranego roku", 
             x="rok") +
    ggtitle(paste0("Wykres umieralnosci wzgledem roku ", outVar10$szereggVar, " dla wybranej plci ",outVar4$plecVar, " oraz wybranego regionu ", outVar5$wojVar, " oraz grupy wiekowej ", outVar3$wiekVar))
     )
    save(wykresg2, file = 'wykresg2')
    return(wykresg2)
    
  })
  
  
  output$wykrese2 <- renderPlotly({ggplotly(
    wykrese2 <-  ggplot(dataIn8(), aes(x=rok, y=wartosc_wgledna, group=1), show.legend=F) + 
        geom_line() + 
        labs(y="Umieralnosc wzgledem wybranego roku", 
             x="rok") +
     ggtitle(paste0("Wykres umieralnosci wzgledem roku ", outVar11$szeregeVar," dla wybranej plci ",outVar6$sexVar, " oraz wybranego kraju ", outVar7$krajVar))
    )
    save(wykrese2, file = 'wykrese2')
    return(wykrese2)
    
  })
  
  
  czyraport <- reactive({
    input$Raport
  })
  
  observeEvent(input$Raport,{  
    if(czyraport()) {
     
      knit(input = "Raport.Rmd", output = "rap.md")
      
      markdownToHTML(file = "rap.md", output = "Raport_final.html")
    }
    
  })
  
} 

# aplikacja
app <- shinyApp(ui, server)
runApp(app)

