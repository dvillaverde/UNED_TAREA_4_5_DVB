#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# 1. INSTALAMOS LOS PAQUETES Y ACTIVAMOS LAS LIBRERIAS NECESARIAS

# 1.1. Instalamos paquetes
if(!is.element("vcd", installed.packages()[, 1])) install.packages("vcd", dependencies = TRUE)
if(!is.element("wordcloud", installed.packages()[, 1])) install.packages("wordcloud", dependencies = TRUE)
if(!is.element("formattable", installed.packages()[, 1])) install.packages("formattable", dependencies = TRUE)
if(!is.element("RColorBrewer", installed.packages()[, 1])) install.packages("RColorBrewer", dependencies = TRUE)
if(!is.element("corpus", installed.packages()[, 1])) install.packages("corpus", dependencies = TRUE)
if(!is.element("devtools", installed.packages()[, 1])) require(devtools)
if(!is.element("wordcloud2", installed.packages()[, 1])) install_github("lchiffon/wordcloud2", dependencies = TRUE)
if(!is.element("alluvial", installed.packages()[, 1])) install.packages("alluvial", dependencies = TRUE)
if(!is.element("ggalluvial", installed.packages()[, 1])) install.packages("ggalluvial", dependencies = TRUE)

#1.2. Activamos las librerias
library(shiny)
library(vcd)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(data.table)
library(corpus)
library(readr)
library(knitr)
library(tidyr)
library(dplyr)
library(formattable)
library(alluvial)
library(ggalluvial)

#2. CARGAMOS LOS DATOS
# Cargamos los datos desde el repositorio github creado especialmente para el módulo 
Super_Heroes_Database2 <-read.csv("https://github.com/dvillaverde/UNED_TAREA_4_5_DVB/blob/Data/Super_Heroes_Database.csv?raw=true",header=T, dec = ",", sep= ";")
Super_Heroes_Powers2 <-read.csv("https://github.com/dvillaverde/UNED_TAREA_4_5_DVB/blob/Data/Super_Heroes_Powers.csv?raw=true",header=T, dec = ",", sep= ";")


#3. CREAMOS LOS DATA.FRAMES QUE NOS INTERESA UTILIZAR
# Nos quedamos solo con los Super Heroes de Marvel Comics y DC Comics
Super_Heroes2 <- rbind(subset(Super_Heroes_Database2, Publisher == "DC Comics"),subset(Super_Heroes_Database2, Publisher == "Marvel Comics"))

# Limpiamos un poco la informacion
#TRATAMOS LOS DATOS PARA PASAR LOS INDEFINIDOS A NAs
Super_Heroes2$Weight[Super_Heroes2$Weight == "-99"] <- NA
Super_Heroes2$Height[Super_Heroes2$Height == "-99"] <- NA
Super_Heroes2$Gender[Super_Heroes2$Gender == "-"] <- NA
Super_Heroes2$Eye.color[Super_Heroes2$Eye.color == "-"] <- NA
Super_Heroes2$Specie[Super_Heroes2$Specie == ""] <- NA
Super_Heroes2$Race[Super_Heroes2$Race == "-"] <- NA
Super_Heroes2$Hair.color[Super_Heroes2$Hair.color == "-"] <- NA
Super_Heroes2$Skin.color[Super_Heroes2$Skin.color == "-"] <- NA
Super_Heroes2$Alignment[Super_Heroes2$Alignment == "-"] <- NA
Super_Heroes2$name <- gsub(" ", "_", Super_Heroes2$name, fixed = TRUE)
Super_Heroes_Powers2$name <- gsub(" ", "_", Super_Heroes_Powers2$name, fixed = TRUE)
Super_Heroes2$name <- as.factor(Super_Heroes2$name)
Super_Heroes_Powers2$name <- as.factor(Super_Heroes_Powers2$name)

#ELIMINAMOS LA COLUMNA DE ID
Super_Heroes2 <- Super_Heroes2[,-1]
Super_Heroes3 <- Super_Heroes2
#Text_Tittle <- "Hola"

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
  
# Text_Tittle <- paste(input$x, "vs.", input$y,"- Selección de", nrow(Super_Heroes3),"de", nrow(Super_Heroes2),"Heroes con el Poder de", Super_Poder)

# Creamos el título del gráfico
#Text_Tittle <- paste(input$x, "vs.", input$y)
  

# Inicio Gráfico 1 - Dispersión de variables con Zoom  
    
# Primero definimos rangesg1 (está fuera de la función al ser reactiva)  
rangesg1 <- reactiveValues(x = NULL, y = NULL)
  
output$plotgraph1 <- renderPlot({
  
  Variablex <- input$x  
  Variabley <- input$y

# CAMBIAMOS LOS DATOS DE LA TABLA AUXILIAR 3 PARA QUE FILTRE POR EL PODER ELEGIDO   
   
    Super_Poder <- 'Total_Poder'
    
    if (input$power != 'None')  
         Super_Poder <- input$power
  
    Num_Columna <- which( colnames(Super_Heroes2)==Super_Poder )
    Super_Heroes3 <- Super_Heroes2
    Super_Heroes3$Poder_Selected <- Super_Heroes2[ , Num_Columna]
    Super_Heroes3 <- subset(Super_Heroes3, Poder_Selected > 0)
  
    # SELECCIONAMOS LOS VALORES MAXIMOS PARA LAS GRAFICAS
      
      valor_max_x <- max(Super_Heroes3[[Variablex]], na.rm = TRUE)*1.1  
      valor_max_y <- max(Super_Heroes3[[Variabley]], na.rm = TRUE)*1.1 

    #  CREAMOS EL TITULO DEL GRAFICO  
      
      Text_Tittle <- paste(input$x, "vs.", input$y, "- Muestra de", nrow(Super_Heroes3),"Heroes con todo el espectro de Poderes")    
      
      if (input$power != 'None') 
        Text_Tittle <- paste(input$x, "vs.", input$y,"- Selección de", nrow(Super_Heroes3),"de", nrow(Super_Heroes2),"Heroes con el Poder de", Super_Poder)
      
      
            
    p <-    ggplot(Super_Heroes3, aes_string(x=input$x, y=input$y)) 

  if (input$power != 'None')
    p <-    p + geom_jitter(data = Super_Heroes2, mapping = aes_string(x = input$x, y = input$y), color ="grey", size = 3, alpha = 0.3)
  
    p <-    p + geom_jitter(height = 1, width = 1, size = 4, alpha = 0.8) 
                 
  if (input$color != 'None')
    p <-    p + aes_string(color=input$color)
    
      facets <- paste(input$facet_row, "~ .")
  
  if (facets != '. ~ .')
    p <-    p + facet_grid(facets)
    
  if (input$lm)
    p <-    p + geom_smooth(method='lm',formula=y~x, na.rm = T, se=TRUE,  color = "black", linetype = "dotdash", size = 0.3)
  
  if (input$smooth)
    p <-    p + geom_smooth(method='loess',formula=y~x, na.rm = T, se=FALSE,  color = "black", linetype = "dotdash", size = 0.3)
    
      
      p <- p +  theme_minimal(base_size = 10, base_family = "Georgia") +
        coord_cartesian(xlim = rangesg1$x, ylim = rangesg1$y, expand = TRUE) +
   #     scale_x_continuous(limits=c(0,valor_max_x)) +
    #    scale_y_continuous(limits=c(0,valor_max_y)) +
        ggtitle(Text_Tittle) +
        theme(plot.title = element_text(size = 14, face = "bold"), legend.position="bottom") +
        scale_color_brewer(palette = "Set2")   
    
    #  print(p)
    
      p
      
  })

# Cuando hacemos doble click en el grafico queremos que cambie los límites del mismo
# o que vuelva al original en caso de no haber una selección
observeEvent(input$plotgraph1_dblclick, {
  brush <- input$plotgraph1_brush
  if (!is.null(brush)) {
    rangesg1$x <- c(brush$xmin, brush$xmax)
    rangesg1$y <- c(brush$ymin, brush$ymax)
    
  } else {
    rangesg1$x <- NULL
    rangesg1$y <- NULL
  }
})


# Fin Gráfico 1

  
  # GRAFICO 2
  
  output$plotgraph2 <- renderPlot({
    
    # CAMBIAMOS LOS DATOS DE LA TABLA AUXILIAR 3 PARA QUE FILTRE POR EL PODER ELEGIDO   
    
    Super_Heroes_names <- Super_Heroes2[, c(1, 180)]
    
    if (input$power != 'None')  
      Super_Heroes_names <- subset(Super_Heroes_Powers2, Poder == input$power)[,c(2,5)]
    
    p2a <- wordcloud(Super_Heroes_names$name, Super_Heroes_names$Total_Poder, min.freq = 1, random.order=FALSE, 
              colors=brewer.pal(8,"Dark2"), ordered.color = FALSE, scale=c(3,.5),rot.per=0.2,
              max.words=60, use.r.layout = TRUE, font = 2, fixed.asp = TRUE)
  
    p2b <-  wordcloud2(Super_Heroes_names, size = 0.3, minSize = 1, gridSize =  5,
               fontFamily = 'Segoe UI', fontWeight = 'bold',
               color = 'random-dark', backgroundColor = "white",
               #minRotation = -pi/8, maxRotation = pi/8, 
               shuffle = FALSE,
               rotateRatio = 0.4, shape = 'pentagon', ellipticity = 0.5,
               widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
 
    print(p2a)
  })
  
  
  # GRAFICO 3
  
  output$plotgraph3 <- renderPlot({
    

    # CAMBIAMOS LOS DATOS DE LA TABLA AUXILIAR 3 PARA QUE FILTRE POR EL PODER ELEGIDO   
    
    Super_Poder <- 'Total_Poder'
    
    if (input$power != 'None')  
      Super_Poder <- input$power
    
    Num_Columna <- which( colnames(Super_Heroes2)==Super_Poder )
    Super_Heroes3 <- Super_Heroes2
    Super_Heroes3$Poder_Selected <- Super_Heroes2[ , Num_Columna]
    Super_Heroes3 <- subset(Super_Heroes3, Poder_Selected > 0)
    
    #  CREAMOS EL TITULO DEL GRAFICO  
    
    Text_Tittle3 <- paste("Total Power Distribution - ",input$heat_row, "vs.", input$color, "- Muestra de", nrow(Super_Heroes3),"Heroes con todo el espectro de Poderes")    
    
    if (input$power != 'None') 
      Text_Tittle3 <- paste("Total Power Distribution - ",input$heat_row, "vs.", input$color,"- Selección de", nrow(Super_Heroes3),"de", nrow(Super_Heroes2),"Heroes con el Poder de", Super_Poder)
    
    
    
     p3 <- ggplot(Super_Heroes3, aes_string(x=input$color, y=input$heat_row)) +
      geom_tile(aes_string(fill = "Total_Poder"), colour = "white") +
      scale_fill_gradient(low = "#fee5d9",
                          high = "#a50f15",
                          name="Poder total:") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position="bottom",
            legend.key.height = unit(0.4, "cm"),
            legend.key.width = unit(2, "cm")) +
             ggtitle(Text_Tittle3)
    
    print(p3)
  })
  
  
# Inicio Gráfico 4 - Grafico Alluvia
  
  output$plotgraph4 <- renderPlot({
    
# Generamos el Data.Set necesario para componer el gráfico Alluvia
    Super_Heroes_alluvia <- summarize(group_by(Super_Heroes2, Publisher, Gender, Alignment, Specie), cuenta = n())

    if (input$v1 != 'Specie')  
      if (input$v2 != 'Specie')  
        if (input$v3 != 'Specie')  
          Super_Heroes_alluvia <- summarize(group_by(Super_Heroes2, Publisher, Gender, Alignment), cuenta = n())
    
# Pintamos el grafico
        
p4 <- ggplot(Super_Heroes_alluvia, aes_string(y= Super_Heroes_alluvia$cuenta, axis1 = input$v1, axis2 = input$v2, axis3 = input$v3)) +
      geom_alluvium(aes_string(fill = input$v3), alpha = 0.3) +
      #  guides(fill = FALSE) +
      geom_stratum(fill = "white", color = "grey", alpha = 0.9) +
      #  geom_label(stat = "stratum", label.strata = TRUE, label.size = 0, alpha =0.5) +
      geom_text(stat = "stratum", label.strata = TRUE, size = 3) +
      #  scale_x_discrete(limits = c(input$v3, "Gender", "Alignment", "Specie"), expand = c(.05, .05)) +
      scale_x_continuous(breaks = 1:3, labels = c(input$v1, input$v2, input$v3)) +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      ggtitle("Distribución de Super Heroes por Publisher, Género y Alineamiento") +
      xlab("Caracteristicas de los SuperHeroes") +
      ylab("Frecuencia / Nº SuperHéroes en cada población")+
      theme_minimal(base_size = 12, base_family = "Georgia") +
      theme(plot.title = element_text(size = 14, face = "bold"), legend.position="bottom")
    
    print(p4)
  })
  
# Fin Gráfico 4
  
  
# Inicio Gráfico 5 - Histogramas
  
  output$plotgraph5 <- renderPlot({
    
    # Generamos el Data.Set necesario para componer el gráfico Alluvia
    Super_Heroes_alluvia <- summarize(group_by(Super_Heroes2, Publisher, Gender, Alignment, Specie), cuenta = n())
    
    if (input$v1 != 'Specie')  
      if (input$v2 != 'Specie')  
        if (input$v3 != 'Specie')  
          Super_Heroes_alluvia <- summarize(group_by(Super_Heroes2, Publisher, Gender, Alignment), cuenta = n())
        
        # Pintamos el grafico
        
        p5 <- ggplot(Super_Heroes_alluvia, aes_string(y= Super_Heroes_alluvia$cuenta, axis1 = input$v1, axis2 = input$v2, axis3 = input$v3)) +
          geom_alluvium(aes_string(fill = input$v3), alpha = 0.3) +
          #  guides(fill = FALSE) +
          geom_stratum(fill = "white", color = "grey", alpha = 0.9) +
          #  geom_label(stat = "stratum", label.strata = TRUE, label.size = 0, alpha =0.5) +
          geom_text(stat = "stratum", label.strata = TRUE, size = 3) +
          #  scale_x_discrete(limits = c(input$v3, "Gender", "Alignment", "Specie"), expand = c(.05, .05)) +
          scale_x_continuous(breaks = 1:3, labels = c(input$v1, input$v2, input$v3)) +
          scale_fill_brewer(type = "qual", palette = "Set2") +
          ggtitle("Distribución de Super Heroes por Publisher, Género y Alineamiento") +
          xlab("Caracteristicas de los SuperHeroes") +
          ylab("Frecuencia / Nº SuperHéroes en cada población")+
          theme_minimal(base_size = 12, base_family = "Georgia") +
          theme(plot.title = element_text(size = 14, face = "bold"), legend.position="bottom")
        
        print(p5)
  })
  
  # Fin Gráfico 5
  
  
  # Inicio Gráfico 6 - Analisis de super-poderes
  
  output$plotgraph6 <- renderPlot({
    
    # Generamos el Data.Set necesario para componer el gráfico Alluvia
    Analisis_Poderes <- summarize(group_by(Super_Heroes_Powers2, Poder), cuenta = n(), Valor = max(Valor))
    
    if (input$powers_v1 == 'cuenta') {
            if (input$powers_v2 == 'Mayores') {
                Analisis_Poderes <- Analisis_Poderes[order(-Analisis_Poderes$cuenta), ]
                Analisis_Poderes$campo <- Analisis_Poderes$cuenta
               } else {
                Analisis_Poderes <- Analisis_Poderes[order(Analisis_Poderes$cuenta), ]
                Analisis_Poderes$campo <- Analisis_Poderes$cuenta
               } 
    } else { 
            if (input$powers_v2 == 'Mayores') {
              Analisis_Poderes <- Analisis_Poderes[order(-Analisis_Poderes$Valor), ]
              Analisis_Poderes$campo <- Analisis_Poderes$Valor
              } else {
              Analisis_Poderes <- Analisis_Poderes[order(Analisis_Poderes$Valor), ]
              Analisis_Poderes$campo <- Analisis_Poderes$Valor
       } 
    }
    
    Analisis_Poderes <- head(Analisis_Poderes, input$powers_v3)
            
        # Pintamos el grafico
        
        p6 <- ggplot(Analisis_Poderes, aes_string(x=reorder(Analisis_Poderes$Poder, -Analisis_Poderes$campo), y=input$powers_v1)) + 
              geom_bar(stat="identity", width=.8, fill= "grey", alpha =0.7) + 
              geom_point(col = "tomato2", size=3) + 
              geom_segment(aes_string(x=Analisis_Poderes$Poder, 
                           xend=Analisis_Poderes$Poder, 
                           y=0, 
                           yend=input$powers_v1)) + 
              labs(title="Distribución de Super Poderes", 
                    subtitle="Make Vs Avg. Mileage", 
                    caption="source: mpg") + 
              xlab("Super Poderes") +
              ylab("Nº total de Heroes") +
              theme_minimal(base_size = 12, base_family = "Georgia") +
              theme(plot.title = element_text(size = 14, face = "bold"), legend.position="bottom") +
              theme(axis.text.x = element_text(angle=65, vjust=0.6))
        
        print(p6)
  })
  
  # Fin Gráfico 6

# Inicio Texto 4 - Conclusiones Análisis Alluvia
  
  output$plottext4 <- renderUI({
    
    t4 <- HTML(paste(
      "", "Conclusiones:", "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      sep = '<br/>'))
    
    print(t4)
    
  })
  
# Fin Texto4

# Inicio Texto 5 - Conclusiones Texto 5
  
  output$plottext5 <- renderUI({
    
    t5 <- HTML(paste(
      "", "Conclusiones:", "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      sep = '<br/>'))
    
    print(t5)
    
  })
  
  # Fin Texto5

  # Inicio Texto 6 - Conclusiones Texto 6
  
  output$plottext6 <- renderUI({
    
    t6 <- HTML(paste(
      "", "Conclusiones:", "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      sep = '<br/>'))
    
    print(t6)
    
  })
  
  # Fin Texto6

  # Inicio Texto 1 - Conclusiones Texto 1
  
  output$plottext1 <- renderUI({
    
    t1 <- HTML(paste(
      "", "Conclusiones:", "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      sep = '<br/>'))
    
    print(t1)
    
  })
  
  # Fin Texto1
  
  # Inicio Texto 2 - Conclusiones Texto 2
  
  output$plottext2 <- renderUI({
    
    t2 <- HTML(paste(
      "", "Conclusiones:", "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      sep = '<br/>'))
    
    print(t2)
    
  })
  
  # Fin Texto2
  
  
  # Inicio Texto 3 - Conclusiones Texto 3
  
  output$plottext3 <- renderUI({
    
    t3 <- HTML(paste(
      "", "Conclusiones:", "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      "",
      "Linea 1",
      "Linea 2", 
      "Linea 3",
      "Linea 4",
      "Linea 5",
      "Linea 6",
      "Linea 7",
      "Linea 8",
      "Linea 9",
      sep = '<br/>'))
    
    print(t3)
    
  })
  
  # Fin Texto3
  
  
  
  
    
})