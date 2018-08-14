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


# Define server logic required to draw plots and texts

shinyServer(function(input, output) {
  

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
      
      Text_Tittle <- paste("Gráfico de Dispersión - Análisis de ", input$x, "vs.", input$y)    
      
      Text_Subtittle <- paste("Muestra de", nrow(Super_Heroes3),"Heroes con todo el espectro de Poderes")    
      
      if (input$power != 'None') 
        Text_Subtittle <- paste("Selección de", nrow(Super_Heroes3),"de", nrow(Super_Heroes2),"Heroes con el Poder de", Super_Poder)
      
      
            
    p <-    ggplot(Super_Heroes3, aes_string(x=input$x, y=input$y)) 

  if (input$power != 'None')
    p <-    p + geom_jitter(data = Super_Heroes2, mapping = aes_string(x = input$x, y = input$y), color ="grey", size = 3, alpha = 0.3)
  
    p <-    p + geom_jitter(height = 1, width = 1, size = 4, alpha = 0.5) 
                 
  if (input$color != 'None')
    p <-    p + aes_string(color=input$color)
    
      facets <- paste(input$facet_row, "~ .")
  
  if (facets != '. ~ .')
    p <-    p + facet_grid(facets)
    
  if (input$lm)
    p <-    p + geom_smooth(method='lm',formula=y~x, na.rm = T, se=TRUE,  color = "black", linetype = "dotdash", size = 0.3)
  
  if (input$smooth)
    p <-    p + geom_smooth(method='loess',formula=y~x, na.rm = T, se=TRUE,  color = "black", linetype = "dotdash", size = 0.3)
    
      
      p <- p +  theme_minimal(base_size = 12, base_family = "Georgia") +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        scale_color_brewer(type = "qual", palette = "Set1") +
        coord_cartesian(xlim = rangesg1$x, ylim = rangesg1$y, expand = TRUE) +
   #     scale_x_continuous(limits=c(0,valor_max_x)) +
    #    scale_y_continuous(limits=c(0,valor_max_y)) +
        ggtitle(Text_Tittle) +
        labs(subtitle= Text_Subtittle,
             caption="Nota: Seleccionando una región del gráfico y haciendo doble click es posible ampliar la imagen. 
             Para volver al estado original, simplemente hacer doble click de nuevo sin realizar ninguna selección en el gráfico") +
        theme(plot.title = element_text(size = 14, face = "bold"), 
              plot.subtitle = element_text(size = 12, face = "bold"),
              legend.position="bottom")
              
    
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
              colors=brewer.pal(7,"Set1"), ordered.color = F, scale=c(3,.5),rot.per=0.2,
              max.words=60, use.r.layout = TRUE, font = 2, fixed.asp = TRUE)
  
    p2a
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
    
    Text_Tittle3 <- paste("Gráfico de Mapa de Calor (Total_Power) - ",input$heat_row, "vs.", input$color)    
    
    Text_Subtittle3 <- paste("Muestra de", nrow(Super_Heroes3),"Heroes con todo el espectro de Poderes")    
    
    if (input$power != 'None') 
      Text_Subtittle3 <- paste("Selección de", nrow(Super_Heroes3),"de", nrow(Super_Heroes2),"Heroes con el Poder de", Super_Poder)
    
    
    
    p3 <- ggplot(Super_Heroes3, aes_string(x=input$color, y=input$heat_row)) +
      geom_tile(aes_string(fill = "Total_Poder"), colour = "white") +
      scale_fill_gradient(low = "#bdd7e7",
                          high = "#08519c",
                          name="Poder total:") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position="bottom",
            legend.key.height = unit(0.4, "cm"),
            legend.key.width = unit(2, "cm")) +
      ggtitle(Text_Tittle3) +
      labs(subtitle= Text_Subtittle3) +
#      theme_minimal(base_size = 12, base_family = "Georgia") +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "bold"))
     
    
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

        #  CREAMOS EL TITULO DEL GRAFICO  
        
        Text_Tittle4 <- paste("Gráfico de Análisis de Distribuciones (Alluvia)")    
        
        Text_Subtittle4 <- paste("Visión de distribuciones de ", input$v1," - ", input$v2, " - ", input$v3)         
        
            
# Pintamos el grafico
        
p4 <- ggplot(Super_Heroes_alluvia, aes_string(y= Super_Heroes_alluvia$cuenta, axis1 = input$v1, axis2 = input$v2, axis3 = input$v3)) +
      geom_alluvium(aes_string(fill = input$v3), alpha = 0.3) +
      #  guides(fill = FALSE) +
      geom_stratum(fill = "white", color = "grey", alpha = 0.9) +
      #  geom_label(stat = "stratum", label.strata = TRUE, label.size = 0, alpha =0.5) +
      geom_text(stat = "stratum", label.strata = TRUE, size = 3) +
      #  scale_x_discrete(limits = c(input$v3, "Gender", "Alignment", "Specie"), expand = c(.05, .05)) +
      scale_x_continuous(breaks = 1:3, labels = c(input$v1, input$v2, input$v3)) +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      scale_color_brewer(type = "qual", palette = "Set1") +
      ggtitle("Distribución de Super Heroes por Publisher, Género y Alineamiento") +
      xlab("Caracteristicas de los SuperHeroes") +
      ylab("Frecuencia / Nº SuperHéroes en cada población")+
      theme_minimal(base_size = 12, base_family = "Georgia") +
      ggtitle(Text_Tittle4) +
      labs(subtitle= Text_Subtittle4) +
      theme(plot.title = element_text(size = 14, face = "bold"), 
        plot.subtitle = element_text(size = 12, face = "bold"), 
        legend.position="bottom")
  
        
    print(p4)
  })
  
# Fin Gráfico 4
  
  
# Inicio Gráfico 5 - Distribución
  
  output$plotgraph5 <- renderPlot({
    
    
    #  CREAMOS EL TITULO DEL GRAFICO  
    
    Text_Tittle5 <- paste("Gráfico de Análisis de Distribuciones (Violines)")    
    
    Text_Subtittle5 <- paste("Visión de distribuciones de ", input$y2," por ", input$x2)         
    
    
        # Pintamos el grafico
        
        p5 <- ggplot(Super_Heroes2, aes_string(x=input$x2, y=input$y2, color = input$x2)) +
          geom_violin(aes_string(fill = input$x2), alpha = 0.3) + 
          geom_jitter(width=0.30, height=0.05, alpha=0.5, size=0.9)

        facets5 <- paste(input$facet_row2, "~ .")
        
        if (facets5 != '. ~ .')
          p5 <-    p5 + facet_grid(facets5)
        
          p5 <- p5 +   
        #    theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
            scale_fill_brewer(type = "div", palette = "Set1") +
            scale_color_brewer(type = "div", palette = "Set1") +
            ggtitle(Text_Tittle5) +
            labs(subtitle= Text_Subtittle5,
                 x= paste("Caracteristicas de los SuperHeroes -", input$x2),
                 y= paste("Valores para ", input$y2)) +
            theme_minimal(base_size = 12, base_family = "Georgia") +
            theme(plot.title = element_text(size = 14, face = "bold"), 
                  plot.subtitle = element_text(size = 12, face = "bold"), 
                  legend.position="bottom") +
            theme(axis.text.x = element_text(angle=65, vjust=0.6)) 
          
        print(p5)
  })
  
  # Fin Gráfico 5
  
  
  # Inicio Gráfico 6 - Analisis de super-poderes
  
  output$plotgraph6 <- renderPlot({
 
    
    #  CREAMOS EL TITULO DEL GRAFICO  
    
    Text_Tittle6 <- paste("Gráfico de Barras - Ranking de Super Poderes")    
    
    # Generamos el Data.Set necesario para componer el gráfico Alluvia
    Analisis_Poderes <- summarize(group_by(Super_Heroes_Powers2, Poder), cuenta = n(), Valor = max(Valor))
    
    if (input$powers_v1 == 'cuenta') {
            if (input$powers_v2 == 'Mayores') {
                Analisis_Poderes <- Analisis_Poderes[order(-Analisis_Poderes$cuenta), ]
                Analisis_Poderes$campo <- Analisis_Poderes$cuenta
                Text_Subtittle6 <- paste("Ranking de Super Poderes más habituales")
               } else {
                Analisis_Poderes <- Analisis_Poderes[order(Analisis_Poderes$cuenta), ]
                Analisis_Poderes$campo <- Analisis_Poderes$cuenta
                Text_Subtittle6 <- paste("Ranking de Super Poderes menos habituales")
               } 
    } else { 
            if (input$powers_v2 == 'Mayores') {
              Analisis_Poderes <- Analisis_Poderes[order(-Analisis_Poderes$Valor), ]
              Analisis_Poderes$campo <- Analisis_Poderes$Valor
              Text_Subtittle6 <- paste("Ranking de Super Poderes más poderosos")
              } else {
              Analisis_Poderes <- Analisis_Poderes[order(Analisis_Poderes$Valor), ]
              Analisis_Poderes$campo <- Analisis_Poderes$Valor
              Text_Subtittle6 <- paste("Ranking de Super Poderes menos poderosos")
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
              scale_fill_brewer(type = "qual", palette = "Set1") +
              scale_color_brewer(type = "qual", palette = "Set1") +
              ggtitle(Text_Tittle6) +
              labs(subtitle= Text_Subtittle6,
              x="Super Poderes",
              y=input$powers_v1) +
          theme_minimal(base_size = 12, base_family = "Georgia") +
          theme(plot.title = element_text(size = 14, face = "bold"), 
                plot.subtitle = element_text(size = 12, face = "bold"), 
                legend.position="bottom") +
          theme(axis.text.x = element_text(angle=65, vjust=0.6)) 
        
        print(p6)
  })
  
  # Fin Gráfico 6

# Inicio Texto 4 - Conclusiones Análisis Alluvia
  
  output$plottext4 <- renderUI({
    
    t4 <- HTML(paste(
      "", strong("Conclusiones Panel 1.1:"), "",
      "En mi opinión este tipo de gráficos son muy",
      "interesantes porque nos permiten ver mucho", 
      "de un solo vistazo. Por ejemplo, Marvel tiene",
      "casi el doble de super heroes que DC Comics.",
      "",
      "Algo similar pasa cuando analizamos el Genero",
      "de los personajes. Se nota que ha sido un",
      "terreno mayoritariamente masculino ya que las",
      "chicas no parecen superar 1/5 parte del elenco.",
      "",
      "Por otro lado, llama la atención que existan",
      "muchos más personajes buenos que malos, lo que", 
      "dificulta el buscado equilibrio entre bien y mal,",
      "y también es curioso la existencia de neutrales",
      "que, a pesar de ser pocos en número, presentan",
      "una característica interesante y es que se",
      "nutren de manera similar, en valor numérico,",
      "tanto de los comics de Marvel como de DC.",
      "Estos seres neutrales son en su mayoría hombres.",
      "",
      "Es interesante visualizar en el primer tramo la",
      "variable de Especies (el gráfico se complica)",
      "y observar su relación con el Editor en Tramo 3",
      "vemos que DC y Marvel aportan un número similar",
      "de Humanos (DC tiene un % alto de humanos) y",
      "de dioses. Sin embargo, casi el 100% de los",
      "mutantes nacen de Marvel, lo que puede ser un",
      "rasgo diferencial de la franquicia.",
      sep = '<br/>'))
    
    t4
    
  })
  
# Fin Texto4

# Inicio Texto 5 - Conclusiones Texto 5
  
  output$plottext5 <- renderUI({
    
    t5 <- HTML(paste(
      "", strong("Conclusiones Panel 1.2:"), "",
      "En este gráfico podemos analizar la distribución",
      "de diversos valores en base a las características", 
      "de los Super Héroes.",
      "",
      "Seleccionando la especie podemos observar que",
      "los Humanos y Mutantes tienen tendencia a",
      "acumular un número limitado de poderes (en torno",
      "a 5/10 poderes), mientras que otras especies,",
      "presentan gran variación y acumulan los valores",
      "más altos. Por ejemplo, si elegimos el Poder",
      "Total veremos que, como es lógico, los dioses", 
      "albergan los valores más altos",
      "",
      "Si analizamos el Total Poder por la variable",
      "Género, vemos que se mantiene el sesgo sexista",
      "visto en el Panel 1.1., al presentar las",
      "mujeres valores habituales de poder muy por",
      "debajo de sus compañeros masculinos. Si",
      "utilizamos facetas podemos ver que esto es",
      "más acusado en DC Comics, con diferencias",
      "mucho más acusadas entre hombres y mujeres",
      "",
      "Otro refuerzo a la idea del cliché sexista",
      "lo vemos al estudiar la distribución de pesos",
      "por género. Prácticamente todas las mujeres se",
      "mantienen en pesos bajos independientemente de",
      "su especie. En los hombres estas distribuciones",
      "son absolutamenete diferentes",
      sep = '<br/>'))
    
    print(t5)
    
  })
  
  # Fin Texto5

  # Inicio Texto 6 - Conclusiones Texto 6
  
  output$plottext6 <- renderUI({
    
    t6 <- HTML(paste(
      "", strong("Conclusiones Panel 1.3:"), "",
      "Analizamos ahora las caracteristicas de los Super",
      "Poderes:",
      "",
      em("Super Poderes más comunes:"),
      "Vemos que existen algunos poderes que son",
      "compartidos por un numero muy elevado de Super",
      "Héroes. Por ejemplo, tener Super Fuerza no",
      "parece diferencial, ya que más de 300 de tus",
      "rivales o compañeros también la tienen. Algo",
      "similar pasa con otros poderes *típicos*,",
      "como son la velocidad, agilidad o inteligencia",
      "que forman parte de los 10 Super Poderes",
      "compartidos por más de 100 Super Héroes", 
      "",
      em("Super Poderes más escasos:"),
      "De manera sencilla podemos comprobar que 12",
      "Super Poderes sólo son ejecutados por un",
      "único Héroe. Vemos tanto caracterísiticas",
      "poderosas (Omnipresencia o Poder del Fénix)",
      "como otras divertidas como el Omnitrix de",
      "Ben10 y el poder de manipular pelo.",
      "",
      em("Distribución de la cuantificación del poder:"),
      "Al elegir el valor como característica de",
      "análisis, vemos que sólo 9 Super Poderes",
      "superan los 20 puntos. Disponer de alguno",
      "de ellos, como la Omnipotencia, Fuerza de Odin",
      "o Poder Cósmico da una importante ventaja",
      "competitiva.",
      sep = '<br/>'))
    
    print(t6)
    
  })
  
  # Fin Texto6

  # Inicio Texto 1 - Conclusiones Texto 1
  
  output$plottext1 <- renderUI({
    
    t1 <- HTML(paste(
      "", strong("Conclusiones Panel 2.1:"), "",
      "Aunque el gráfico está diseñado principalmente",
      "para visualizar la dispersión de variables", 
      "cuantitativas para un determinado Super Poder,",
      "el filtro original nos permite ver que existe",
      "una relación esperada entre el número de",
      "poderes de un individuo y su Poder Total,",
      "de hecho esta relación no parece del todo",
      "lineal sino que se observa cierto crecimiento",
      "exponencial aunque muy influenciado por los",
      "valores altos (en poder de los neutrales",
      "y de los dioses, lo que podemos comprobar",
      "modificando el color del gráfico usando",
      "la variable Especie).",
      "",
      "Si analizamos en el eje de las x la altura", 
      "y hacemos zoom para eliminar del grafico los",
      "valores atípicos, podemos ver que no parece",
      "que los dibujantes de comics relacionen el",
      "poder de un héroe en base a lo que mida. Algo",
      "similar pasa si seleccionamos el peso, aunque",
      "en este caso si parece existir cierta",
      "tendencia. Si utilizamos la variable Genero",
      "como color, vemos que el sesgo realmente",
      "viene dado por la diferencia de poder y",
      "peso entre hombres y mujeres analizado en",
      "paneles anteriores.",
      sep = '<br/>'))
    
    print(t1)
    
  })
  
  # Fin Texto1
  
  # Inicio Texto 2 - Conclusiones Texto 2
  
  output$plottext2 <- renderUI({
    
    t2 <- HTML(paste(
      "", strong("Conclusiones Panel 2.2:"), "",
      "Lo primero que nos llama la atención cuando",
      "visualizamos la nube de palabras sin ningún", 
      "Super Poder seleccionado, es la existencia de",
      "un Super Héroe que destaca sobre todos los",
      "demás y que yo, particularmente, desconocía.",
      "Se trata de One-Avobe-All y su nombre parece",
      "adecuado si es el Super Héroe más poderoso",
      "",
      "Si utilizamos el filtro de Super Poder para",
      "analizar los Super Poderes más valiosos",
      "según el Ranking del Panel 1.3, también veo",
      "cosas curiosas que desconocía. Por ejemplo,", 
      "si filtramos por el poder de la Omnipotencia",
      "vemos 4 nombres que no había visto antes:",
      "a.) Tres entidades cósmicas como One-Avobe-All,",
      "Living Tribunal y Beyonder. Estos seres, en",
      "principio neutrales, parecen puestos por los",
      "dibujantes de Comic como un elemento que",
      "puede mantener el equilibrio bien/mal según",
      "se necesite... parece que después de todo",
      "la idea del Dios omnipotente tradicional",
      "no está desarraigada de éste colectivo.",
      "b.) Un humano/mutante como Franklin Richards",
      "del que no sabía nada hasta ahora y que",
      "es el hijo de dos de los 4 fantásticos con",
      "uno de los mayores poderes de todos.",
      sep = '<br/>'))
    
    print(t2)
    
  })
  
  # Fin Texto2
  
  
  # Inicio Texto 3 - Conclusiones Texto 3
  
  output$plottext3 <- renderUI({
    
    t3 <- HTML(paste(
      "", strong("Conclusiones Panel 2.3:"), "",
      "El mapa de calor nos permite identificar las",
      "mayores concentraciones de poder en función", 
      "de las caracterísiticas de los Heroes según",
      "los silos en los que se clasifican.",
      "",
      "De esta forma, de un vistazo rápido podemos",
      "ver que no existen máquinas neutrales (se",
      "crean con un propósito) y que el mayor poder",
      "se encuentra en los dioses neutrales).",
      "",
      "El mapa de calor también nos sirve para",
      "hacer otros análisis interesantes, como por", 
      "ejemplo la distribución de poder entre",
      "raza y color de hojos o color de pelo",
      "",
      "Por ejemplo, seleccionando el color del",
      "cabello en filas y el Editor en columnas",
      "observamos que DC situa a los héroes más",
      "poderosos en tonos de cabello Plata o",
      "naranja. En el caso de Marvel es la luz",
      "la que destaca, posiblemente influído",
      "por One-Above-All. Es curioso ver que",
      "para ambas casas hay una relevante",
      "cantidad de poder acumulado en Héroes",
      "que no tienen pelo.",
      sep = '<br/>'))
    
    print(t3)
    
  })
  
  # Fin Texto3
  
  
  
  
    
})