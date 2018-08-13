#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

#4. CREAMOS LAS VARIABLES PARA LA SELECCION
# distinguimos variables "a nivel de intervalo" ("continuas" para ggplot)
nums <- sapply(Super_Heroes2[,-(12:178)], is.numeric)
v_continuas <- names(Super_Heroes2[,-(12:178)])[nums]

# las variables "categóricas" ("discretas" para ggplot)
cats <- sapply(Super_Heroes2[,1:11], is.factor)
v_categoricas <- names(Super_Heroes2[,1:11])[cats]

# las variables "categóricas" ("discretas" para ggplot)
v_poderes <- names(Super_Heroes2[, 12:178])




shinyUI(

    navbarPage("UNED - TEMA 4.5 - Ejercicio Visualizacion Avanzada Shiny",
               tabPanel("Panel 0 - Introduccion y descripción del trabajo realizado",
                      mainPanel(
                        h3("UNED - MASTER EN BIG DATA & BUSINESS ANALYTICS", align = "center"),
                        h3("MODULO 4.5. - Visualización Avanzada", align = "center"),
                        h4("ALUMNO: David Villaverde Benito / FECHA: 14.08.2018", align = "center"),
                        hr(""),
                        h4("1 Introducción al Ejercicio:"),
                        p(""),
                        p("Como ejercicio final del Módulo 4.5. Visualización Avanzada, el profesor, Pedro Concejero,
                           nos dió la oportunidad de elegir entre diversas opciones. Siguiendo su recomendación
                           he decidido lanzarme al desarrollo y publicación de una aplicación interactiva con R-Shiny tratando
                           de aplicar las enseñanzas que nos ha dado durante el curso y completándola con el extenso contenido
                           existente en internet y otras fuentes"),
                        hr(""),
                        h4("2. Datos utilizados:"),
                        p("                                                                  "),
                        p("Quizá uno de los aspectos más complicados que encontré, al menos iniciamente, en el desarrollo del ejercicio
                           ha sido la elección del contenido y del juego de datos a utilizar. Finalmente, casi por accidente,
                           encontré un ejercicio interesante de catalogación de todos los super héroes y sus características.
                           Está claro que el tema no es serio y está enfocado a fans de este tipo de ocio, pero dado
                           que es un tema que me gusta y que, gracias a mis hijos, es de mucho interés en mi familia, 
                           decidí aprovechar esta información para tratar de hacer un análisis interesante y una representación
                           visual entretenida"),
                        p("La información original es de fácil acceso en Kaggle a través del siguiente enlace"),
                        a(href="", "https://www.kaggle.com/claudiodavi/superhero-set"),
                        p("Adicionalmente a este juego de datos, existe una página web - https://www.superherodb.com/powers/ - donde se ha cuantificado
                          el valor de cada uno de los super poderes descritos. Con un pequeño proceso manual es sencillo de convertir
                          en un data.set utilizable para nuestro análisis (en la siguiente web):"),
                        p("En cualquier caso, y dado que se trata de un número razonable de registros, he realizado mi propio
                           proceso de revisión y depuración de los datos utilizando una hoja de cálculo. Los datos resultantes
                          - y que son los utilizados en la aplicación Shiny generada - están colgados en mi página de GitHub:"),
                        a(href="", "https://github.com/dvillaverde/UNED_TAREA_4_5_DVB/tree/Data"),
                        p("El contenido subido (y que tambén se entrega dentro del fichero zip) se encuentra dividido en dos ficheros:"),
                        p("a. Super_Heroes_Database.csv"),
                        p("b. Super_Heroes_Powers.csv"),
                        hr(""),
                        h4("3. Estructura del ejercicio y de la aplicación Shiny:"),
                        p("                                                                  "),
                        p("La aplicación se divide en tres paneles principales:"),
                        p(strong("a.) Panel 0 - Introduccion y descripción del trabajo realizado")),
                        p("Es el panel en el que nos encontramos. Pretende ser una introducción al ejercicio y explicar los
                           datos utilizados, la estructura de la aplicación y la información aportada para la evaluación
                           del módulo cursado"),
                        p("Aunque en la propuesta de esqueleto que nos proporcionó el profesor como ejemplo se proponía utilizar
                           esta zona como repositorios de las concluisiones alcanzadas durante el análisis, dada la extensa información
                          analizable, he preferido habilitar una zona al lado de cada uno de los gráficos - tal y como se describirá a 
                          continuación - de las diferentes pestañas. Es en estos espacios de texto donde, bajo el epígrafe de
                          *Conclusiones*, he redactado mis análisis y aspectos relevantes identificados"),
                        p(strong("b.) Panel 1 - Analisis de las caracteristicas de la Muestra")),
                        p("Se trata de un panel eminentemente descriptivo de la información contenida en el fichero.
                           Consta de un área en la zona izquierda que nos permite seleccionar variables para interactuar
                           con los gráficos generados en la zona de la derecha. Ésta, a su vez se encuentra dividida en tres 
                           pestañas diferentes que nos permiten analizar diferentes tipos de información:"),
                        p(em("Panel 1.1. Análisis Alluvia: "), "Gráfico mediante el cual podemos observar la distribución de
                          los registros en función de variables categóricas sencillas como el Género, el Editor, su propensión al bien
                          o al mal y su especie"),
                        p(em("Panel 1.2. Análisis de Distribución: "), "xxxx"),
                        p(em("Panel 1.3. Información sobre Super Poderes: "), "xxxx"),                        
                        p(strong("c.) Panel 2. - Analisis de Super Heroes y Super Poderes")),
                        p("En este panel se pretende poder trabajar con la información del data set y poder analizar la
                           información disponible con la posibilidad de hacer filtros por un detreminado super-poder.
                          Al igual que en la pestaña anterior, dispondremos de una zona a la izquierda para seleccionar los
                          valores que utilizaremos en los diferentes paneles de la derecha, divididos en:"),
                        p(em("Panel 2.1. Dispersión de valores: "), "xxx"),
                        p(em("Panel 2.2. Super Héroes más poderosos: "), "xxxx"),
                        p(em("Panel 2.3. Distribución mediante mapa de calor: "), "xxxx"),     
                        hr(""),
                        h4("4. Componentes / Información entregada:"),
                        p("                                                                  "),
                        h5("1. El código R (ui.R / server.R) -por separado o en un .zip"),
                        h5("2. La URL o dirección de shinyapps.io a la que se ha subido"),
                        h5("IMPORTANTE: Si usáis vuestros datos deben estar incluidos o accesibles en internet"),
                        hr(""),
                        p(strong("Firmado:", "David Villaverde Benito"))
                        )),
             
             tabPanel("Panel 1 - Analisis de las caracteristicas de la Muestra",
                        sidebarPanel( 
                                    selectInput('v1', 'Elige variable 1', v_categoricas, 'Publisher'),
                                    selectInput('v2', 'Elige variable 2', v_categoricas, 'Gender'),
                                    selectInput('v3', 'Elige variable 3', v_categoricas, 'Alignment'),
                                    selectInput('color2', 'Analisis de Detalle / Color', c(None='.', v_categoricas), 'Alignment'),
                                    selectInput('power2', 'Elige Super Poder', c(All_Powers ='None', v_poderes), 'All_Powers'),
                        
                                    radioButtons('powers_v1', 'Elige variable para analisis de Super Poderes', c("cuenta", "Valor"), 'cuenta', inline = T),
                                    radioButtons('powers_v2', 'Elige el orden para analisis de Super Poderes', c("Mayores", "Menores"), 'Mayores', inline = T),
                                    sliderInput('powers_v3', 'Elige el número de poderes a visualizar', 10, 60, value = 30)
                                    ),
                      
                      mainPanel("Panel 1 - Descripcion de los datos / Panel Descriptivo",
                                tabsetPanel(
                                            tabPanel("Panel 1.1. - Análisis Alluvia", 
                                                     fluidRow(splitLayout(cellWidths = c("65%", "35%"), 
                                                              plotOutput("plotgraph4", height=500), 
                                                              htmlOutput("plottext4", inline = FALSE)
                                                              ))
                                                      ), 
                                            tabPanel("Panel 1.2. - Histogramas", 
                                                     fluidRow(splitLayout(cellWidths = c("65%", "35%"), 
                                                              plotOutput("plotgraph5", height=500), 
                                                              htmlOutput("plottext5", inline = FALSE)
                                                              ))
                                                      ),
                                            tabPanel("Panel 1.3. - Otro", 
                                                     fluidRow(splitLayout(cellWidths = c("65%", "35%"), 
                                                              plotOutput("plotgraph6", height=500), 
                                                              htmlOutput("plottext6", inline = FALSE)
                                                              ))
                                                      )
                                            )
                                )),
                      
                      # Inicio
                      
          tabPanel("Panel 2. - Analisis de Super Heroes y Super Poderes",
                   
                     sidebarPanel( 
                                 selectInput('x', 'Elige variable para eje X', v_continuas, 'Total_Poderes'),
                                 selectInput('y', 'Elige variable para eje Y', v_continuas, 'Total_Poder'),
                                 selectInput('color', 'Analisis de Detalle / Color', c(None='.', v_categoricas), 'Alignment'),
                                 selectInput('power', 'Elige Super Poder', c(All_Powers ='None', v_poderes), 'All_Powers'),
                                 
                                 checkboxInput('lm', 'Línea de Regresión'),
                                 checkboxInput('smooth', 'Suavizado LOESS'),

                                 selectInput('facet_row', 'Elige variable para facetas por filas', c(None='.', v_categoricas)),
                                 selectInput('heat_row', 'Elige variable para analisis de mapa de calor', c(None='.', v_categoricas), 'Specie')
                                ),
                               
                               mainPanel("Panel 2 - Analisis de Super Poderes / Panel Analítico",
                                         tabsetPanel(
                                           tabPanel("Panel 2.1. - Análisis de Dispersión", 
                                                    fluidRow(splitLayout(cellWidths = c("65%", "35%"), 
                                                                         plotOutput("plotgraph1", height=500,
                                                                                     dblclick = "plotgraph1_dblclick",  #nuevo
                                                                                     brush = brushOpts(                 #nuevo
                                                                                     id = "plotgraph1_brush",           #nuevo
                                                                                     resetOnNew = TRUE)                 #nuevo
                                                                                    ), 
                                                                         htmlOutput("plottext1", inline = FALSE)
                                                    ))
                                           ), 
                                           tabPanel("Panel 2.2. - Nube de Palabras", 
                                                    fluidRow(splitLayout(cellWidths = c("65%", "35%"), 
                                                                         plotOutput("plotgraph2", height=500), 
                                                                         htmlOutput("plottext2", inline = FALSE)
                                                    ))
                                           ),
                                           tabPanel("Panel 2.3. - Mapa de Calor", 
                                                    fluidRow(splitLayout(cellWidths = c("65%", "35%"), 
                                                                         plotOutput("plotgraph3", height=500), 
                                                                         htmlOutput("plottext3", inline = FALSE)
                                                    ))
                                           )
                                         )
                               ))                      
                      
                      # Fin
))


