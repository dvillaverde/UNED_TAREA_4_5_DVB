#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# 1. Instalamos los paquetes y activamos las librerías necesarias

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

#2. Cargamos los datos:
# Cargamos los datos desde el repositorio github creado especialmente para el módulo 

Super_Heroes_Database2 <-read.csv("https://github.com/dvillaverde/UNED_TAREA_4_5_DVB/blob/Data/Super_Heroes_Database.csv?raw=true",header=T, dec = ",", sep= ";")
Super_Heroes_Powers2 <-read.csv("https://github.com/dvillaverde/UNED_TAREA_4_5_DVB/blob/Data/Super_Heroes_Powers.csv?raw=true",header=T, dec = ",", sep= ";")

#3. CREAMOS LOS DATA.FRAMES QUE NOS INTERESA UTILIZAR

# Nos quedamos solo con los Super Heroes de Marvel Comics y DC Comics
Super_Heroes2 <- rbind(subset(Super_Heroes_Database2, Publisher == "DC Comics"),subset(Super_Heroes_Database2, Publisher == "Marvel Comics"))

#4. LIMPIAMOS Y TRATAMOS LOS DATOS
#Pasamos los indefinidos a NAs
Super_Heroes2$Weight[Super_Heroes2$Weight == "-99"] <- NA
Super_Heroes2$Height[Super_Heroes2$Height == "-99"] <- NA
Super_Heroes2$Gender[Super_Heroes2$Gender == "-"] <- NA
Super_Heroes2$Eye.color[Super_Heroes2$Eye.color == "-"] <- NA
Super_Heroes2$Specie[Super_Heroes2$Specie == ""] <- NA
Super_Heroes2$Race[Super_Heroes2$Race == "-"] <- NA
Super_Heroes2$Hair.color[Super_Heroes2$Hair.color == "-"] <- NA
Super_Heroes2$Skin.color[Super_Heroes2$Skin.color == "-"] <- NA
Super_Heroes2$Alignment[Super_Heroes2$Alignment == "-"] <- NA

#Arreglamos los nombres de los Super Heroes y de los Poderes y los pasamos a Factor
Super_Heroes2$name <- gsub(" ", "_", Super_Heroes2$name, fixed = TRUE)
Super_Heroes_Powers2$name <- gsub(" ", "_", Super_Heroes_Powers2$name, fixed = TRUE)
Super_Heroes2$name <- as.factor(Super_Heroes2$name)
Super_Heroes_Powers2$name <- as.factor(Super_Heroes_Powers2$name)

#Eliminamos la columna ID en el primer data.frame
Super_Heroes2 <- Super_Heroes2[,-1]

#5. CREAMOS LAS VARIABLES PARA LA SELECCION
# distinguimos variables "a nivel de intervalo" ("continuas" para ggplot)
nums <- sapply(Super_Heroes2[,-(12:178)], is.numeric)
v_continuas <- names(Super_Heroes2[,-(12:178)])[nums]

# las variables "categóricas" ("discretas" para ggplot)
cats <- sapply(Super_Heroes2[,2:11], is.factor)
v_categoricas <- names(Super_Heroes2[,2:11])[cats]

# las variables "categóricas" ("discretas" para ggplot) para los poderes
v_poderes <- names(Super_Heroes2[, 12:178])

# variables categóricas simples ("para el gráfico Alluvia")
v_alluvia <- c("Publisher","Gender","Alignment","Specie")


#6. COMIENZO DE LA APLICACIÓN

shinyUI(

    navbarPage("UNED - TEMA 4.5 - Ejercicio Visualizacion Avanzada Shiny",

#Inicio Panel 0               
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
                        a(href="https://www.kaggle.com/claudiodavi/superhero-set", "https://www.kaggle.com/claudiodavi/superhero-set"),
                        p("Adicionalmente a este juego de datos, existe una página web - https://www.superherodb.com/powers/ - donde se ha cuantificado
                          el valor de cada uno de los super poderes descritos. Con un pequeño proceso manual es sencillo de convertir
                          en un data.set utilizable para nuestro análisis (en la siguiente web):"),
                        p("En cualquier caso, y dado que se trata de un número razonable de registros, he realizado mi propio
                           proceso de revisión y depuración de los datos utilizando una hoja de cálculo. Los datos resultantes
                          - y que son los utilizados en la aplicación Shiny generada - están colgados en mi página de GitHub:"),
                        a(href="https://github.com/dvillaverde/UNED_TAREA_4_5_DVB/tree/Data", "https://github.com/dvillaverde/UNED_TAREA_4_5_DVB/tree/Data"),
                        p("El contenido subido (y que tambén se entrega dentro del fichero zip) se encuentra dividido en dos ficheros:"),
                        p("a. Super_Heroes_Database.csv"),
                        p("b. Super_Heroes_Powers.csv"),
                        hr(""),
                        h4("3. Estructura del ejercicio y de la aplicación Shiny:"),
                        p("                                                                  "),
                        p("Aunque la aplicación puede ejecutarse en el entorno local, al haberse entregado los ficheros
                           ui.R y server.R - como se explica en el apartado 4 - La aplicación también se encuentra publicada
                          y es accesible en el siguiente enlace:"),
                        a(href="https://dvillaverd.shinyapps.io/ejercicio_modulo/", "https://dvillaverd.shinyapps.io/ejercicio_modulo/"),
                        br(),
                        p("La aplicación se divide en tres paneles principales:"),
                        p(strong("a.) Panel 0 - Introduccion y descripción del trabajo realizado")),
                        p("Es el panel en el que nos encontramos. Pretende ser una introducción al ejercicio y explicar los
                           datos utilizados, la estructura de la aplicación y la información aportada para la evaluación
                           del módulo cursado"),
                        p(strong("IMPORTANTE: "),"Aunque en la propuesta de esqueleto que nos proporcionó el profesor como ejemplo se proponía utilizar
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
                        p(em("Panel 1.2. Análisis de Distribución en Violín: "), "Mediante este análisis podremos analizar la distribución
                          de las diferentes variables contínuas del data.set en función de las diferentes variables categóricas, entendiendo de 
                          esta forma la distribución de la información de los diferentes Super-Heroes"),
                        p(em("Panel 1.3. Ranking de Super Poderes: "), "En este caso, en lugar de estudiar los datos de los Super Héroes, analizamos
                          información interesante sobre los Super Poderes. En concreto podremos ver los principales Super Poderes
                          que más comunes son y aquellos más escasos. De igual forma, podremos visualizar aquellos más poderosos y aquellos
                          que no aportan una diferencia estratégica a aquel que lo posee"),                        
                        p(strong("c.) Panel 2. - Analisis de Super Heroes y Super Poderes")),
                        p("En este panel se pretende poder trabajar con la información del data set y poder analizar la
                           información disponible con la posibilidad de hacer filtros por un detreminado super-poder.
                          Al igual que en la pestaña anterior, dispondremos de una zona a la izquierda para seleccionar los
                          valores que utilizaremos en los diferentes paneles de la derecha, divididos en:"),
                        p(em("Panel 2.1. Dispersión de valores: "), "Dispersión de los valores cuantitativos del data.set mostrando por colores y facetas
                          las variables cateogóricas que nos interesen así como una propuesta de líneas de regresión. Al seleccionar un
                          determinado Super-Poder, el gráfico nos muestra coloreados sólo aquellos super-heroes que los contienen, manteniendo
                          en color gris el resto de Super-Héroes. Como valor adicional, y dado que existen valores muy alejados en los rangos cuantitativos,
                          existe la posibilidad de hacer zoom sobre una determinada zona tal y como se explicia en la nota al pie."),
                        p(em("Panel 2.2. Super Héroes más poderosos: "), "Mediante una nube de palabras, muy de moda hoy en día, se muestran aquellos super héroes que
                          tienen un determinado super-poder seleccionado (si no se ha seleccionado ninguno muestra todos). El tamaño de la palabra está relacionado
                          con el poder del individuo, por lo que aquellos con mayores tamaños de letra serán más poderosos y vice-versa."),
                        p(em("Panel 2.3. Distribución mediante mapa de calor: "), "En este último bloque vemos la distribución del poder total de los diferentes Super-Héroes
                          categorizados en función de dos variables categóricas seleccionadas en un -mapa de calor-, lo que nos permite
                          identificar las zonas donde se concentra el poder."),     
                        hr(""),
                        h4("4. Componentes / Información entregada:"),
                        p("                                                                  "),
                        p("En la página web de la UNED he subido el siguiente fichero: ",strong(em("UNED_MBDYBA_Modulo_4_5_TAREA_DVB_2018_08_14.zip"))),
                        p("En dicho fichero se encuentra almacenado el siguiente contenido:"),
                        p("1.) El código R: ui.R / server.R"),
                        p("2.) Los ficheros originales descargados de la web"),
                        p("3.) Los ficheros procesados y utilizados por la aplicación"),  
                        p("4.) Fichero pdf con los links a la dirección de Snyapps.io en la que se encuentra la aplicación y resto de información relevante"),
                        hr(""),
                        p(strong("Firmado:", "David Villaverde Benito"))
                        )),
#Final Panel 0
#Inicio Panel 1
             tabPanel("Panel 1 - Analisis de las caracteristicas de la Muestra",
                        sidebarPanel( 
                                    h4(strong("Variables para análisis de Super Héroes - Panel 1.1")),          
                                    selectInput('v1', 'Elige Tramo 1', v_alluvia, 'Publisher'),
                                    selectInput('v2', 'Elige Tramo 2', v_alluvia, 'Gender'),
                                    selectInput('v3', 'Elige Tramo 3 / Color', v_alluvia, 'Alignment'),
                                    h4(strong("Variables para análisis de Distribuciones - Panel 1.2")),          
                                    selectInput('x2', 'Elige variable para eje X y Color', v_categoricas, 'Specie'),
                                    selectInput('y2', 'Elige variable para eje Y', v_continuas, 'Total_Poderes'),
                                    selectInput('facet_row2', 'Elige variable para facetas por filas', c(None='.', v_alluvia)),
                                    h4(strong("Variables para análisis de Super Poder - Panel 1.3")),
                                    radioButtons('powers_v1', 'Elige variable para analisis de Super Poderes', c("cuenta", "Valor"), 'cuenta', inline = T),
                                    radioButtons('powers_v2', 'Elige el orden para analisis de Super Poderes', c("Mayores", "Menores"), 'Mayores', inline = T),
                                    sliderInput('powers_v3', 'Elige el número de poderes a visualizar', 10, 60, value = 30)
                                    ),
                      
                      mainPanel("Panel 1 - Descripcion de los datos / Panel Descriptivo",
                                tabsetPanel(
                                            tabPanel("Panel 1.1. - Análisis Alluvia", 
                                                     fluidRow(splitLayout(cellWidths = c("67%", "33%"), 
                                                              plotOutput("plotgraph4", height=600), 
                                                              htmlOutput("plottext4", inline = FALSE)
                                                              ))
                                                      ), 
                                            tabPanel("Panel 1.2. - Distribuciones en Violín", 
                                                     fluidRow(splitLayout(cellWidths = c("67%", "33%"), 
                                                              plotOutput("plotgraph5", height=600), 
                                                              htmlOutput("plottext5", inline = FALSE)
                                                              ))
                                                      ),
                                            tabPanel("Panel 1.3. - Ranking de Super Poderes", 
                                                     fluidRow(splitLayout(cellWidths = c("67%", "33%"), 
                                                              plotOutput("plotgraph6", height=600), 
                                                              htmlOutput("plottext6", inline = FALSE)
                                                              ))
                                                      )
                                            )
                                )),
                      
#Final Panel 1
#Inicio Panel 2

          tabPanel("Panel 2. - Analisis de Super Heroes y Super Poderes",
                   
                     sidebarPanel( 
                                h4(strong("Filtro por Super Poder")),  
                                selectInput('power', 'Elige un Super Poder para filtrar todo el análisis', c(All_Powers ='None', v_poderes), 'All_Powers'),
                                h4(strong("Variable para Gráfico de Dispersión - Panel 2.1 -")),  
                                selectInput('x', 'Elige variable para eje X', v_continuas, 'Total_Poderes'),
                                 selectInput('y', 'Elige variable para eje Y', v_continuas, 'Total_Poder'),
                                h4(strong("Información adicional / Detalles")), 
                                selectInput('color', 'Analisis de Detalle / Color', v_categoricas, 'Alignment'),
                                selectInput('facet_row', 'Elige variable para facetas por filas', c(None='.', v_categoricas)), 
                                h4(strong("Zona de elección de regresiones")), 
                                 checkboxInput('lm', 'Línea de Regresión'),
                                 checkboxInput('smooth', 'Suavizado LOESS'),
                                 hr(""),
                                 h4(strong("Variable para análisis de Mapa de Calor - Panel 2.3 -")),          
                                 selectInput('heat_row', 'Elige variable para analisis de mapa de calor', v_categoricas, 'Specie')
                                ),
                               
                               mainPanel("Panel 2 - Analisis de Super Poderes / Panel Analítico",
                                         tabsetPanel(
                                           tabPanel("Panel 2.1. - Análisis de Dispersión", 
                                                    fluidRow(splitLayout(cellWidths = c("67%", "33%"), 
                                                                         plotOutput("plotgraph1", height=600,
                                                                                     dblclick = "plotgraph1_dblclick",  #nuevo
                                                                                     brush = brushOpts(                 #nuevo
                                                                                     id = "plotgraph1_brush",           #nuevo
                                                                                     resetOnNew = TRUE)                 #nuevo
                                                                                    ), 
                                                                         htmlOutput("plottext1", inline = FALSE)
                                                    ))
                                           ), 
                                           tabPanel("Panel 2.2. - Nube de Palabras", 
                                                    fluidRow(splitLayout(cellWidths = c("67%", "33%"), 
                                                                         plotOutput("plotgraph2", height=600), 
                                                                         htmlOutput("plottext2", inline = FALSE)
                                                    ))
                                           ),
                                           tabPanel("Panel 2.3. - Mapa de Calor", 
                                                    fluidRow(splitLayout(cellWidths = c("67%", "33%"), 
                                                                         plotOutput("plotgraph3", height=600), 
                                                                         htmlOutput("plottext3", inline = FALSE)
                                                    ))
                                           )
                                         )
                               ))                      
                      
#Final Panel 2

))


