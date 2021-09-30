
library(shiny)
library(shinythemes)
library(AlphaVantageClient)
library(DT)
library(httr)
library(jsonlite)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyauthr)
library(RMySQL)
library(DBI)
library(pool)
library(lubridate)
library(shinyalert)

jsCode <- 'shinyjs.pageCol = function(params){

$( function() {
    var data= params[0];
    
    $(\'#dropdown-content\').empty();
    
    if(Array.isArray(data) == false){
     return;
    }
    
    function addCode() {
               
               
               for(var i = 0; i< data.length; i++){
               
                 document.getElementById("dropdown-content").innerHTML += 
                \"<a onClick= \'close_list(this)\'>\" + data[i] + \"</a>\";
               }
             }
    addCode()
    
} );

}'

cookie_caducidad<- 1
# Define UI for application that draws a histogram
ui <- bootstrapPage(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("pageCol")),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$link(rel = "stylesheet", type = "text/css", href="//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
        tags$link(rel = "stylesheet", type = "text/css", href="/resources/demos/style.css"),
        tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
        tags$script(src = "myscript.js")
    ),
    # Application title
    theme = shinytheme("cerulean"),
    
    navbarPage(title = "Tranding App", id="tabs", collapsible = TRUE,
               tabPanel("Search",
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectInput("time_series", "Time series", 
                                                     choices = list("Daily" = "time_series_daily", 
                                                                    "Weekly" = "time_series_weekly", "Monthly" = "time_series_monthly")),
                                         
                                         tags$div(id="autocomplete", class="dropdown",
                                                  textInput("emisora", "Symbol Search"),
                                                  tags$div(id="dropdown-content" ,class="dropdown-content")
                                         ),
                                         actionButton("buscar", "Buscar"),
                                         h3("Save Data"),
                                         actionButton("save", "Save"),
                                         h3("Download data"),
                                         actionButton("dnw", "Donwload")
                                         
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                fluidRow(
                                    shinyauthr::loginUI(id = "login", cookie_expiry = cookie_caducidad,
                                                        title = "Iniciar Sesión",
                                                        user_title = "Nombre de Usuario",
                                                        pass_title = "Contraseña",
                                                        login_title = "Log in",
                                                        error_message = "Nombre de usuario o contraseña incorrectas!"
                                    ) 
                                ),
                                
                                fluidRow(
                                    uiOutput("title"),
                                    column(6,
                                           plotlyOutput('plot_data')
                                           
                                    ), 
                                    column(6,
                                           plotlyOutput('plot_volumen')
                                    )
                                ),
                               fluidRow( class="well",
                                   dataTableOutput("tabla_tranding")
                               )
                            )
                        )
                        
                        )
               ),
    div(shinyauthr::logoutUI(id = "logout", label="Salir"), style="position: absolute; top: 8px; right: 20px; z-index: 1000;")

   
)

# Define server logic required to draw a histogram

conexion_base <- dbPool(
    drv = dbDriver("MySQL", max.con = 1000),
    dbname = "prueba",
    host = "database-pruebas.cayjegc6zikj.us-east-2.rds.amazonaws.com",
    user = 'admin',
    password = 'admin_admin',
    idleTimeout = 3600000 ###una hora
)


server <- function(input, output, session) {
    ###shinyauthr
    ## Días para que la sesión expire
    
    
    ## Función para guardas información de las sesiones en la base de datos
    add_sessionid_to_db <- function(user, sessionid, conn = conexion_base) {
        tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
            dbWriteTable(conn, "sessionids", ., append = TRUE, row.names = FALSE)
    }
    
    ## Función que regresa un data frame con la información del usuario y la sesión
    get_sessionids_from_db <- function(conn = conexion_base, expiry = cookie_caducidad) {
        dbReadTable(conn, "sessionids") %>%
            mutate(login_time = ymd_hms(login_time)) %>%
            as_tibble() %>%
            filter(login_time > now() - days(expiry))
    }
    
    ## Obtener los usuarios de la base de datos
    sql_usuarios <- "SELECT * FROM usuarios"
    consulta_usuarios <- sqlInterpolate(conexion_base, sql_usuarios)
    usuarios_base<-dbGetQuery(conexion_base, consulta_usuarios)
    
    
    
    ##Modulo de la paquetería shinyauthr para el login de la página usando las credenciales creadas
    logout_init <- callModule(
        shinyauthr::logout,
        id = "logout",
        active = reactive(credenciales()$user_auth)
    )
    
    ## Llama al modulo de login con las columnas de las contraseñas y usuarios de la base de datosy
    ### crea las credenciales
    credenciales <- callModule(
        shinyauthr::login,
        id = "login",
        data = usuarios_base,
        user_col = user, 
        pwd_col = password,
        sessionid_col = sessionid, 
        cookie_getter = get_sessionids_from_db,
        cookie_setter = add_sessionid_to_db,
        sodium_hashed = TRUE,
        log_out = reactive(logout_init())
    )
    
    
    ##Lee las credenciales de acceso cuando unusuario está registrado o no
    user_data <- reactive({
        credenciales()$info
    })
    
    
    
    ###Prueba con paquetería
    setAPIKey(key = "0PYUTT0O5MG98O0P")
    
    
    
    ###Prueba con API directamenteg
    
    
    datos_reactivos<- reactiveValues()
   
    observe({
        if(nchar(input$emisora) %in% 0){
            return()
        }
        res = GET('https://www.alphavantage.co/query',
                  query = list("function" = "SYMBOL_SEARCH", keywords = input$emisora, apikey="0PYUTT0O5MG98O0P"))
        
        datos_emisoras <- fromJSON(rawToChar(res$content))
        if(class(datos_emisoras$bestMatches) != "data.frame"){
          datos_reactivos$data = "none"
        } else {
          datos_reactivos$data = datos_emisoras$bestMatches[,1]
        }
        
        
       
        
        js$pageCol(datos_reactivos$data)
        
    })
    
    output$value <- renderPrint({ datos_reactivos$data })

    
    output$tabla<-renderDataTable({
        req(datos_reactivos$emisora)
        
        datatable(datos_reactivos$emisora)
    })
    
    observeEvent(input$buscar,{
        req(input$time_series)
        req(input$emisora)
        emisora <- fetchSeries(function_nm = input$time_series, symbol = input$emisora, outputsize = "compact", datatype = "json")

        if(emisora$xts_object %in% "Bad response, error" || emisora$xts_object %in% "objeto 'out_xts' no encontrado"){
            showNotification(h3("No data available"), duration = 5,
                             id = NULL, type = c("error"))
            return()
        }
        datos_reactivos$emisora<- fortify(emisora$xts_object)
        
        datos_reactivos$nombres<- c(input$time_series, input$emisora)
    })
    
    output$plot_data<-renderPlotly({
        req(datos_reactivos$emisora)
        
        datos<-datos_reactivos$emisora
        names(datos)<-c("Date", "Open", "High", "low", "close", "volume")
        
        ggplot_plot1<-datos %>%
            ggplot(aes(x= Date, y= Open))+
            geom_line(color ="blue") + 
            scale_y_log10() +
            labs(title = "Line Chart", 
                 subtitle = "Log Scale", 
                 y = "Closing Price", x = "") + 
            theme_linedraw()+
            theme(
                panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent", color = NA)
            )
        
        ggplotly(ggplot_plot1) %>%config(displaylogo=FALSE)
    })
    
    output$plot_volumen<-renderPlotly({
        req(datos_reactivos$emisora)
        
        datosb<-datos_reactivos$emisora
        names(datosb)<-c("Date", "Open", "High", "low", "close", "volumen")
        
        ggplot_plot2<-datosb %>%
            ggplot(aes(x = Date, y = volumen)) +
            geom_segment(aes(xend = Date, yend = 0, color = volumen)) + 
            geom_smooth(method = "loess", se = FALSE) +
            labs(title = "Volume Chart", 
                 subtitle = "Charting  Volume", 
                 y = "Volume", x = "") +
            theme_linedraw() +
            theme(legend.position = "none", 
                  panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = NA)
                  ) 
        
        ggplotly(ggplot_plot2) %>%config(displaylogo=FALSE)
    })
    
    output$title<-renderUI({
        req(datos_reactivos$nombres)
        
        h2(paste("Symbol - ",datos_reactivos$nombres[2]), style="color:#FFFFFF; margin-left:70px;")
    })
    
    output$tabla_tranding<-renderDataTable({
        req(datos_reactivos$emisora)
        datosc<-datos_reactivos$emisora
        names(datosc)<-c("Date", "Open", "High", "low", "close", "volumen")
        datatable(datosc)
    })
    
    observeEvent(input$save,{
        req(input$emisora)
        
        sql_nuevo_usuario <- 'INSERT INTO usuario_tranding (usuario,emisora, date) 
                    VALUES (?user,?emi, ?dat);'
        
        consulta_usuarios <- sqlInterpolate(conexion_base, sql_nuevo_usuario, user= user_data()$user, 
                                            emi=input$emisora, dat=Sys.Date())
        dbGetQuery(conexion_base, consulta_usuarios)
        
        shinyalert("Good", "Data stored on database.", type = "success")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
