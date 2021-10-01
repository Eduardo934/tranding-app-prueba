
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
library(sodium)

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
               tabPanel("Main",
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            uiOutput("panel_lateral"),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                fluidRow(style="margin-left:50%;",
                                    shinyauthr::loginUI(id = "login", cookie_expiry = cookie_caducidad) 
                                ),
                                
                                uiOutput("main")
                            )
                        )
                        
                        ),
               tabPanel("Data",
                        uiOutput("panel_2")
                        )
               ),
    div(shinyauthr::logoutUI(id = "logout", label="Log out"), style="position: absolute; top: 8px; right: 20px; z-index: 1000;")

   
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
    
    ##Obtiene las emisoras guardadas por cada usuario
    ## Obtener los usuarios de la base de datos
    usuarios_tranding<-reactiveValues()
    sql_usuarios_tranding <- "SELECT * FROM usuario_tranding"
    consulta_usuarios_tranding <- sqlInterpolate(conexion_base, sql_usuarios_tranding)
    usuarios_tranding$emisoras<-dbGetQuery(conexion_base, consulta_usuarios_tranding)
    
    
    
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
    
    ## panel lateral 
    output$panel_lateral<-renderUI({
        req(credenciales()$user_auth)
        req(user_data()$permisos%in%"usuario")
        sidebarPanel(id="panel_lateral", width = 3, style = "position:fixed;width:20%;",
                     h3("Select Inputs", style="color:#333;"),
                     selectInput("time_series", "Time series", 
                                 choices = list("Daily" = "time_series_daily", 
                                                "Weekly" = "time_series_weekly", "Monthly" = "time_series_monthly")),
                     
                     tags$div(id="autocomplete", class="dropdown",
                              textInput("emisora", "Symbol Search", value="AMZN"),
                              tags$div(id="dropdown-content" ,class="dropdown-content")
                     ),
                     actionButton("buscar", "Buscar"),
                     h3("Save Data", style="color:#333;"),
                     actionButton("save", "Save"),
                     h3("Download data", style="color:#333;"),
                     uiOutput("boton_download")
                     
        )
    })
    
    ##Panel principal
    output$main<- renderUI(expr = if(credenciales()$user_auth){
        if(user_data()$permisos%in%"usuario"){
            div(
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
        } else {
            div(style="align-content: center;",
                column(width = 10, class="well", style="margin-left:30%;",
                       dataTableOutput("tabla_usuarios"),
                       br(),
                       br(),
                       fluidRow(
                           column(width = 2, actionButton("eliminar", label = "Eliminar",  class = "btn-new")),
                           column(width = 2, actionButton("nuevo", label = "Nuevo",  class = "btn-new"))
                       )
                )
            )
        }
    })
    
    ## Segunda pestaña
    output$panel_2<-renderUI(expr = if(credenciales()$user_auth){
        req(user_data()$permisos%in%"usuario")
        fluidRow(
            column(10,
                   uiOutput("nombre"),
                   br(),
                   
                   fluidRow(style="margin-left:100px;", class="well",
                            h3("Data saved by user"),        
                            dataTableOutput("tabla_guardados")
                   ),
                   br(),
                   h2("Simulation", style="margin-left:100px;"),
                   fluidRow(style="margin-left:100px;", class="well",
                            fluidRow(
                                column(4, numericInput("money", "Money",10000, min=1, max=100000)),
                                column(4, dateRangeInput("daterange3", "Date range:",
                                                         start  = "2001-01-01",
                                                         end    = "2010-12-31",
                                                         min    = "2001-01-01",
                                                         max    = "2021-09-30",
                                                         format = "mm/dd/yy",
                                                         separator = " - ")),
                                column(4, actionButton("emulate", "Emulate"), style="margin-top:18px;")
                            ),
                            br(),
                            fluidRow(
                                plotlyOutput("emulate_graph")
                            )
                   )
            )
        )
    })
    
    
    usuarios<- reactiveValues()
    sql_usuarios <- "SELECT * FROM usuarios"
    usuarios_base<-dbGetQuery(conexion_base, sql_usuarios)
    
    usuarios$datos<-usuarios_base
    
    output$tabla_usuarios<- renderDataTable({
        req(credenciales()$user_auth)
        req(user_data()$permisos %in%"administrador")
        
        usuarios_plot<- usuarios$datos
        
        datatable(usuarios_plot[,c("user", "permisos", "nombre")], options = list(
            pageLength = 10,
            scrollX = TRUE,
            searching = TRUE
        ))
    })
    
    observeEvent(input$nuevo,{
        showModal(
            modalDialog(
                title = "Nuevo usuario",
                fluidRow(
                    column(width = 12,
                           textInput("usuarioN", label = h4("Usuario")),
                           passwordInput("passwordN",label = h4("Constraseña")),
                           radioButtons("permisosN", label = h4("Permiso"),
                                        choiceNames = c("Usuario","Administrador"),
                                        choiceValues = c("usuario","administrador")
                           ),
                           textInput("nombreN",label = h4("Nombres"))
                    )
                ),
                easyClose = FALSE,
                footer = tagList(
                    actionButton("cancelar","Cancelar"),
                    actionButton("guardarN","Guardar")
                )
            )
        )
    })
    
    #### Guarda los usuarios en la base de datos; antes aplica algunas restricciones para añdir un nuevo usuario
    observeEvent(input$guardarN,{
        req(credenciales()$user_auth)
        req(user_data()$permisos%in%"administrador")
        
        if(nchar(input$usuarioN)<1 || nchar(input$passwordN)<1 ||
           nchar(input$nombreN)<1){
            showNotification(
                h4("Debes completar todos los campos"), 
                action = NULL, duration = 5, type = "warning")
        } else {
            sql_usuarios <- "SELECT * FROM usuarios"
            usuarios_base<-dbGetQuery(conexion_base, sql_usuarios)
            
            if(length(grep(input$usuarioN,usuarios_base$user))>0){
                showNotification(
                    h4("El nombre de usuario ya existe"), 
                    action = NULL, duration = 5, type = "warning")   
            } else {
                sql_nuevo_usuario <- 'INSERT INTO usuarios (user,password,permisos,nombre) 
                    VALUES (?iduser,?idpassword,?idpermiso,?idnombre);'
                
                consulta_usuarios <- sqlInterpolate(conexion_base, sql_nuevo_usuario, iduser= input$usuarioN, 
                                                    idpassword=password_store(input$passwordN),
                                                    idpermiso=input$permisosN,idnombre=input$nombreN)
                dbGetQuery(conexion_base, consulta_usuarios)
                
                removeModal()
                
                showNotification(
                    h4("Creación exitosa"), 
                    action = NULL, duration = 5, type = "message")
                
                sql_usuarios <- "SELECT * FROM usuarios"
                usuarios$datos<-dbGetQuery(conexion_base, sql_usuarios)
            }
            
        }
    })
    ####Valores reactivos que guardaran info de las selecciones en la tabla
    selecciones_tabla<- reactiveValues()
    
    
    ##Cierra las ventanas emergentes
    observeEvent(input$cancelar,{
        removeModal()
    })
    #### Ventana emergente para advertir de que se van a eliminar usuarios
    observeEvent(input$eliminar,{
        
        if(length(input$tabla_usuarios_rows_selected)>0){
            showModal(
                modalDialog(title = "Borrar",
                            fluidPage(column(12,h3("Cuidado: Estás a punto de borrar usuarios de la base de datos"),style="color:red;")),
                            easyClose = FALSE,
                            size = "m",
                            footer = tagList(
                                actionButton("cancelar","Cancelar"),
                                actionButton("borrar_usuario","Eliminar")
                            ) 
                )
            )
        } else {
            showNotification(
                h4("Selecciona un renglón"), 
                action = NULL, duration = 5, type = "warning") 
        }
        
    })
    
    ##### Elimina el usuario de la base de datos
    observeEvent(input$borrar_usuario,{
        req(credenciales()$user_auth)
        req(user_data()$permisos%in%"administrador")
        
        selecciones_tabla$renglon<-input$tabla_usuarios_rows_selected
        usuarios_a_borrar<-usuarios$datos[selecciones_tabla$renglon,"user"]
        
        sql_borrar_usuario <- paste("DELETE FROM usuarios WHERE user IN (",
                                    paste0(sprintf("'%s'",usuarios_a_borrar),collapse = ","),")")
        
        dbGetQuery(conexion_base, sql_borrar_usuario)
        removeModal()
        
        showNotification(
            h4("Usuario eliminado con éxito"), 
            action = NULL, duration = 5, type = "message")
        
        sql_usuarios <- "SELECT * FROM usuarios"
        usuarios$datos<-dbGetQuery(conexion_base, sql_usuarios) ###### Vuelve a leer la base para actualizar la tabla
        
    })
    
    ###Prueba con paquetería
    setAPIKey(key = "0PYUTT0O5MG98O0P")
    
    
    
    ###genera los resultados de busqueda de emisoras
    
    
    datos_reactivos<- reactiveValues()
   
    observe({
        req(credenciales()$user_auth)
        req(input$emisora)
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
    
    output$value <- renderPrint({ 
        req(credenciales()$user_auth)
        datos_reactivos$data 
        })

    
    output$tabla<-renderDataTable({
        req(credenciales()$user_auth)
        req(datos_reactivos$emisora)
        
        datatable(datos_reactivos$emisora)
    })
    
    ### Carga una emisora por default al momento de inicar la app
    emisora <- fetchSeries(function_nm = "time_series_daily", symbol = "AMZN", outputsize = "compact", datatype = "json")
    
    datos_reactivos$emisora<- fortify(emisora$xts_object)
    
    ## Llama  a la API y devuelve la info de la emisora
    
    observeEvent(input$buscar,{
        req(credenciales()$user_auth)
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
        req(credenciales()$user_auth)
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
        req(credenciales()$user_auth)
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
        req(credenciales()$user_auth)
        req(datos_reactivos$nombres)
        
        h2(paste("Symbol - ",datos_reactivos$nombres[2]))
    })
    
    output$tabla_tranding<-renderDataTable({
        req(credenciales()$user_auth)
        req(datos_reactivos$emisora)
        datosc<-datos_reactivos$emisora
        names(datosc)<-c("Date", "Open", "High", "low", "close", "volumen")
        datatable(datosc)
    })
    
    observeEvent(input$save,{
        req(credenciales()$user_auth)
        req(input$emisora)
        
        sql_nuevo_usuario <- 'INSERT INTO usuario_tranding (usuario,emisora, date) 
                    VALUES (?user,?emi, ?dat);'
        print(input$emisora)
        consulta_usuarios <- sqlInterpolate(conexion_base, sql_nuevo_usuario, user= user_data()$user, 
                                            emi=input$emisora, dat=Sys.Date())
        dbGetQuery(conexion_base, consulta_usuarios)
        
        sql_usuarios_tranding <- "SELECT * FROM usuario_tranding"
        consulta_usuarios_tranding <- sqlInterpolate(conexion_base, sql_usuarios_tranding)
        usuarios_tranding$emisoras<-dbGetQuery(conexion_base, consulta_usuarios_tranding)
        
        showNotification(h3("Data saved"), duration = 5,
                         id = NULL, type = c("message"))
    })
    
    output$nombre<- renderUI({
        req(user_data()$user)
        h2(paste("Welcome", user_data()$user), style= "margin-left:100px;")
    })
    
    output$tabla_guardados<-renderDataTable({
        req(credenciales()$user_auth)
        datos<-usuarios_tranding$emisoras[usuarios_tranding$emisoras$usuario %in% user_data()$user,]
        datatable(datos, selection = "single")
    })
    
    
    ### calculo
    observeEvent(input$emulate,{
        req(credenciales()$user_auth)
        if(is.null(input$tabla_guardados_rows_selected)){
            showNotification(h3("Select a row on table"), duration = 5,
                             id = NULL, type = c("error"))
            return()
        }
        emisora<- usuarios_tranding$emisoras[usuarios_tranding$emisoras$usuario %in% user_data()$user,]
        
        emisora<- emisora[input$tabla_guardados_rows_selected, "emisora"]
        
        
        emisora_data <- fetchSeries(function_nm = "time_series_monthly", symbol = emisora, outputsize = "full", datatype = "json")
        
        if(emisora_data$xts_object %in% "Bad response, error" || emisora_data$xts_object %in% "objeto 'out_xts' no encontrado"){
            showNotification(h3("No data available"), duration = 5,
                             id = NULL, type = c("error"))
            return()
        }
        datos_estadistica<- fortify(emisora_data$xts_object)
        datos_estadistica<- datos_estadistica[(datos_estadistica$Index > input$daterange3[1] & datos_estadistica$Index < input$daterange3[2]),]
        
        if(nrow(datos_estadistica) < 1){
            showNotification(h3("No data available on this date"), duration = 5,
                             id = NULL, type = c("error"))
            return()
        }
        names(datos_estadistica)[5]<-"close"
        nacciones <- input$money/datos_estadistica[1,"close"]
        
        datos_estadistica$inversion <- datos_estadistica$close * nacciones
        
        datos_reactivos$datos_estadistica<-datos_estadistica
        
    })
    
    output$emulate_graph<-renderPlotly({
        req(credenciales()$user_auth)
        req(datos_reactivos$datos_estadistica)
        datos<-datos_reactivos$datos_estadistica
        names(datos)<-c("Date", "Open", "High", "low", "close", "volume", "Invest")
        
        ggplot_plot1<-datos %>%
            ggplot(aes(x= Date, y= Invest))+
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
    
    ##
    output$boton_download<-renderUI(expr = if(!is.null(datos_reactivos$emisora)){
        downloadButton("dnw", "Donwload")
    } else {
        NULL
    })
    
    ## Desacarga los datos
    output$dnw<-downloadHandler(
        filename="tranding_data.csv",
        content=function(file){
            req(credenciales()$user_auth)
            req(datos_reactivos$emisora)
            datosd<-datos_reactivos$emisora
            names(datosd)<-c("Date", "Open", "High", "low", "close", "volumen")
            write.csv(datosd,file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
