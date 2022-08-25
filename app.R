ui <- bootstrapPage(
  tags$head(
    tags$link(href = "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css", rel="stylesheet", integrity="sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor", crossorigin="anonymous"),
    tags$style(paste0(readLines('www/css/custom.css'), collapse = '')),
    tags$script(src = 'js/custom.js')
  ),
  tags$body(
    shinyjs::useShinyjs(),
    uiOutput('page')
  )
)

server <- function(input, output, session) {
  session$userData$usuario <- reactiveVal(NULL)
  init(input, output, session)
  pages <- function(page) {
    if(is.null(page)) {page <- 'splash'}
    switch (
      page,
      'splash' = SplashPage(page),
      'login' = LoginPage(page),
      'home' = HomePage(page)
    )
  }
  output$page <- renderUI({
    pages(input$page)
  })
  
  session$userData$db$productos <- listProducto()$Items %>% entityList2DF
  session$userData$db$platos <- listPlato()$Items %>% entityList2DF
  session$userData$db$tags <- listTag()$Items %>% entityList2DF
  session$userData$db$uvs <- listUV()$Items %>% entityList2DF
  session$userData$db$horarios <- listHorario()$Items %>% entityList2DF
  
  sessionCheck(input, output, session)
  
  validateAccount <- reactive({
    s <- session$clientData$url_serach
    s <- parseQueryString(s)
    if(!is.null(s[['a']])) {
      i <- secure_error_handle(findIdentity(q = list(account = s[['a']]))$Items[[1]])
      if(i$status == 1) return(NULL) else {
        if(i$data$account == 0) return(NULL) else {
          updateIdentity(i$data$id, 'account', 0)
          # updateUsuario(i$data$uId, 'notificaciones', notifica)
        }
      }
    }
  })
  
  delay(500, {
    
    LoginPageServer('login')
    HomePageServer('home')
    
  })
  
  
}
shinyApp(ui = ui, server = server, onStart = function() {source("global.R")})
