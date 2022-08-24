ui <- fluidPage(
  tags$head(
    tags$link(href = "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css", rel="stylesheet", integrity="sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor", crossorigin="anonymous"),
    tags$style(paste0(readLines('www/css/custom.css'), collapse = '')),
    tags$script(src = 'js/custom.js')
  ),
  tags$body(
    useShinyjs(),
    uiOutput('page')
  )
)

server <- function(input, output, session) {
  pages <- function(page) {
    if(is.null(page)) {page <- 'home'}
    switch (
      page,
      'login' = LoginPage(page),
      'home' = HomePage(page)
    )
  }
  
}
shinyApp(ui = ui, server = server, onStart = function() {source("global.R")})
