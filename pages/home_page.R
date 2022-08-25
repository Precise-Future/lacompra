HomePage <- function(id) {
  ns <- NS(id)
  tags$div(
    class = 'pageIn container-fluid',
    tags$div(
      style = "display:flex; justify-content:center; place-items:center; width:100%;",
      tags$div(
        class = 'left',
        
      ),
      tags$div(
        class = 'right',
        tags$div(
          
        ),
        tags$hr(),
        tags$div(
          
        ),
        tags$hr(),
        tags$div(
          h3('Añadir Unidad de Venta'),
          textInput(ns('nuv'), NULL, placeholder = 'nombre'),
        ),
        tags$hr(),
      )
    )
    
  )
}
HomePageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}