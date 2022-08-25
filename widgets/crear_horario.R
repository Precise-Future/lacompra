CrearHorario <- function(id) {
  ns <- NS(id)
  tags$div(
    class = 'container',
    h3('Añadir Horario'),
    textInput(ns('nh'), NULL, placeholder = 'nombre'),
    tags$div(
      style = "display:flex; justify-content:center; place-items:center;", 
      tags$div(class = 'hm', numericInput(ns('hh'), 'Hora', value = 0, min = 0)),
      tags$div(class = 'hm', numericInput(ns('hm'), 'Minutos', value = 0, min = 0))
    ),
    uiOutput(ns('errors')),
    actionButton(ns('addHorario'), 'Añadir', class = 'btn btn-primary mt-5')
  )
}

CrearHorarioServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      errors <- reactiveVal()
      output$errors <- renderUI({
        er <- errors()
        if(is.null(er)) {tags$span()} else {tagList(lapply(1:length(er), function(i) {tags$h6(class = 'text-danger', er[i])}))}
      })
      observeEvent(input$addHorario, {
        newp <- secure_error_handle(newHorario(nombre = isolate(input$np), hora = paste0(isolate(input$hh), ":", isolate(input$hm))))
        if(newp$status == 1) {errors('No se ha podido crear la etiqueta. Por favor revise todos los campos')} else {
          pId <- secure_error_handle(createHorario(newp$data))
          if(pId$status == 0) {showNotification(ui = tags$h4('Se ha creado una nueva etiqueta'), type = 'message')} else {
            errors('No se ha podido crear la etiqueta. Por favor revise todos los campos')
          }
        }
      }, ignoreNULL = T, ignoreInit = T)
    }
  )
}