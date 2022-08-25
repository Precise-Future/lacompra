CrearTag <- function(id) {
  ns <- NS(id)
  tags$div(
    class = 'container',
    h3('Añadir Tags'),
    textInput(ns('nt'), NULL, placeholder = 'nombre'),
    uiOutput(ns('errors')),
    actionButton(ns('addTag'), 'Añadir', class = 'btn btn-primary mt-5')
  )
}

CrearTagServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      errors <- reactiveVal()
      output$errors <- renderUI({
        er <- errors()
        if(is.null(er)) {tags$span()} else {tagList(lapply(1:length(er), function(i) {tags$h6(class = 'text-danger', er[i])}))}
      })
      observeEvent(input$addTag, {
        newp <- secure_error_handle(newTag(nombre = isolate(input$np)))
        if(newp$status == 1) {errors('No se ha podido crear la etiqueta. Por favor revise todos los campos')} else {
          pId <- secure_error_handle(createTag(newp$data))
          if(pId$status == 0) {showNotification(ui = tags$h4('Se ha creado una nueva etiqueta'), type = 'message')} else {
            errors('No se ha podido crear la etiqueta. Por favor revise todos los campos')
          }
        }
      }, ignoreNULL = T, ignoreInit = T)
    }
  )
}