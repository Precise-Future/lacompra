CrearProducto <- function(id) {
  ns <- NS(id)
  tags$div(
    class = 'container',
    h3('Añadir producto'),
    textInput(ns('np'), NULL, placeholder = 'nombre'),
    numericInput(ns('pp'), 'Precio', value = 0, min = 0),
    selectInput(ns('uv'), 'Unidad de venta', choices = ''),
    numericInput(ns('pu'), 'Precio / Unidad de Venta', value = 0, min = 0),
    numericInput(ns('usos'), 'Usos', value = 0, min = 0),
    textInput(ns('sm'), NULL, placeholder = 'Supermercado'),
    selectInput(ns('tags'), 'Etiquetas', choices = '', multiple = T),
    uiOutput(ns('errors')),
    actionButton(ns('addProducto'), 'Añadir', class = 'btn btn-primary mt-5')
  )
}

CrearProductoServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      errors <- reactiveVal()
      output$errors <- renderUI({
        er <- errors()
        if(is.null(er)) {tags$span()} else {tagList(lapply(1:length(er), function(i) {tags$h6(class = 'text-danger', er[i])}))}
      })
      observeEvent(input$addProducto, {
        newp <- secure_error_handle(newProducto(nombre = isolate(input$np), precio = isolate(input$pp), uv = isolate(input$uv), precio_x_unidad = isolate(input$pu),
                                                usos = isolate(input$usos), supermercado = isolate(input$sm), tags = isolate(input$tags)))
        if(newp$status == 1) {errors('No se ha podido crear el producto. Por favor revise todos los campos')} else {
          pId <- secure_error_handle(createProducto(newp$data))
          if(pId$status == 0) {showNotification(ui = tags$h4('Se ha creado un nuevo producto'), type = 'message')} else {
            errors('No se ha podido crear el producto. Por favor revise todos los campos')
          }
        }
      }, ignoreNULL = T, ignoreInit = T)
    }
  )
}