LoginPage <- function(id) {
  ns <- NS(id)
  tags$div(
    class = 'page pageIn',
    textInput(ns('email'), NULL, placeholder = 'email'),
    passwordInput(ns('pass'), NULL, placeholder = 'contraseña'),
    uiOutput(ns('errors')),
    actionButton(ns('login'), 'Iniciar sesión', class = 'btn btn-primary mt-5')
  )
}
LoginPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      errors <- reactiveVal()
      output$errors <- renderUI({
        er <- errors()
        if(is.null(er)) {tags$span()} else {
          tagList(
            lapply(1:length(er), function(i) {tags$h6(class = 'text-danger', er[i])})
          )
        }
      })
      observeEvent(input$login, {
        e <- isolate(input$email)
        p <- isolate(input$pass)
        v <- c(validar(e, 'Email', list('noNull', 'noEmpty', 'email')), validar(p, 'Contraseña', list('noNull', 'noEmpty', 'pass')))
        if(any(v != '')) {errors(v)} else {
          i <- readIdentity(make_identity(e, p))$Item
          if(is.null(i$uId)) {
            erros('Usuario o contraseña incorrectos')
          } else {
            u <- readUsuario(i$uId)$Item
            if(is.null(u)) {
              errors('Error desconocido al iniciar sesión')
            } else {
              session$userData$usuario(u)
              s <- createSession(newSession(u$id))
              x <- paste0("setCookie('pfSession','", s, "');")
              print(paste0('To set: ', x))
              runjs(x)
              runjs('Shiny.setInputValue("page", "home");')
            }
          }
        }
      }, ignoreNULL = T, ignoreInit = T)
    }
  )
}