SplashPage <- function(id) {
  ns <- NS(id)
  tags$div(
    class = 'splash page pageIn',
    tags$img(src = paste0(globals$img, 'iso-sm.webp')),
    tags$div(spin_epic(spin = "spring", color = '#123465'), class = 'loader mt-5')
  )
}
SplashPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}