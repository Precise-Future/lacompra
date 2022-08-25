newNotificacion <- function(nombre, texto, cuerpo) {
  return(
    list(
      id = random(), nombre = nombre, texto = texto, cuerpo = cuerpo
    )
  )
}

createNotificacion <- function(newNotificacion) {
  putDDB("notificacion", newNotificacion, 'ACCESS_URL')
  return(newNotificacion$id)
}

readNotificacion <- function(id) {
  getDDB("notificacion", id, 'ACCESS_URL')
}

updateNotificacion <- function(id, key, value) {
  x <- readNotificacion(id)$Item
  x[[key]] <- value
  putDDB("notificacion", x, 'ACCESS_URL')
}

deleteNotificacion <- function(id) {
  deleteDDB("notificacion", id, 'ACCESS_URL')
}

listNotificacion <- function() {
  listDDB("notificacion", 'ACCESS_URL')
}

findNotificacion <- function(q) {
  filterDDB("notificacion", q)
}
