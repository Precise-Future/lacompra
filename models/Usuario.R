newUsuario <- function(nombre, apellidos, email, rol, photo = paste0(globals$img, 'iso-sm.webp'), calle = '', numero = '', detalle = '', cp = '', pais = '', ciudad = '', lat = 0, lng = 0) {
  return(
    list(
      id = random(), nombre = nombre, apellidos = apellidos, email = email, rol = rol, code = softRandom(), photo = photo, calle = calle, 
      numero = numero, detalle = detalle, cp = cp, pais = pais, ciudad = ciudad, lat = lat, lng = lng, verificado = 0, notificaciones = 1, created = as.character(today())
    )
  )
}

createUsuario <- function(newUsuario) {
  putDDB("usuario", newUsuario, 'ACCESS_URL')
  return(newUsuario$id)
}

readUsuario <- function(id) {
  getDDB("usuario", id, 'ACCESS_URL')
}

updateUsuario <- function(id, key, value) {
  x <- readUsuario(id)$Item
  x[[key]] <- value
  putDDB("usuario", x, 'ACCESS_URL')
}

deleteUsuario <- function(id) {
  deleteDDB("usuario", id, 'ACCESS_URL')
}

listUsuario <- function() {
  listDDB("usuario", 'ACCESS_URL')
}

findUsuario <- function(q) {
  filterDDB("usuario", q, 'ACCESS_URL')
}
