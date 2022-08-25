newHorario <- function(nombre, hora) {
  return(list(
    id = random(), nombre = nombre, hora = hora
  ))
}

createHorario <- function(newHorario) {
  putDDB("horario", newHorario)
  return(newHorario$id)
}

readHorario <- function(id) {
  getDDB("horario", id)
}

updateHorario <- function(id, key, value) {
  x <- readHorario(id)$Item
  x[[key]] <- value
  putDDB("horario", x)
}

deleteHorario <- function(id) {
  deleteDDB("horario", id)
}

listHorario <- function() {
  listDDB("horario")
}

findHorario <- function(q) {
  filterDDB("horario", q)
}
