newPlato <- function(nombre, horario, usuario, ingredientes, tags, precio = 0) {
  if(precio == 0) {
    precio <- sum(unlist(Map(function(i) {i$precio * i$cantidad}, ingredientes)), na.rm = T)
  }
  return(list(
    id = random(), nombre = nombre, horario = horario, usuario = usuario, ingredientes = ingredientes, tags = tags, precio = precio
  ))
}

createPlato <- function(newPlato) {
  putDDB("plato", newPlato)
  return(newPlato$id)
}

readPlato <- function(id) {
  getDDB("plato", id)
}

updatePlato <- function(id, key, value) {
  x <- readPlato(id)$Item
  x[[key]] <- value
  putDDB("plato", x)
}

deletePlato <- function(id) {
  deleteDDB("plato", id)
}

listPlato <- function() {
  listDDB("plato")
}

findPlato <- function(q) {
  filterDDB("plato", q)
}
