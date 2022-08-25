newUV <- function(nombre) {
  return(list(
    id = random(), nombre = nombre
  ))
}

createUV <- function(newUV) {
  putDDB("uv", newUV)
  return(newUV$id)
}

readUV <- function(id) {
  getDDB("uv", id)
}

updateUV <- function(id, key, value) {
  x <- readUV(id)$Item
  x[[key]] <- value
  putDDB("uv", x)
}

deleteUV <- function(id) {
  deleteDDB("uv", id)
}

listUV <- function() {
  listDDB("uv")
}

findUV <- function(q) {
  filterDDB("uv", q)
}
