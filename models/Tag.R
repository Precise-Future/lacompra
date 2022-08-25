newTag <- function(nombre) {
  return(list(
    id = random(), nombre = nombre
  ))
}

createTag <- function(newTag) {
  putDDB("tag", newTag)
  return(newTag$id)
}

readTag <- function(id) {
  getDDB("tag", id)
}

updateTag <- function(id, key, value) {
  x <- readTag(id)$Item
  x[[key]] <- value
  putDDB("tag", x)
}

deleteTag <- function(id) {
  deleteDDB("tag", id)
}

listTag <- function() {
  listDDB("tag")
}

findTag <- function(q) {
  filterDDB("tag", q)
}
