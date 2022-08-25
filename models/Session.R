newSession <- function(uId, ip = '') {
  return(
    list(
      id = random(),
      uId = uId,
      ip = ip,
      created = as.character(now())
    )
  )
}

createSession <- function(newSession) {
  putDDB("session", newSession, 'ACCESS_URL')
  return(newSession$id)
}

readSession <- function(id) {
  getDDB("session", id, 'ACCESS_URL')
}

updateSession <- function(id, key, value) {
  x <- readSession(id)$Item
  x[[key]] <- value
  putDDB("session", x, 'ACCESS_URL')
}

deleteSession <- function(id) {
  deleteDDB("session", id, 'ACCESS_URL')
}

listSession <- function() {
  listDDB("session", 'ACCESS_URL')
}

findSession <- function(q) {
  filterDDB("session", q, 'ACCESS_URL')
}
