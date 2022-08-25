newIdentity <- function(id, uId, account) {
  return(
    list(
      id = id,
      uId = uId, 
      account = account
    )
  )
}

createIdentity <- function(newIdentity) {
  putDDB("identity", newIdentity, 'ACCESS_URL')
  return(newIdentity$id)
}

readIdentity <- function(id) {
  getDDB("identity", id, 'ACCESS_URL')
}

updateIdentity <- function(id, key, value) {
  x <- readIdentity(id)$Item
  x[[key]] <- value
  putDDB("identity", x, 'ACCESS_URL')
}

deleteIdentity <- function(id) {
  deleteDDB("identity", id, 'ACCESS_URL')
}

listIdentity <- function() {
  listDDB("identity", 'ACCESS_URL')
}

findIdentity <- function(q) {
  filterDDB("identity", q, 'ACCESS_URL')
}
