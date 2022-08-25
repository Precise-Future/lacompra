####### API and Dynamodb Jobs ######
getDDB <- function(entity = "identity", id = 'wasabi', type = 'API_URL') {
  token <- 'API_TOKEN'
  if(type == 'ACCESS_URL') token <- 'ACCESS_TOKEN'
  request <- paste0(Sys.getenv(type), entity, "?token=", Sys.getenv(token), "&id=", id)
  response <- httr::GET(
    url = request
  )
  return(httr::content(response))
}

listDDB <- function(entity, type = 'API_URL') {
  token <- 'API_TOKEN'
  if(type == 'ACCESS_URL') token <- 'ACCESS_TOKEN'
  request <- paste0(Sys.getenv(type), entity, "?token=", Sys.getenv(token))
  response <- httr::GET(
    url = request
  )
  return(httr::content(response))
}
# entity <- "comuna"
# object <- readComuna("36")
# object <- object$Item
# object$origen <- 1
putDDB <- function(entity = "inmueble", object, type = 'API_URL') {
  token <- 'API_TOKEN'
  if(type == 'ACCESS_URL') token <- 'ACCESS_TOKEN'
  request <- paste0(Sys.getenv(type), entity, "?token=", Sys.getenv(token))
  response <- httr::PUT(
    url = request,
    encode = 'json',
    body = I(object)
  )
  print(content(response))
}

deleteDDB <- function(entity, id, type = 'API_URL') {
  token <- 'API_TOKEN'
  if(type == 'ACCESS_URL') token <- 'ACCESS_TOKEN'
  request <- paste0(Sys.getenv(type), entity, "?token=", Sys.getenv(token), "&id=", id)
  response <- httr::DELETE(
    url = request
  )
  httr::content(response)
}

filterDDB <- function(entity, q, type = 'API_URL') {
  token <- 'API_TOKEN'
  if(type == 'ACCESS_URL') token <- 'ACCESS_TOKEN'
  request <- paste0(Sys.getenv(type), entity, "?token=", Sys.getenv(token))
  response <- httr::POST(
    url = request,
    encode = 'json',
    body = I(q)
  )
  httr::content(response)
}