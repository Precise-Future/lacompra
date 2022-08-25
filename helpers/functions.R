random <- function() {
  sha1(paste0(collapse = '', c(sample(x = c(0:9), 5), now())))
}

softRandom <- function() {
  paste0(collapse = '', c(sample(x = LETTERS, 3), str_remove_all(str_split(now(), ' ')[[1]][2], ':')))
}

make_identity <- function(email, pass) {
  return(hmac(object = paste0(email, pass), key = paste0(collapse = '\n'), algo = 'sha256', serialize = F, raw = F))
}

init <- function(input, output, session) {
  path <- checkMyStack()
  aws <- fromJSON(paste0(path, "credentials.json"))
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = as.character(aws$bucket$accesskey),
    "AWS_SECRET_ACCESS_KEY" = as.character(aws$bucket$secretkey),
    "AWS_DEFAULT_REGION" = as.character(aws$bucket$region),
    "AWS_BUCKET_NAME" = as.character(aws$bucket$name),
    "API_URL" = as.character(aws$api$url),
    "API_TOKEN" = as.character(aws$api$token),
    "ACCESS_URL" = as.character(aws$access$url),
    "ACCESS_TOKEN" = as.character(aws$access$token),
    "APP_NAME" = as.character(aws$application$name),
    "APP_URL" = as.character(aws$application$url)
  )
}

sessionCheck <- function(input, output, session) {
  observeEvent(input$cookie, {
    c <- input$cookie
    if(c != "") {
      c <- substr(c, as.numeric(str_locate(c, 'pfSession=')[1, 2] + 1), str_length(c))
      print(c)      
      s <- readSession(c)$Item
      u <- readUsuario(s$uId)$Item
      print(u$nombre)
      if(!is.null(u)) {session$userData$usuario(u)}
      delay(3500, {runjs('Shiny.setInputValue("page", "home");')})
    } else {
      delay(3500, {runjs('Shiny.setInputValue("page", "login");')})
    }
  })
}

nuevaCuenta <- function(email, pass, nombre, apellidos, rol, afiliador = "") {
  status <- 0
  x <- make_identity(email, pass)
  touch <- readIdentity(x)$Item
  
  if(is.null(touch$account)) {
    uId <- createUsuario(newUsuario(nombre = nombre, apellidos = apellidos, email = email, rol = rol))
    account <- softRandom()
    iId <- createIdentity(newIdentity(id = x, uId = uId, account = account))
    if(afiliador != "") {
      a <- readUsuario(afiliador)$Item
      if(is.null(a$id)) {aId = "not found"} else {aId <- createAfiliado(newAfiliado(afiliador = afiliador, afiliado = uId))}
    } else {aId <- NULL}
    data <- list(usuario = uId)
  } else {
    status <- 1
    if(touch$account == 0) {data <- list(usuario = 'verificar')} else {data <- list(usuario = 'old')}
  }
  
  return(list(status, data))
}


checkMyStack <- function() {
  if(dir.exists('../aws/')) {
    return('../aws/')
  } else {
    if(dir.exists('/home/ubuntu/')) {
      return('/home/ubuntu/')
    } else {
      return('unknown')
    }
  }
}


findEntityIdByName <- function(name, entity, nomCol = 'nombre', exact = F) {
  entity$id[str_detect(tolower(entity[, nomCol]), tolower(name))]
}

createSelectInputChoices <- function(entidad, nomCol, valCol, firstEmpty = T) {
  out <- lapply(1:nrow(entidad), function(i) {unlist(entidad[i, valCol])})
  names(out) <- lapply(1:nrow(entidad), function(i) {as.character(unlist(entidad[i, nomCol]))})
  if(firstEmpty) out <- c(list('Seleccionar' = 0), out)
  return(out)
}

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(pattern = id, x = names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}
clear <- function(x) {
  return(
    str_remove_all(chartr("áéíóúñ", "aeioun", tolower(x)), "[:punct:]")
  )
}
superClear <- function(x) {
  return(
    str_replace_all(clear(x), ' |[:punct:]|\\+|\\-', '')
  )
}
format_latin_money <- function(x) {
  formatC(x, format = "d", digits = 0,  big.mark = ",", decimal.mark = ".")
}
secuenciaAleatoria <- function() {
  id <- now()
  momento <- unlist(str_split(id, " "))
  id <- unlist(lapply(runif(3, min = 1, max = 26), function(x) {round(x, 0)}))
  id <- str_flatten(LETTERS[id], collapse = "")
  fecha <- momento[1]
  hora <- momento[2]
  id <- str_flatten(paste0(id, str_remove_all(hora, "[:punct:]")))
  return(id)
}

toPascalCase <- function(x) {
  str_remove_all(str_to_title(x), ' |[:punct:]')
}

toSnakeCase <- function(x) {
  str_replace_all(tolower(x), ' ', '_')
}

toCamelCase <- function(x) {
  x <- toPascalCase(x)
  str_replace(x, substr(x, 1, 1), tolower(substr(x, 1, 1)))
}

secure_error_handle <- function(what) {
  response <- tryCatch({
    list(
      status = 0,
      data = what
    )
  }, error = function(e) {
    print(e)
    list(
      status = 1,
      data = e
    )
  })
}

idsToName <- function(ids, entidad) {
  return(
    unlist(
      Map(function(ids) {
        entidad$nombre[as.numeric(entidad$id) == as.numeric(ids)]
      }, ids)
    )
  )
}
# Esoterismo (del griego ἐσώτερος [API /e'so:teros/]: «de dentro, interior, íntimo»; unido al sufijo «–ismo») es un término genérico
# usado para referirse al conjunto de conocimientos, doctrinas, enseñanzas, prácticas, ritos, técnicas o tradiciones de una corriente
# de pensamiento que utiliza secretos, símbolos incomprensibles o de difícil acceso y que se transmiten únicamente a una minoría selecta
# denominada iniciados, por lo que no son conocidos por los profanos.
from_esoteric_to_number <- function(x = 'bono Gigas 12') {
  if(is.null(x)) return(0)
  if(str_length(x) == 0) return(0)
  tryCatch({
    if(str_length(parse_number(x)) > 2) {
      if(str_detect(substr(x, start = (str_length(x)-2), stop = (str_length(x)-2)), '[:punct:]')) {
        numero <- parse_number(
          paste0(c(
            as.numeric(paste0(str_extract_all(substr(x, start = 0, stop = (str_length(x)-2)), '[:number:]', simplify = T), collapse = '')),
            as.numeric(paste0(str_extract_all(substr(x, start = (str_length(x)-2), stop = str_length(x)), '[:number:]', simplify = T), collapse = ''))
          ), collapse = '.')
        )
        return(numero)
      } else {
        return(
          parse_number(paste0(str_extract_all(x, '[:number:]', simplify = T), collapse = ''))
        )
      }
    } else {
      return(parse_number(x))
    }
  }, error = function(e) {
    print(e)
    x
  })
}

inMaintenance <- function() {
  if(localVersion) {
    as.logical(as.numeric(readLines(paste0(mainDir, 'maintenance-mode.txt'))))
  } else {
    as.logical(as.numeric(s3read_using(readLines, bucket = bucket, object = 'maintenance-mode.txt')))
  }
}
entityList2DF <- function(entity) {
  do.call(rbind, lapply(entity, list2DF))
}
removeNullFromNamedList <- function(x) {
  x[lengths(x) != 0]
}
nameEntityfromId <- function(entity, id) {
  entity$nombre[entity$id == id]
}
orBlank <- function(x) {
  if(is.null(x)) '' else if(is.na(x)) '' else if(length(x) == 0) '' else  x
}
