isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

isValidPass <- function(x) {
  (str_length(x) > 5) & (!str_detect(x, '\"\'|='))
}

rutValido <- function(rut = '8924907-4') {
  tryCatch({
    if(str_length(rut) > 5) {
      splitted <- str_extract_all(str_remove_all(rut, '[:punct:]'), pattern = '[:alnum:]')[[1]]
      numero <- as.numeric(splitted[-length(splitted)])
      numero <- numero[length(numero):1]
      digitoVerificador <- 11 - sum(numero * c(2:7, 2:7)[1:length(numero)]) %% 11
      tolower(splitted[length(splitted)]) == ifelse(digitoVerificador == 11, "0", ifelse(digitoVerificador == 10, "k", as.character(digitoVerificador)))
    } else {F}
  }, error = function(e) {
    print(e)
    F
  })
}

validar <- function(dato, atributo, validations) {
  go <- which(lapply(validations, length) == 1)
  valid <- T
  i <- 1
  txt <- ''
  while((txt == '') & (i <= length(go))) {
    val <- validations[[go[i]]]
    switch (val,
            'noNull' = if(is.null(dato)) txt <- paste0('El campo ', atributo, ' no puede ser nulo'),
            'numeric' = if(is.na(as.numeric(dato))) txt <- paste0('El campo ', atributo, ' solo admite valores numéricos'),
            'noEmpty' = if(dato == '') txt <- paste0('El campo ', atributo, ' no puede estar vacío'),
            'inHere' = if(!(dato %in% validations$here)) txt <- paste0('El campo ', atributo, ' tiene un valor no permitido'),
            'email' = if(!isValidEmail(dato)) txt <- paste0('El campo ', atributo, ' debe ser un email válido'),
            'pass' = if(!isValidPass(dato)) txt <- 'La contraseña debe tener al menos 5 caracteres y no puede contener los siguientes (\', \", =)',
            'rut' = if(!rutValido(dato)) txt <- 'El rut no es válido',
            'dependence' = {
              if(validations$dependenceList$who) {
                if(validations$dependenceList$req == 'up') 
                  if(dato < validations$dependenceList$value) 
                    txt <- paste0('El campo ', atributo, ' debe ser mayor que ', validations$dependenceList$value)
                if(validations$dependenceList$req == 'down') 
                  if(dato > validations$dependenceList$value) 
                    txt <- paste0('El campo ', atributo, ' debe ser menor que ', validations$dependenceList$value)
                if(validations$dependenceList$req == 'equal') 
                  if(dato != validations$dependenceList$value) 
                    txt <- paste0('El campo ', atributo, ' debe ser igual a ', validations$dependenceList$value)
              }
            }
    )
    i <- i + 1
  }
  return(txt)
}
