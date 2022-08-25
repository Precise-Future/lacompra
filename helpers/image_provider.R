#### requiere la librería magick en ubuntu para poder renderizar la imagen desde el raw que se recibe de S3
imageDrawer <- function(img, height = '150px', class = '') {
  out <- tryCatch({
    ### detectar desde donde se carga la imagen. Las Opciones son: S3, servidor, url. Se identifica por el comienzo de la url
    if(str_starts(img, 'imagenes/')) {
      ### Proveedor = S3
      imgFromS3(img, height, class)
    } else {
      if(str_starts(img, 'img') |str_starts(img, 'http')) {
        ### Proveedor = Servidor o URL
        renderUI({HTML(paste0('<img src = "',img, '" class = "', class, '">'))})
      }
    }
  }, error = function(e) {
    print(e)
    renderUI({HTML(paste0('<img src = "https://media.cleanknightsrd.com/img/no-image.png" class = "', class, '">'))})
  })
  return(out)
}


imgFromS3 <- function(img, height = '150px', class = '') {
  ext <- unlist(str_extract(img, '.png|.jpeg|.jpg'))[1]
  if(localVersion) {
    gotten <- read_file_raw(paste0(mainDir, img))
  } else {
    gotten <- get_object(object = img, bucket = bucket, as = 'raw')
  }
  renderImage({
    tmpfile <- image_background(image_read(gotten, strip = T), color = 'none') %>% 
      image_write(tempfile(fileext=ext), format = str_remove_all(ext, '[:punct:]'), quality = 50, depth = 8, flatten = T)
    list(src = tmpfile, height = height, contentType = paste0("image/", ext), width = 'auto', className = class)
  }, deleteFile = T, outputArgs = list(width = 'auto', height = '100%', inline = T))
}
whichImage <- function(img) {
  tryCatch({
    if(str_starts(img, 'imagenes/')) {
      return(NULL)
    } else {
      return(img)
    }
  }, error = function(e) {
    return('https://media.cleanknightsrd.com/img/no-img4service.jpg')
  })
}
s3Image <- function(img) {
  tryCatch({
    if(str_starts(img, 'imagenes/')) {
      img <- imgFromS3(img, 'auto')
      return(img)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(renderImage({list(src='https://media.cleanknightsrd.com/img/no-img4service.jpg')}, deleteFile = T))
  })
}

tryIcon <- function(icon) {
  # if(localVersion) return(icon(name = 'remove-circle', lib = 'glyphicon'))
  out <- tryCatch({
    icon(icon)
  }, error = function(e) {
    return(icon(name = 'remove-circle', lib = 'glyphicon'))
  })
  out
}

uploadImage <- function(imagen) {
  if(localVersion) {
    file.copy(from = imagen$datapath, to = paste0(mainDir, 'imagenes/', imagen$name))
  } else {
    put_object(file = imagen$datapath, object = paste0('imagenes/', imagen$name), bucket = bucket)
  }
  return(paste0('imagenes/', str_replace_all(imagen$name, ' ', '+')))
}
