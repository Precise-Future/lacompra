newProducto <- function(nombre, precio, uv = NA, precio_x_unidad = NA, usos, supermercado, tags) {
  return(list(
    id = random(), nombre = nombre, precio = precio, uv = uv, precio_x_unidad = precio_x_unidad, usos = usos, supermercado = supermercado, tags = tags
  ))
}

createProducto <- function(newProducto) {
  putDDB("producto", newProducto)
  return(newProducto$id)
}

readProducto <- function(id) {
  getDDB("producto", id)
}

updateProducto <- function(id, key, value) {
  x <- readProducto(id)$Item
  x[[key]] <- value
  putDDB("producto", x)
}

deleteProducto <- function(id) {
  deleteDDB("producto", id)
}

listProducto <- function() {
  listDDB("producto")
}

findProducto <- function(q) {
  filterDDB("producto", q)
}
