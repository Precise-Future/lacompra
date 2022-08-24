newProducto <- function(nombre, precio, uv = NA, precio_x_unidad = NA, usos, supermercado, tags) {
  return(list(
    id = random(), nombre = nombre, uv = uv, precio_x_unidad = precio_x_unidad, usos = usos, supermercado = supermercado, tags = tags
  ))
}

