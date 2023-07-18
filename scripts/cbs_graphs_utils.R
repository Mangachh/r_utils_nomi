#' @author Lluís Cobos Aumatell
#' Libreria para guardar varios gráficos en la carpeta seleccionada.

library(ggplot2)


#' Crea un gráfico a partir d los parámetros que pasamos.
#' No es muy diferente a llamar el ggplo, pero en ese caso
#' podemos pasar "strings" sin ningún problema
#' @param data -> dataframe
#' @param x -> el valor de x, puede ser un "string"
#' @param y -> el valor de y, puede ser un "string"
#' @param geom_plot -> el tipo de gráfico, se puede pasar la función
#' [geom_box()] o bien se puede pasar como variable
#' my_geom <- geom_box()
get_plot <- function(data, x, y, group, geom_plot) {
  return(ggplot(
    data,
    aes(group = .data[[group]], x = .data[[x]], y = .data[[y]])
  ) +
    geom_plot)
}

get_plot_hard <- function(data, x, y, group, geom_plot){
  plot <- ggplot(data, aes(
    group = .data[[group]], x = .data[[x]], y = .data[[y]])
  )

  plot <- plot + geom_plot
  return(plot)
}

#' Guarda los gráficos en la ruta seleccionada.
#' La función no es santo de mi devoción ya que me gustaría
#' tener 2: una que hiciese los gráficos y otra que los guardase
#' en el disco duro, aún así no soy capaz de hacer que un vector
#' de ggplots funcione como me guste, crea un montón de cosas
#' y luego no soy capaz de imprimirlos bien. Hay, creo, una
#' solución usando otra libreria, pero tendría que mirarla bien.
#' @param data -> dataframe con los datos
#' @param group -> grupo para el gráfico
#' @param x_value -> el valor del eje x
#' @param y_ist -> lista de valores del eje y. Utilizamos este eje para el buqle
#' @param geom_type -> tipo de geom, de esta manera se pueden crear los gráficos que se
#' quieran con todas las características.
#' se puede pasar la función
#' [geom_box()] o bien se puede pasar como variable
#' my_geom <- geom_box()
#' @param extension -> extension de la imagen
#' @param paht -> ruta de la carpeta. OJU!!!! no es el nombre del archivo, ya que este vendrá con el valor
#' concreto de la iteración del y_list. Por ejemplo, si y_list <- c("Pepote"), el archivo se llamará "Pepote"
save_graphs_from_dataframe <- function(data, group, x_value, y_list, geom_type, extension, path) {
  for (i in seq_along(y_list)) {
    name <- y_list[i]
    print(paste("Creating graph: ", name))
    plot <- get_plot(data = data, group = group, x = x_value, y = name, geom_type)
    ggsave(filename = paste(name, "png", sep = "."), plot = plot, path = path)
  }
}
