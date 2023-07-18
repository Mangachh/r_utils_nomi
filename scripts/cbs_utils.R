#' @author Lluís Cobos Aumatell

#' Función que devuelve un vector con los nombres de las 
#' columnas de un dataframe siguiendo un patrón.
#' Básicamete utiliza grepl para pillar los nombres
#' @param data -> dataframe del que queremos sacar las columnas
#' @param pattern -> patrón de selección
#' @seealso grepl  
get_column_name_by_contains <- function(data, pattern){
    col_names <- colnames(data)
    col_grep <- grepl(pattern = pattern, col_names)
    return(col_names[col_grep]) 
}