library(openxlsx)

#' @author Lluís Cobos Aumatell

#' Función que devuelve un vector con los nombres de las 
#' columnas de un dataframe siguiendo un patrón.
#' Básicamete utiliza grepl para pillar los nombres
#' @param data -> dataframe del que queremos sacar las columnas
#' @param pattern -> patrón de selección
#' @seealso grepl  
get_columns_name_by_contains <- function(data, pattern) {
    col_names <- colnames(data)
    col_grep <- grepl(pattern = pattern, col_names)
    return(col_names[col_grep]) 
}

#' Guarda en un excel una lista de dataframes. Cada dataframe es una
#' hoja de excel. si queremos que cada hoja tenga un nombre diferente
#' escogido por nos, lo más fácil es crear un vector de nombres y
#' pasarlo al <sheet_names>.
#' ej:
#' .... sheet_names = c("Hoja molona", "Hoja10000", "Pepote")
#'
#' @param frames -> lista de dataframes a guardar
#' @param sheet_names -> nombre de las hojas, NULL por defecto
#' @param path -> path del arxivo con el nombre
#' @param extension -> extensión del archivo, ".xlsx" por defecto
save_to_excel <- function(frames, sheet_names = NULL, path, extension = ".xlsx") {
    names <- c()
    to_save <- frames
    for (i in seq_along(frames)) {
        if (is.null(sheet_names) == FALSE && is.null(sheet_names[i]) == FALSE) {
            
            names <- append(names, sheet_names[i])
        } else {
            names <- append(names,  paste("sheet_", i, sep = ""))
        }
    }
    to_save <- setNames(to_save, names)
    write.xlsx(x = to_save, paste(path, extension, sep = ""))
}
