source("scripts/cbs_utils.R")

#################################
#                               #
#       Start settings          #
#                               #
#################################

# el path del archivo
FILE_PATH <- "data/raw/LH_maxdepth_80.txt"

# columnas que nos interesa mirar
COLUMN_START <- "MAXDPF"

# aquí las columnas que vamos a quitar ya que no nos interesan
COLUMN_TO_DELETE_START <- "maxDPF"

# esto es para la función apply, 1 indica que mira por fila, 2 que mira por columna
MARGIN_ROW <- 1
MARGIN_COL <- 2

# el valor con el que comparamos, no inclusive 
MINUS_THAN <- 0

# cargamos dataframe
pits_df <- as.data.frame(read.table(FILE_PATH,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE
))

# pillamos las columnas, usamos mi función custom
cols_to_check <- get_columns_name_by_contains(pits_df, COLUMN_START)

# limpiamos el dataframe con las maxDPF en minuscula, trabajaremos con este ahora
# pillamos los nombres
cols_to_erase <- get_columns_name_by_contains(pits_df, COLUMN_TO_DELETE_START)
pits_df_clean <- pits_df[, !(names(pits_df) %in% cols_to_erase)]
remove(cols_to_erase)


#################################
#                               #
#       Conteo de negativos     #
#                               #
#################################

# Primero miraremos por cada columna cúantos negativos tiene.
# Hay dos operaciones: 
#   * una que da todas las columnas y si esta no tiene
#       negativo entonces pone 0
#   * otra que sólo muestra las columnas con negativo


# miramos los índeces que tengan un negativo
# por cada columna nos da los índices son
# en plan diccionario (p.e: MAXDPF6: [5, 8, 12])
# eso implica que las filas de MAXDPF6 tienen número negativo y son tres.
col_indices_neg <- apply(pits_df_clean[cols_to_check], MARGIN = MARGIN_COL, function(x) which(x < MINUS_THAN))

# vale, utilizamos sapply para pillar la length que tiene cada índice
sulcal_count <- sapply(col_indices_neg, length)

# pasamos a dataframe, usamos un trasnpose para que quede mejor
# +-------+--------+--------+
# |maxdp1 | maxdp2 | maxdfp |
# +-------+--------+--------+
# |  5    |    2   |   45   |
# +-------+--------+--------+
sulcal_count <- t(as.data.frame(sulcal_count))
rownames(sulcal_count) <- c("count")

# esta se la misma lista sin los 0's, hacemos también transpose
sulcal_count_no_zero <- t(as.data.frame(
    sulcal_count[, !colSums(sulcal_count == 0)],
))


temp_list <- apply(sulcal_count, MARGIN_COL, function(x) {
    return((x / nrow(pits_df_clean) * 100))
})

sulcal_count <- rbind(sulcal_count, temp_list)
rownames(sulcal_count) <- c("count", "perc")

temp_list <- c()
temp_list <- apply(sulcal_count_no_zero, MARGIN_COL, function(x) {
    return((x / nrow(pits_df_clean) * 100))
})

sulcal_count_no_zero <- rbind(sulcal_count_no_zero, temp_list)
rownames(sulcal_count_no_zero) <- c("count", "perc")

#########################################
#                                       #
#       Muestra las filas con pits      #
#       negativos y elimina los que     #
#       no son negativos                #
#                                       #
#########################################


# vale, a partir de aquí quitaremos las row que no tengan ningún valor negativo
# podríamos hacerlo de otra manera, 
# pero no está de más hacer algún bucle que otro y es más rápido para mí.
# de esta manera nos aseguramos de tener índices únicos

# guardamos los índices únicos
unique_indeces <- c()

# por cada item de la lista
for(item in col_indices_neg){
    # si la length es mayor que 0
    if(length(item) > 0){
        # miramos qué tiene dentro
        for(index in item){
            # si nuestro vector `unique_indices` no contiene el índice, añadimos
            if(index %in% unique_indeces == FALSE){
                unique_indeces <- append(unique_indeces, index)
            }
        }
    }
}

# quitamos las rows que no tienen negativos
negative_pits <- pits_df_clean[unique_indeces, ]

# hora los números igual o mayor de 0, a NAN, remplazamoss. Esto es una paja de R, no se entiende ná de ná
negative_pits[, cols_to_check][negative_pits[, cols_to_check] >= 0] <- NaN
# ponemos el conteo
negative_pits$negative_count <- rowSums(!is.na(negative_pits[cols_to_check]))
# ordenamos ya que esto lo ha desordenado
negative_pits <- negative_pits[order(as.numeric(row.names(negative_pits))), ]

# añadimos conteo a nuevo dataframe final
pits_df_final <- pits_df_clean
pits_df_final$negative_count <- rowSums(!is.na(pits_df_final[cols_to_check]) & pits_df[cols_to_check] < 0)

#########################################
#                                       #
#       Save The Dataframe              #
#                                       #
#########################################

# vamos a guardar los dataframes. Usaremos cbs_utils
# pits_df -> dataframe recién cargado.
#            no se usa más que para pillar datos
#
# pits_df_clean -> dataframe sin columnas maxDPF
#                  lo usamos para los siguientes dataframes
#
# pits_df_final -> dataframe con las columnas maxDPF quitadas
#                  al final tiene conteo de negativos
#
# negative_pits -> dataframe con las columnas maxDPF quitadas
#                  filas que no contienen negativos quitadas
#                  si un valor NO es negativo, es NaN
#                  final tiene conteo de negativos
#
# sulcal_count -> dataframe que por cada sulcal (MAXDPF) tiene la cuenta de negativos
#                 si el conteo es 0, aparece
#
# sulcal_count_no_zero -> dataframe que por cada sulcal (MAXDPF) tiene la cuenta de negativos
#                         si el conteo es 0, no aparece el sulcal

# TODO: poner arriba
PATH <- "outputs/negative_pits/LH_negative_pits"


df_list <- list(pits_df_final, negative_pits, sulcal_count, sulcal_count_no_zero)
sheet_names <- c("Full Pits Clean", "Negative Pits", "Sulcal Count", "Sulcal Count not 0")


save_to_excel(frames = df_list, sheet_names = sheet_names, path = PATH)
