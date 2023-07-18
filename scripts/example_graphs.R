source("scripts/graphs.R")
source("scripts/utils.R")

library(tidyverse)
library(hrbrthemes)
library(viridis)

FILE_TO_LOAD <- "data/raw/RH_depth_80.txt"
FOLDER_TO_SAVE <- "C:/nomir/pits_project/outputs/imgs/"

print("hello")
# load dataframe
my_data <- as.data.frame(read.table(FILE_TO_LOAD, sep = "\t", header = TRUE, stringsAsFactors = FALSE))

# get the columns
COLUMN_INIT <- "MAXDPF"
final_cols <- get_column_name_by_contains(my_data, COLUMN_INIT)
print(final_cols)

# get the graphs
group <- "Sexe"
x <- "Sexe"
# TODO: ver qué se puede hacer aquí...

# Crear una lista con lo que queramos añadir al gráfico.
geom <- list(
    geom_boxplot(),
    geom_boxplot(aes(colour = Sexe), colour= c("red","blue")),
    labs(
        title = "Amo a ver qué pasa",
        subtitle = "Cobos",
        caption = "Source: data-nomi",
        x = "La cosa esa",
        y = "La otra cosa"
    ),
    theme_classic()
)

# a mano para comprobar que son iguales
p <- ggplot(my_data, aes(group = Sexe, x = Sexe, y = MAXDPF1)) +
    geom_boxplot(aes(colour = Sexe)) +
    labs(
        title = "Amo a ver qué pasa",
        subtitle = "Cobos",
        caption = "Source: data-nomi",
        x = "La cosa esa",
        y = "La otra cosa"
    )+
    theme_classic()
print(p)


save_graphs_from_dataframe(
    data = my_data,
    group = group,
    x_value = x,
    y_list = final_cols,
    geom_type = geom,
    extension = "png",
    path = FOLDER_TO_SAVE
)
