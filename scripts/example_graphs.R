source("scripts/cbs_graphs_utils.R")
source("scripts/cbs_utils.R")

#TODO: Modify
FILE_TO_LOAD <- "data/raw/FILE_NAME.extension"
FOLDER_TO_SAVE <- "outputs/imgs/"

print("hello")
# load dataframe
my_data <- as.data.frame(read.table(FILE_TO_LOAD, sep = "\t", header = TRUE, stringsAsFactors = FALSE))

# get the columns
COLUMN_INIT <- "MAXDPF"
final_cols <- get_column_name_by_contains(my_data, COLUMN_INIT)
print(final_cols)

# vars names
group <- "put_name_here"
x <- "put_name_here"

# Crear una lista con lo que queramos añadir al gráfico.
geom <- list(
    geom_boxplot(aes(colour = GROUP_NAME), colour = c("red", "blue"), fill = c("#dc7d7d75", "#9090e5")),
    labs(
        title = "Amo a ver qué pasa",
        subtitle = "Cobos",
        caption = "Source: data-nomi",
        x = "La cosa esa",
        y = "La otra cosa"
    ),
    theme_classic()
)

save_graphs_from_dataframe(
    data = my_data,
    group = group,
    x_value = x,
    y_list = final_cols,
    geom_type = geom,
    extension = "png",
    path = FOLDER_TO_SAVE
)
