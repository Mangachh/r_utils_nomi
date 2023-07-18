#' Código para hacer un loop de un modelo de regresión lineal sobre los cluster de los sulcal pits
#' Puede adaptarse para otros tipos de variables

library(readxl)
library(writexl)
source("cbs_linear_utils.R")

# TODO: MODIFY
FILE_TO_READ <- "data/raw/FILE_NAME.extension"
FOLDER_TO_SAVE <- "outputs/linear_regression/"

#Inicio de las variables de interés
dep_var <- "Num"


#parte de la fórmula de lm con las variables indepes tiene que empezar ~ VIs
indep_vars <- "~ FORMULA"


#Abre el archivo y crea un dataframe
mydata <- as.data.frame(read.table(FILE_TO_READ, sep = "\t", header = TRUE, stringsAsFactors = FALSE))

# crea una lista con las VDs
c_names <- colnames(mydata)

## crea una lista con el inicio que nos interesa (dep_var) en formato TRUE FALSE
grepl_names <- grepl(dep_var, c_names)

# Modifica lista inicial de VDs con las variables TRUE (las que nos interesan del paso anterior)
c_names <- c_names[grepl_names]
# print(c_names)

#vector vacíp para introducir pvalues
p_vector <- c()

# Loop

# creamos una lista con los sumarios
summaries <- linear_reg_loop(my_data = mydata, indep_vars = indep_vars, dep_vars = c_names)

# test para ver que funca porque usa symbols y eso
mod <- lm(Num1 ~ Edat + Sexe, data = mydata)
smm <- summary(mod)
capture.output(smm, file=paste(FOLDER_TO_SAVE, "hand.txt", sep=""))
# si queremos el output, descomentar la línea 
# (está bien para comprobar que los datos estén correctos)
save_output(summaries = summaries, path = FOLDER_TO_SAVE)

# pasamos los sumarios a un solo dataframe 
print("hola_1")
sum_df <- summaries_to_dataframe(summaries)
print(typeof(sum_df))
save_to_excel(frames = p <- list(sum_df), path = paste(FOLDER_TO_SAVE, "sumaries.xlsx", sep = ""))
print("hola_2")
# ajustamos la p, tanto en edad como en sexo
p_adjust_df <- summaries_p_adjust(summaries, indep_var = "Sexe", value = "Pr(>|t|)", method = "bonferroni")
p_edat_df <- summaries_p_adjust(summaries, indep_var = "Edat", value = "Pr(>|t|)", method = "bonferroni")

# lista con los dataframes que queremos mostrar en el excel
pepote <- list(sum_df, p_adjust_df, p_edat_df)

# los guarda con nombres en la sheet
save_to_excel(pepote, sheet_names = c("Coefficients", "P sexe", "P edat"), path = paste(FOLDER_TO_SAVE, "test_ex.xlsx", sep=""))

# los guarda sin nombres en las sheets
# save_to_excel(pepote, path = paste(FOLDER_TO_SAVE, "test_ex.xlsx", sep=""))

