#' @author Lluís Cobos Aumatell
#' Libreria que facilita la creación de diferentes
#' <summary> con diferentes variables, los junta
#' todos en un mismo dataframe (si queremos)
#' y los guarda en un excel.
#' TODO: mirar que los nombres de las variables
#' sean correctos y self-explanatory

# libreria para guardar en excel
library(writexl)


#' Hace una regresión lineal a partir de un data_frame,
#' una var dependiente y las independientes. Además
#' el summary tiene una nueva variable llamada <dep_var>
#' donde se almacena el nombre de esta.
#' Así, llamando <summary$dep_var> nos devolverá la
#' variable.
#'
#' @param my_data -> el dataframe a usar
#' @param dep_var -> la variable dependiente
#' @param indep_vars -> variables independientes.
#' Se usa para hacer la fórmula así que deben estar, p.e en "~ VAR1 + VAR2"
#' @return -> sumario
linear_regression <- function(my_data, dep_var, indep_vars) {
    formula <- paste(dep_var, indep_vars)
    model <- lm(formula, data = my_data)
    my_summary <- summary(model)
    my_summary$dep_var <- dep_var
    return(my_summary)
}

#' Hace una regresión lineal con un vector de variables dependientes.
#' La función se fia de que los datos son correctos
#' (posibles correcciones si se testea).
#' Permite hacer regresiones a todo el vector o a una parte de él
#' cambiando start y end.
#' Utiliza la función <linear_regression> de este mismo archivo,
#' por lo tanto, cada <summary> tiene su variable dependiente
#' en <summary$dep_var>
#'
#' @param my_data -> dataframe que se usa
#' @param indep_vars -> variables independientes. Usar "~ VAR1 + VAR2"
#' @param dep_vars -> vector con los nombres de las variables
#' dependientes en el dataframe
#' @param start -> 1 por default, cambiar si se quiere empezar por otro índice de la lista
#' @param end -> -1 por defecto, indica que hará toda la lista. Si queremos hacer un número menor
#' cambiarlo.
#' @return -> retorna una lista de summary, para acceder a cada uno summaries[[X]]
linear_reg_loop <- function(my_data, indep_vars, dep_vars, start = 1, end = -1) {
    if (end == -1) {
        end <- length(dep_vars)
    }

    summaries <- list()
    for (i in start:end) {
        dep_var <- dep_vars[i]
        summary <- linear_regression(my_data = my_data, dep_var, indep_vars)
        summaries[[i]] <- summary
    }

    return(summaries)
}

#' Función creada para guardar los sumarios de la función linear_reg_loop
#' aunque realmente podría guardar cualquier lista (probarlo)
#' Cada <summary> de la lista de <summaries> se guarda en un archivo
#' diferente de texto. Usado para echar un ojo y ver que
#' las funciones están bien.
#'
#' @param summaries -> lista de sumarios a guardar
#' @param path -> ruta de la carpeta donde queremos guardar los arxivos
#' @param prefix -> prefijo de los archivos, "summary_" por defecto.
#' @param extension -> extensión del archivo, ".txt" por defecto.
#' **OJU!!!!** si se quiere cambiar de extensión, hay que poner el punto (.txt)
save_output <- function(summaries, path = NA, prefix = "summary_", extension = ".txt") {
    if (is.na(path)) {
        warning("No hay ruta de archivo")
        return()
    } else if (is.null(summaries)) {
        warning("No hay lista de summarios")
        return()
    }

    for (i in seq_along(summaries)) {
        summary <- summaries[[i]]
        file_name <- paste(path, prefix, summary$dep_var, extension, sep = "")
        capture.output(summary, file = file_name)
    }
}

#' A partir de una lista de sumarios, creamos un dataframe con los
#' <p_values> ajustados según el <method> escogido. Estos <method> son
#' los que utiliza R por defecto ("fdr", "bonferroni", etc).
#' Al devolver un dataframe es más fácil operar con él.
#' @param summaries -> lista de sumarios donde queremos hacer la corección
#' @param indep_var -> nombre la variable indep. De momento sólo acepta uno
#' @param value -> Este es el nombre de la columna donde se haya la p.
#' Al hacer un print<(summary$coefficients)>, veremos diferentes
#' columnas. Hay que poner el nombre de la que nos interesa tal como
#' sale en ese print.
#' O bien podemos usar colnames(summary$coefficients) para ver
#' el nombre de las columnas. ("Pr(>|t|)") -> algo así tiene que salir
#' @param method -> el método de la corrección. "fdr", "bonferroni", etc...
#' @return -> un dataframe con las p corregidas
summaries_p_adjust <- function(summaries, indep_var, value, method) {
    # creamos columnas como vectores, más fácil de añadir luego en el dataframe
    cluster_vec <- c()
    def_p <- c()
    p_to_adjust <- c()
    print(indep_var)
    print(value)
    for (i in summaries) {
        cluster_vec <- append(cluster_vec, i$dep_var)
        coef_sum <- coef(i)
        p_to_calculate <- coef_sum[indep_var, value]
        def_p <- append(def_p, p_to_calculate)
        p_to_adjust <- append(p_to_adjust, p_to_calculate)
    }

    # austamos las p según el vector que hemos creado y el método
    p_adjusted <- p.adjust(p_to_adjust, method = method)

    # creamos el dataframe
    p_adjust_df <- data.frame(cluster_vec, def_p, p_adjusted)

    # cambiamos el nombre de las columnas
    # TODO: dejar que se pueda poner como input
    colnames(p_adjust_df) <- c("Cluster", indep_var, method)
    return(p_adjust_df)
}


#' Guarda la lista de <summaries> en un dataframe.
#' Así se puede operar con facilidad con ellas y
#' además es el paso para exportarlos a excel.
#'
#' Hay ciertas 'ñapas' para que funcione bien y sea
#' más fácil. Por ejemplo, la variable dependiente
#' se repite en todas las columnas necesarias para rellenar
#' los huecos basados en el <summary$coefficients>
#' ej:
#'    +-------+-------+----------+-------+
#'    |Vardep | Invar | Estimate |  P    |
#'    +-------+-------+----------+-------+
#'    |Num_1  | Sexe  |  0.5555  |  2.4  | -> esto es la variable 1
#'    |Num_1  | Edat  |  0.7777  |  0.1  |
#'    +-------+-------+----------+-------+
#'    |Num_2  | Sexe  |  0.222   |  0.3  | -> esto es la variable 2
#'    |Num_2  | Edat  |  0.21111 |  0.47 |
#'    +-------+-------+----------+-------+
#'
#' TODO: crear un vector para seleccionar los nombres de las columnas
#' Vardep e Invar ???
#'
#' @param summaries -> listas de sumarios que convertir en un dataframe
#' @return -> un dataframe con todos los sumarios
summaries_to_dataframe <- function(summaries) {
    # dataframe donde guardamos los sumarios
    sums_df <- data.frame(row.names = NULL)

    # creamos el dataframe final
    # vectores como columnas
    final_df <- data.frame()
    dep_vars_names <- c()
    in_var_column <- c()

    for (i in summaries) {
        # como coefficients tiene, de momento, el mayor número de filas
        # las calculamos. TODO: ponerlo en otro lado, no hace falta que sea
        # a cada línea
        coef_rows <- nrow(i$coefficients)

        # repetimos el nombre de la dep_var tantas veces como filas tenga el coef_rows
        dep_vars_names <- append(dep_vars_names, rep(i$dep_var, times = coef_rows))

        # metemos el sumario en el df
        sums_df <- rbind(sums_df, i$coefficients)
        # ponemos las columnas
        in_var_column <- append(in_var_column, rownames(i$coefficients))
    }
    # metemos todo en el dataframe
    final_df <- data.frame(VAR_DEPS = dep_vars_names, IN_VAR = in_var_column, sums_df, row.names = NULL)

    return(final_df)
}

#' Guarda en un excel una lista de dataframes. Cada dataframe es una
#' hoja de excel. si queremos que cada hoa tenga un nombre diferente
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
    to_save <- list()

    for (i in seq_along(frames)) {
        if (is.null(sheet_names) == FALSE && is.null(sheet_names[i]) == FALSE) {
            to_save <- append(to_save, setNames(frames[i], sheet_names[i]))
        } else {
            to_save <- append(to_save, frames[i])
        }
    }

    # TODO: path + extension!!!
    write_xlsx(to_save, path)
}