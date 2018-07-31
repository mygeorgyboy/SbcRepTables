library(tidyverse)
library(knitr)
library(plotly)
library(kableExtra)

# Report settings
chart_alpha <- 0.5
chart_colors <- function(){
    scale_fill_brewer(type = "qual", palette = "Set1")
}

#' Agrega un renglon con totales a un dataframe
#' Las columnas no numericas contendran un caracter de longitud 0 o el texto del parametro sumLabel
sbc_add_col_sum <- function(x,sumLabel = ""){
     row <-  sbc_get_col_sum_row(x,sumLabel)
     return (rbind(x,row))
}

#' Agrega un renglon con totales a un dataframe
#' Las columnas no numericas contendran un caracter de longitud 0 o el texto del parametro charColText
sbc_add_row_sum <- function(x,sumLabel = "total"){
    col_vec <-  sbc_get_row_sum_vector(x)
    x[[sumLabel]] <- col_vec
    return (x)
}


#' La función recibe una columna de un dataframe.
#' Solo regresa un numero o text, no aplica para dataframes
#' 1) si la columna es numerica regresa la suma
#' 2) si la columna es alfanumerica regresa un caracter de longitud 0 o lo que vega en el parametro charColText
#' Normalmente se llama desde la función 'sbc_sum_columns' para agregar un renglon con totales por columna
sbc_get_col_sum <- function(x,sumLabel = ""){
    if(class(x) =="character"){
            return (as.character(sumLabel) );
    } else if(class(x) == "numeric" || class(x) == "integer"){
        return(sum(x,na.rm = TRUE))
    }

    return(x)
}
#' get a row with sum of columns
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param x dataset containig the data to report (\code{"hsv"} or \code{"cluster"})
#' @param sumLabel label of the row sum, if ommited it is an 0 length string
#'
#' @return Dataframe with a row of column sumns
#'
#' @examples
#' plot_crayons()
#'
#' @export
sbc_get_col_sum_row <- function(x,sumLabel = ""){
    return(as_data_frame(map(x,sbc_get_col_sum,sumLabel)))
}

# Esta función es un wraper de knitr::kable
# Agrega la siguente funcionalidad:
# 1) Los números los muestra con separador de miles
# 2) Agrega estilo (Bold) al ultimo renglon si el parametro hasColTotals = TRUE (para mostrar totales)
sbc_kable <- function(x,..., hasColTotals = FALSE,hasRowTotals = FALSE,format.args = NULL) {
    tab <- knitr::kable(x,..., format.args = list(decimal.mark = '.', big.mark = ","))
     if(hasColTotals){
         tab <-tab %>% row_spec(nrow(x), bold = T) %>%  kable("html", escape = F)
     }
    if(hasRowTotals){
        tab <-tab %>% column_spec(ncol(x), bold = T) %>%  kable("html", escape = F)
    }
    return(tab)
}

# Crea una tabla que cuenta el número de casos
# La tabla de salida contiene un renglon para cada valor de la columna 'rowVar'
# La tabla de salida contiene una culumna para cada valor de la variable 'colVar'
# Se pueden cambiar los titulos de las columnas de salida nameRows , nameCols
# El texto de 'total' se puede cambiar con la variablt nameTotal
# Ejemplo: sbc_2_var_table_count(cadenas,"formato","estado",nameRows ="Formatos mencionados", nameTotal = "Total de casos")
sbc_table_2_var_count <-
    function(x,
             rowVar,
             colVar
           ) {
        w <- group_by_(x, colVar, rowVar) %>%
            summarize(nameCount = n())

        names(w) <- c(colVar, rowVar, "count")
        w <- spread_(w, colVar, "count")
        w <- w %>% mutate_at(c(2:length(w)), funs(replace(., is.na(.), 0)))

        return(w)
    }

# Similar a sbc_table_2_var_count, pero regresa porcentajes en lugar de cueta
# 1) MARGIN = 1 (default)
#    Los renglones suman 100
#    Ejemplo: sbc_table_2_var_perc(encuestas,"nse","genero",MARGIN = 1) %>% sbc_add_row_sum()
# 2) MARGIN = 2
#    Las columnas suman 100
#    Ejemplo: sbc_table_2_var_perc(encuestas,"nse","genero",MARGIN = 2) %>% sbc_add_col_sum()
# 2) MARGIN = 3
#    Las columnas mas los rengloes suman 100
#    Ejemplo: sbc_table_2_var_perc(encuestas,"nse","genero",MARGIN = 3) %>% sbc_add_col_sum() %>% sbc_add_row_sum()
sbc_table_2_var_perc <- function(x, rowVar, colVar,MARGIN =1) {
    w <- sbc_table_2_var_count(x,rowVar,colVar)
    sbc_table_to_perc(w,MARGIN = MARGIN)
    return(w)
}


# Regresa una tabla con el porcentaje para cada valor de la variable pasada como parametro
# Se puede usuar showCount = FALSE para mostrar la cuenta y el porcentaje
# A tibble: 2 x 3
#   genero     n perc
#   <chr>  <int>      <dbl>
# 1 Hombre   839      26.2
# 2 Mujer   2361      73.8
sbc_table_1_var_perc <- function(x, col_name,showCount = FALSE) {
    w <- count_(x,col_name)
     w <- w %>% mutate(perc = n / sum(w$n) *100)
    if(showCount){
        return(w)
    }else{
        return( w[,c(1,3)])
    }
}

# Igual que sbc_table_1_var_perc
# Regresa proporciones en lugar de porcentajes
sbc_table_1_var_prop <- function(x, col_name,showCount = FALSE) {
    w <- count_(x,col_name)
    w <- w %>% mutate(proportion = n / sum(w$n))
    if(showCount){
        return(w)
    }else{
        return( w[,c(1,3)])
    }
}

# Igual que sbc_table_1_var_perc
# Regresa proporciones en lugar de porcentajes
sbc_table_1_var_count <- function(x, col_name,showCount = FALSE) {
  w <- count_(x,col_name)
  w <- w %>% mutate(count = n)
  if(showCount){
    return(w)
  }else{
    return( w[,c(1,3)])
  }
}

# Recibe un dataframe y regresa un vector que contiene la suma de todas las columnas numericas en cada renglon
# La función fue creada para agregar una columna de TOTAL al funal de cuadro generado por sbc_2_var_table_count
# Sin embargo es una función generica y se puede utilizar siempre que se requiera sumar columnas
# El vector de resultado puede agregarse facilmente al dataframe mediante df$new_col <- vector_resultado
sbc_get_row_sum_vector <- function(x){
    tmp_x <- x %>% select_if(is.numeric)
    sum_vector <- vector(mode="numeric", length=nrow(tmp_x))
    for(i in 1:nrow(tmp_x)) {
        row <- tmp_x[i,]
        sum_row <- 0
        for(j in seq_along(row)){
           if(!is.na(row[[j]])){
             sum_row <- sum_row +row[[j]]
           }
        }
        sum_vector[i] <- sum_row
    }
    return(sum_vector)
}

# Regresa una lista de porcentajes de la variable targetVar expicada por la variable byVar
# Por ejemplo si queremos ver el GENERO graficado por CIUDAD
# sbc_list_2_var_perc(conv17_rnd,"GENERO","CIUDAD")
sbc_list_2_var_perc <- function(x, targetVar, byVar) {
    w <- group_by_(x, byVar, targetVar) %>%
        dplyr::summarize(casos = n()) %>%
        mutate(porcentaje = casos / sum(casos) * 100) %>%
        select(-casos)
    return(w)
}


# Grafica de barras de 2 variables
sbc_plot_2_var_bar <- function(x, var1, var2, xlabel, ylabel = "%" ,position = "dodge", valueLabel = "valor" ,flipLabels = FALSE) {
    w <-  sbc_list_2_var_perc(x, var1, var2)
    names(w) <- c(var2,var1,valueLabel)
    g <-  ggplot(w, aes_string(x = var2,
                               y = valueLabel,
                               fill = var1)) +
        geom_bar(
            stat = "identity",
            position = position,
            color = "#666666",
            alpha = chart_alpha
        ) +  labs(x = xlabel, y = ylabel) +
        theme_minimal() + chart_colors()

    if(flipLabels){
        g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    return(ggplotly(g))
}

# Gráfica de barras de una variable
sbc_plot_1_var_bar <-
    function(x, var, xlabel, ylabel = "%", valueLabel = "valor", flipLabels = FALSE) {
        w <- sbc_table_1_var_perc(encuestas, var)
        names(w) <- c(var,valueLabel)
        g <-  ggplot(w, aes_string(x = var,
                                   y = valueLabel,
                                   fill = var)) +
            geom_bar(
                stat = "identity",
                color = "#666666",
                alpha = chart_alpha
            ) +  labs(x = xlabel, y = ylabel) +
            theme_minimal() + chart_colors()

        if(flipLabels){
            g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        return(ggplotly(g))
    }

sbc_plot_2_var_box <- function(x,numCol, catCol, numColLabel = numCol, catColLabel = catCol){

   g <- ggplot(x, aes_string(y = numCol, x=catCol,color=catCol)) +
        geom_boxplot() +
             coord_flip() +
                theme_minimal() +
                     chart_colors() + labs(x = catColLabel,y = numColLabel)
   return(ggplotly(g))
}

sbc_table_2_var_boxplot <- function(x, numCol, catCol){
    numCol <- as.name(numCol)
     x %>% group_by_(catCol) %>%
        summarise_(minimo = min(numCol))
}

sbc_plot_1_var_box <- function(x,numCol,  numColLabel = numCol ){
    g <- ggplot(x, aes_string(y = numCol)) +
        geom_boxplot() +
        coord_flip() +
        theme_minimal() +
        chart_colors() + labs(y = numColLabel)
    return(g)
}

sbc_transpose <- function(x){
  col_names <- x[[1]]
  x <- as_tibble(t(x[,-1]))
  names(x) <- col_names
  return(x)
}


# Crea una tabla contrastando dos variab les con una tecera
# Se debe proporcionar la función y la coluimna por la que se desea crear el cudro
# Ejemplo: sbc_table_2_var_summarise(cadenas,"cadena","avatar","gasto_visita", mean, na.rm = T)
# En la función anterior se desea mostrar un cuadro de cadenas vs avatars mostrando el promedio del gasto por visita en cada celda
sbc_table_2_var_summarise <- function(x,rowVar, colVar,summarizeVar , FUN, na_to_cero = F, na.rm =FALSE){
  x$summarizeVar <- x[[summarizeVar]]
  w <- x %>% dplyr::group_by_(rowVar,colVar) %>% dplyr::summarise(value =FUN(summarizeVar,na.rm = na.rm))  %>% ungroup()
  total <- sum(w$value)
  w <-  w  %>% spread_(colVar,"value")
  if(na_to_cero){
    for(i in 2:ncol(w)){
      col <- w[i]
      if(sum(is.na(col))>0){
        col[is.na(col)] <- 0
        w[i] <- col
      }
    }
  }
  return (w)
}

# Toma una tabla y la con vierte en porcentajes de acuerdo al marge proporcionado
# Es util cuando se usa la función sbc_table_2_var_summarise y luego queremos el valor expresado en prentajes
sbc_table_to_perc <- function(x,MARGIN =1) {
  w <- x
  if (MARGIN == 1) {
    totalCounts <- sbc_get_row_sum_vector(w)
    for (i in 2:ncol(w)) {
      w[[i]] <- w[[i]] / totalCounts * 100
    }
  }else if(MARGIN == 2){
    col_sum <- sbc_get_col_sum_row(w)
    for(i in 1:nrow(w)){
      row <- w[i,]
      for(j in 2:ncol(w)){
        w[i,j] <- row[[j]] / col_sum[1,j] *100
      }
    }
  }else{
    row_sum <- sbc_get_row_sum_vector(w)
    total_sum <- sum(row_sum)
    for(i in 1:nrow(w)){
      row <- w[i,]
      for(j in 2:ncol(w)){
        w[i,j] <- row[[j]] / total_sum *100
      }
    }
  }
  return(w)
}
