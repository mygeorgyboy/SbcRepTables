#' SbcRepTables: A way to have "Summary Tables" for creating quick reports
#'
#' with Mardown or to export to excel- Allso include a couple of quick charts.

#' @section Table functions:
#' sbc_table_2_var_count
#' sbc_table_1_var_count
#'
#' @docType package
#' @name SbcRepTables
NULL

library(tidyverse)
library(knitr)
library(plotly)
library(kableExtra)

# Report settings
chart_alpha <- 0.5
chart_colors <- function(){
    scale_fill_brewer(type = "qual", palette = "Set1")
}

#'  Agrega un renglon con totales a un dataframe
#'
#' Las columnas no numericas contendran un caracter de longitud 0 o el texto del parametro charColText
#' @export
sbc_add_col_sum <- function(x,sumLabel = ""){
    row <-  sbc_get_col_sum_row(x,sumLabel)
    return (rbind(x,row))
}

#' Agrega un renglon con totales a un dataframe
#'
#' Las columnas no numericas contendran un caracter de longitud 0 o el texto del parametro charColText
#' @export
sbc_add_row_sum <- function(x,sumLabel = "total"){
    col_vec <-  sbc_get_row_sum_vector(x)
    x[[sumLabel]] <- col_vec
    return (x)
}


# La función recibe una columna de un dataframe.
# Solo regresa un numero o text, no aplica para dataframes
# 1) si la columna es numerica regresa la suma
# 2) si la columna es alfanumerica regresa un caracter de longitud 0 o lo que vega en el parametro charColText
# Normalmente se llama desde la función 'sbc_sum_columns' para agregar un renglon con totales por columna
sbc_get_col_sum <- function(x,sumLabel = ""){
    if(class(x) =="character" || class(x) =="factor"  ){
        return (as.character(sumLabel) );
    } else if(class(x) == "numeric" || class(x) == "integer"){
        return(sum(x,na.rm = TRUE))
    }
    return(x)
}

sbc_get_col_sum_row <- function(x,sumLabel = ""){
    return(as_data_frame(map(x,sbc_get_col_sum,sumLabel)))
}

# Esta función es un wraper de knitr::kable
# Agrega la siguente funcionalidad:
# 1) Los números los muestra con separador de miles
# 2) Agrega estilo (Bold) al ultimo renglon si el parametro hasColTotals = TRUE (para mostrar totales)
sbc_kable <- function(x,..., hasColTotals = FALSE,hasRowTotals = FALSE,format.args = NULL,digits = 2) {
    tab <- knitr::kable(x,..., format.args = list(decimal.mark = '.', big.mark = ","),digits = digits )
    if(!"list" %in% class(x)){
        if(hasColTotals){
            tab <-tab %>% row_spec(nrow(x), bold = T)
        }
        if(hasRowTotals){
            tab <-tab %>% column_spec(ncol(x), bold = T)
        }
        return(tab)
    }else{
        # Se trata de una lista de dataframes, generar el cuadro
        w <- x[[1]]
        for(j in 2:length(x)){
            w <- bind_rows(w,x[[j]])
        }
        names(w)[1] <- "."
        tab <- knitr::kable(w,..., format.args = list(decimal.mark = '.', big.mark = ","),digits = digits )
        indices <- map_dbl(x, nrow)
        tab <- tab %>% group_rows(index = indices) %>% scroll_box(width = "100%")
        return(tab)
    }
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
             colVar,
             na.rm = FALSE
    ) {
        if(rowVar == colVar){
            x$aux3874lk <- x[[rowVar]]
            colVar<- "aux3874lk"
        }

        w <- group_by_(x, colVar, rowVar) %>%
            summarize(nameCount = n())
        if(na.rm){
            w <-w[!is.na(w[[2]]),]
            w <-w[w[[2]]!='NA',]
        }

        names(w) <- c(colVar, rowVar, "count")
        w <- spread_(w, colVar, "count")
        names(w) <-  names(w) %>% str_replace_all('<NA>','NA')
        w <- w %>% mutate_at(c(2:length(w)), funs(replace(., is.na(.), 0)))
        if(na.rm){
            w <- w %>% select(-starts_with('<NA>'))
            w <- w %>% select(-starts_with('NA'))

        }
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
#sbc_table_2_var_perc <- function(x, rowVar, colVar,MARGIN =1,na.rm =FALSE) {
#    w <- sbc_table_2_var_count(x,rowVar,colVar,na.rm )
#    w <- sbc_table_to_perc(w,MARGIN = MARGIN)
#    return(w)
#}

sbc_table_2_var_perc <- function(x, rowVar, colVar,MARGIN =1,na.rm =FALSE, incidenceVariable = NULL) {

    w <- sbc_table_2_var_count(x,rowVar,colVar,na.rm )
    if(!is.null(incidenceVariable)){
        z <-x %>% group_by_(colVar) %>%
            summarise(divide_by = n_distinct(!!sym(incidenceVariable))) %>%
            sbc_transpose() %>%
            mutate(dummy39871876 = "dummy") %>% select(dummy39871876, names(w)[-1])
        for(i in 2:length(names(w))){
            w[[i]]  =  w[[i]] /z[[1,i]] * 100
        }
    }else {
        w <- sbc_table_to_perc(w,MARGIN = MARGIN)
    }
    return(w)
}

# Regresa una tabla con el porcentaje para cada valor de la variable pasada como parametro
# Se puede usuar showCount = FALSE para mostrar la cuenta y el porcentaje
# A tibble: 2 x 3
#   genero     n perc
#   <chr>  <int>      <dbl>
# 1 Hombre   839      26.2
# 2 Mujer   2361      73.8
sbc_table_1_var_perc <- function(x, col_name,showCount = FALSE, incidenceVariable = NULL) {
    w <- count_(x,col_name)
    if(is.null(incidenceVariable)){
        divide_by <- sum(w$n)
    }else{
        divide_by <- x[[incidenceVariable]] %>% unique() %>% length()
    }
    w <- w %>% mutate(perc = n / divide_by *100) %>% arrange(desc(perc))
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
sbc_plot_2_var_bar <- function(x, var1, var2, xlabel=var1, ylabel = "%" ,position = "dodge", valueLabel = "valor" ,flipLabels = FALSE, invertVars = FALSE, returnGGplot = FALSE) {
    w <-  sbc_list_2_var_perc(x, var1, var2) %>% arrange(desc(porcentaje))
    names(w) <- c(var2,var1,valueLabel)
    if(invertVars){
        g <-  ggplot(w, aes_string(x = var1,
                                   y = valueLabel,
                                   fill = var2)) +
            geom_bar(
                stat = "identity",
                position = position,
                color = "#666666",
                alpha = chart_alpha
            ) +  labs(x = xlabel, y = ylabel) +
            theme_minimal() + chart_colors()
    }else{
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
    }


    if(flipLabels){
        g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }

    if(returnGGplot){
        g
    }else{
        return(ggplotly(g))
    }
}

# Gráfica de barras de una variable
sbc_plot_1_var_bar <-
    function(x, var, xlabel=var, ylabel = "%", valueLabel = "valor", flipLabels = FALSE) {
        w <- sbc_table_1_var_perc(x, var)
        names(w) <- c(var,valueLabel)
        g <-  ggplot(w, aes_string(x = var,
                                   y = valueLabel)) +
            geom_bar(
                stat = "identity",
                color = "#666666",
                alpha = chart_alpha
            ) +  labs(x = xlabel, y = ylabel) +
            theme_bw()
        #+ chart_colors()

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
    x$sbc_val <- x[[numCol]]
    x %>% group_by_( catCol) %>%
        summarize(`Min.` = min(sbc_val,na.rm = T),
                  `1st Qu.` = quantile(sbc_val, probs = c(0.25),na.rm = T),
                  Median = median(sbc_val,na.rm = T),  Mean = mean(sbc_val,na.rm = T),
                  `3rd Qu.` = quantile(sbc_val, probs = c(0.75),na.rm = T),
                  `Max.` = max(sbc_val,na.rm = T))
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

#' Lista resumen
#' Regresa una lista con varios dataframes, uno para cada valor del parámetro 'sectionVars'
#' Los dataframes contienen una columna para cada valor del parametro 'colVar'
#' Los renglones de los dataframes tienen un renglon para cada valor de 'sectionVars'
#' El primer dataset contiene la cuenta de casos
sbc_summary_list <- function(x,
                             colVar=NA,
                             sectionVars,
                             sectionNames = sectionVars,
                             MARGIN=2,
                             showCount = FALSE,
                             countLabel = "Casos",
                             percentageLabel = 'Porcentaje',
                             totalLabel = 'Total',
                             showGlobalCount = T,
                             showGlobalPercentage = T,
                             showSectionCount = T,
                             showSectionPercentage = T,
                             globalCountSectionLabel = "Casos totales",
                             na.rm = TRUE,
                             incidenceVariable = NULL){

    get_section_count <- function(x, col_name, na.rm = TRUE, show_percentage = T){
        in_df <- x

        if(show_percentage){
            tmp_df <- in_df %>%
                group_by(!!sym(col_name)) %>%
                summarise(count=n()) %>%
                mutate(pct = count/sum(count)*100) %>%
                sbc_add_col_sum(sumLabel = totalLabel)
        }else{
            tmp_df <- in_df %>%
                group_by(!!sym(col_name)) %>%
                summarise(count=n()) %>%
                sbc_add_col_sum(sumLabel = totalLabel)
        }


        # change NA names for 'NA string
        tmp_df[[1]][is.na(tmp_df[[1]] )] <- 'NA'

        tmp_df <- tmp_df %>% sbc_transpose() %>%
            mutate(row_label = NA) %>%
            select(row_label,everything())

        tmp_df$row_label[1] <- countLabel

        if(show_percentage){
            tmp_df$row_label[2] <- percentageLabel
        }

        return (tmp_df)
    }


    #Primero procesar seccionesVar y SeccionesName para incluir solo las columnas validas
    #incluir <- !sectionVars %in% setdiff(sectionVars, names(x))
    #sectionVars <- sectionVars[incluir]
    #sectionNames <- sectionNames[incluir]



    if(!is.na(colVar)){

        if(na.rm ){
            in_df <- filter(x, !is.na(!!sym(colVar)))
        }else{
            in_df <- x
        }

        # Obtener los nombres de los encabezados para asegurar que a ninguna sección le faltan valores
        summary_col_names  <- get_section_count(in_df, colVar,na.rm =  na.rm ,show_percentage = F)
        summary_col_names <- names(summary_col_names)

        if(showGlobalCount){
            data_frames_list <- vector("list", length(sectionVars) +1)
            numeric_indicator <- vector("logical",length(sectionVars) +1)
            data_frames_list[[1]]<- get_section_count(in_df, colVar,na.rm =  na.rm ,show_percentage = showGlobalPercentage)
            sectionNames <- c(globalCountSectionLabel,sectionNames)
            idx <- 2
        }else{

            #get_section_count(in_df, colVar,na.rm =  na.rm ,show_percentage = showGlobalPercentage)

            data_frames_list <- vector("list", length(sectionVars))
            numeric_indicator <- vector("logical",length(sectionVars) )

            idx <- 1
        }

        for(i in 1:length(sectionVars)){
            col_name = sectionVars[i]
            multi_col <- FALSE
            if(col_name %>% str_detect("\\*")){
                col_name <- col_name %>% str_remove_all("\\*")
                multi_cols_names <- names(x)[names(x) %>% startsWith(col_name)]
                tmp_df <- NULL
                if(length(multi_cols_names)>0){
                    for(u in seq_along(multi_cols_names)){
                        if(is.null(tmp_df)){
                            tmp_df <- in_df %>% select(multi_cols_names[u],colVar) %>%
                                rename(valor =multi_cols_names[u]) %>% filter(!is.na(valor))
                        }else{
                            tmp_df <- in_df %>% select(multi_cols_names[u],colVar) %>%
                                rename(valor =multi_cols_names[u]) %>% filter(!is.na(valor)) %>%
                                bind_rows(tmp_df)
                        }
                    }
                }
                multi_col <- TRUE
                bck_in_df <- in_df
                in_df <- tmp_df
                names(in_df)[1] <- col_name

            }

            ## calcular primero el numero de casos y porcentajes
            if(na.rm ){
                # Esta linea se habia modificado para variables con * pero causa bugs
                # in_df <- filter(in_df, !is.na(!!sym(col_name)),!is.na(!!sym(colVar)))
                in_df <- filter(x, !is.na(!!sym(col_name)),!is.na(!!sym(colVar)))
            }

            if(class(in_df[[col_name]]) == "numeric" || class(in_df[[col_name]]) == "integer" ){
                wmin <- sbc_get_num_col_fun(in_df,colVar,col_name,min)
                wq1 <- sbc_get_num_col_fun(in_df,colVar,col_name,q1)
                wmedian <- sbc_get_num_col_fun(in_df,colVar,col_name,median)
                wmean <- sbc_get_num_col_fun(in_df,colVar,col_name,mean)
                wq3 <- sbc_get_num_col_fun(in_df,colVar,col_name,q3)
                wmax <- sbc_get_num_col_fun(in_df,colVar,col_name,max)
                w <- bind_rows(wmin,wq1,wmedian,wmean,wq3, wmax)
                numeric_indicator[idx] <- TRUE

            }else{
                w <-sbc_table_2_var_perc(in_df,col_name,colVar,MARGIN = MARGIN, na.rm = na.rm,  incidenceVariable =  incidenceVariable)
                names(w)[1] <-"concepto"
                w_tot <- sbc_table_1_var_perc(in_df,col_name,  incidenceVariable =  incidenceVariable)
                names(w_tot) <-c("concepto","total")
                w <- left_join(w,w_tot)
            }
            names(w)[1] <- "row_label"
            names(w)[length(names(w))] <- totalLabel
            misssing_cols <- setdiff(summary_col_names, names(w))
            for(z in misssing_cols){
                w[[z]] <- ""
            }
            w <- w %>% select(summary_col_names)

            if(showSectionCount){
                section_totals_df <- get_section_count(in_df,
                                                       colVar,na.rm =  na.rm ,
                                                       show_percentage =  showSectionPercentage)
                for(z in misssing_cols){
                    section_totals_df[[z]] <- ""
                }
                section_totals_df <- section_totals_df %>% select(summary_col_names)
                data_frames_list[[idx ]]<- bind_rows(section_totals_df, w) %>% select(summary_col_names)
            }else{
                data_frames_list[[idx ]] <- w
            }
            idx <- idx +1
            if(multi_col){
                in_df <- bck_in_df
            }
        }
        names(data_frames_list) <- c(sectionNames)
    }else{
        # Sin grupos
        if(showCount){
            data_frames_list <- vector("list", length(sectionVars) +1 )
            casos <- data_frame(countLabel , count(x)$n)
            names(casos) <- c("concepto","valor")
            sectionNames <- c(countLabel,sectionNames)
            data_frames_list[[1]]<- casos
            idx <- 2
        }else{
            data_frames_list <- vector("list", length(sectionVars) )
            idx <- 1
        }

        for(i in 1:length(sectionVars)){
            col_name = sectionVars[i]
            if(class(x[[col_name]]) == "numeric" || class(x[[col_name]]) == "integer" ){
                wmin <- min( x[[col_name]],na.rm = T)
                wq1 <- quantile(x[[col_name]], c(0.25),na.rm = T)
                wmedian <- median( x[[col_name]],na.rm = T)
                wmean <- mean( x[[col_name]],na.rm = T)
                wq3 <-quantile(x[[col_name]], c(0.75),na.rm = T)
                wmax <- max( x[[col_name]],na.rm = T)
                w <-  tribble(
                    ~concepto, ~valor,
                    "min",   wmin,
                    "q1",   wq1,
                    "median",   wmedian,
                    "mean",   wmean,
                    "q3",   wq3,
                    "max", wmax
                )
            }else{
                w <- sbc_table_1_var_perc(x,col_name)
                w <- w %>% sbc_add_col_sum(sumLabel = totalLabel)
                names(w) <- c("concepto", "valor")
            }
            data_frames_list[[idx ]]<- w
            idx <- idx +1
        }
        names(data_frames_list) <- c(sectionNames)
    }

    attr(data_frames_list,'numeric_indicator') <- numeric_indicator
    attr(data_frames_list,'group_width') <- c(length(summary_col_names))


    return(data_frames_list)
}



#' Aplica una función a una columna numerica
#' El resultado es agrupado por 'colVar'
#' Al final se agrega la columna ´A total´ que contien el resultado
#' de la función sin agrupar.
#' Se utiliza en sbc_summary_list para calcular la media y mediana
sbc_get_num_col_fun <- function(x,colVar,col_name,FUN){
    fun <- as.character(substitute(FUN))
    summ <- paste0(fun,'(', col_name, ',na.rm =T)')
    w <- x %>% group_by_(.dots=colVar) %>%  dplyr::summarise_(.dots = setNames(summ,summ)) %>% sbc_transpose()
    fun_tot <- paste0(fun,'(', x[[col_name]], ',na.rm =T)')
    w$`A total` <-FUN(x[[col_name]],na.rm = T)
    #w$`A total` <-FUN(x[[col_name]],na.rm = T)
    w$concepto <- fun
    names(w) <- sbc_clean_na_names(names(w))
    w <- select(w,concepto,everything())
}

q1 <- function(x,...){
    return(quantile(x, c(0.25),na.rm = T))
}
q3 <- function(x,...){
    return(quantile(x, c(0.75),na.rm = T))
}
#' Remove outliers from a vector
#' It make the value NA
#' Used to report numerical values without outliers
#' Just generate a new colum in the data set
#' @example
#' cadenas$gasto_visita_no_outliers <- sbc_clen_outliers(cadenas$gasto_visita)
sbc_clen_outliers <- function(x) {
    iqr <- IQR(x, na.rm = T)
    if(iqr >0){
        q1 <- quantile(x, c(0.25), na.rm = T)
        q2 <- quantile(x, c(0.5), na.rm = T)
        q3 <- quantile(x, c(0.75), na.rm = T)
        min_range <-  q1 - (1.5 * iqr)
        max_range <-  q3 + (1.5 * iqr)
        #mean_muestra <- mean(x[ (x>min_range & x < max_range)] ,na.rm = T)
        #x[ !(x>min_range & x < max_range)] <- mean_muestra
        x[ !(x>min_range & x < max_range)] <- NA
    }
    return(x)
}

sbc_clean_na_names <- function(x){
    # x contiene los nombres de un dataframe
    if(sum(is.na(x))>0){
        x[is.na(x)] <- 'NA'
    }
    return(x)
}


library(data.tree)
# Funcion principal para crear los arboles
# Solo necesita los datos (solo las columnas a usar) y el nombre del arbol
# Si no se quieren incluir los porcentajes en el nombre  show_nums = F
sbc_get_tree <- function(x, tree_name="",show_nums = T){
    sbc_get_tree_aux(x,Node$new(tree_name,ct=nrow(x),pt=100),level=1,nrow(x),show_nums = show_nums )
}

# Función auxiliar, no debe llamarse directamente
sbc_get_tree_aux <- function(x,node,level=1,tot_count,show_nums = T){
    node_childrens <- x %>% group_by_(names(x)[1]) %>% summarise(count =n()) %>% mutate(perc = count/tot_count*100) %>% arrange(desc(perc))
    for(i in 1:nrow(node_childrens)){
        row <-node_childrens[i,]
        if(show_nums == TRUE){
            node_name <- paste(" <",level,"> " ,row[[1]]," (",row$count,") ",round(row$perc,2),"%",sep = "")
        }else{
            node_name <- paste(" <",level,"> " ,row[[1]],sep= "")
        }
        new_node <- Node$new(node_name,ct=row$count,pt=row$perc)
        node$AddChildNode(new_node)
        if(ncol(x)>1){
            y <- x[x[[1]] == row[[1]],-1]
            sub_node <- sbc_get_tree_aux(y,new_node,level+1,tot_count,show_nums)
        }
    }
    return(node)
}

sbc_remove_na_columns <- function(x){
    x <- x[,colSums(is.na(x))<nrow(x)]
}


sbc_load_rds_dir <- function(x){
    data_path <- x
    data_files <- list.files(data_path)
    for(i in seq_along(data_files)){
        file_name <- paste(data_path,"/", data_files[i], sep = "")
        if(file_name %>% str_detect(".rds")){
            assign( data_files[i] %>% str_remove(".rds"),read_rds(file_name), envir= .GlobalEnv)
        }
    }
}

#' Compare one column on different data frames
#'
#' Used to compare several years of a variable
#'
#' @param x A list contaning the dataframes, names are used as name columns.
#' @param var_name variable to be compared
#' @param incidence_var if not null is used to calculate incedence (divide by length(unique(incidence_var)))
#'
sbc_compare_df_var_perc <- function(x,var_name, transpos_ind =F, incidence_var = NULL, showCount = F){
    if(is.null(names(x))){
        names(x) <- as.character(seq_along(x))
    }
    var_table <- NULL
    for(i in seq_along(x)){

        tmp_ds <- count_( x[[i]],var_name)
        divide_by <-sum(tmp_ds$n)
        if(!is.null(incidence_var)){
            divide_by <- x[[i]][[incidence_var]] %>% unique() %>% length()
        }
        tmp_ds <- tmp_ds %>% mutate(perc = n /  divide_by*100) %>% arrange(desc(perc))
        if(!showCount){
            tmp_ds <- tmp_ds[,c(1,3)]
        }

        tmp_ds$ds_name <- names(x)[i]
        if(is.null(var_table)){
            var_table <- tmp_ds
        }else{
            var_table <- bind_rows(var_table,tmp_ds)
        }
    }
    if(transpos_ind){
        out_ds <- var_table %>% spread(var_name, perc)
    }else{
        out_ds <- var_table %>% spread(ds_name, perc)
    }
    return(out_ds)
}

sbc_compare_ds_var_count <- function(x,var_name, transpos_ind =F){
    if(is.null(names(x))){
        names(x) <- as.character(seq_along(x))
    }
    var_table <- NULL
    for(i in seq_along(x)){
        tmp_ds <- x[[i]] %>% sbc_table_1_var_count(var_name)
        tmp_ds$ds_name <- names(x)[i]
        if(is.null(var_table)){
            var_table <- tmp_ds
        }else{
            var_table <- bind_rows(var_table,tmp_ds)
        }
    }
    if(transpos_ind){
        out_ds <- var_table %>% spread(var_name, count)
    }else{
        out_ds <- var_table %>% spread(ds_name, count)
    }
    return(out_ds)
}

hm_style_min <-   read_file("./data/hm_style_min.css")
hm_style <-   read_file("./data/hm_style.css")
hm_script <-   read_file("./data/hm_javascript.js")

sbc_html_heatmap  <- function(x, noFormatRows = c("Casos") ,
                              numericRows = c("min","q1","q3","median","mean","max"),
                              plasmaRows = c("sum_pct_no"),
                              numericIndicator = NULL,
                              groupWidths = NULL,
                              groupNames= NULL,
                              boldColumns = grep("Total",names(x[[1]]) ,value = T),
                              tableHeight=NULL,
                              tableWidth=NULL,
                              showGroups = FALSE,
                              grayTitles = T,
                              groupSeparator = F,
                              hide_titles =F){
    datos <- x

    # has groups of columns ?
    if(is.null(groupWidths)){
        if(is.null(attr(x,"group_width"))){
            has_groups <- FALSE
        }else{
            group_widths <- attr(x,"group_width")
            has_groups <- TRUE
        }
    }else{
        group_widths <- groupWidths
        has_groups <- TRUE
    }

    group_separators  <- cumsum(group_widths) +2
    group_separators[1:(length(group_separators))]

    if(has_groups){
        if(is.null(groupNames)){
            group_names <- vector(mode = 'character',length = length(group_widths))
            for(i in seq_along(group_widths)){
                group_names[i] <- paste("Grupo",i)
            }
        }else{
            group_names <- groupNames
        }
    }

    # Si no falla borrar esta seccion
    #if(exists("numericIndicator") && !is.null(numericIndicator)){
    #    numeric_indicator <- numericIndicator
    #}else if(!is.null(attributes(x)[["numeric_indicator"]])){
    #    numeric_indicator <- attributes(x)[["numeric_indicator"]]
    #}else{ # all string columns
    #    numeric_indicator <-  vector("logical",length(datos))
    #}




    tag <- function(tag, text){
        return(paste("<",tag,">",text,"</",tag,">",sep = ""))
    }

    get_row_max <- function(x){
        tmp_x <- x %>% select_if(is.numeric)
        max_vector <- vector(mode="numeric", length=nrow(tmp_x))
        for(i in 1:nrow(tmp_x)) {
            row <- tmp_x[i,]
            max_val <- -Inf
            for(j in seq_along(row)){
                if(!is.na(row[[j]])){
                    if(row[[j]]>max_val){
                        max_val <- row[[j]]
                    }
                }
            }
            max_vector[i] <- max_val
        }
        return(max_vector)
    }

    get_row_min <- function(x){
        tmp_x <- x %>% select_if(is.numeric)
        min_vector <- vector(mode="numeric", length=nrow(tmp_x))
        for(i in 1:nrow(tmp_x)) {
            row <- tmp_x[i,]
            min_val <- +Inf
            for(j in seq_along(row)){
                if(!is.na(row[[j]])){
                    if(row[[j]]<min_val){
                        min_val <- row[[j]]
                    }
                }
            }
            min_vector[i] <- min_val
        }
        return(min_vector)
    }

    cell_colors <- c('#FDE725','#F7E620','#F1E51D','#EBE51A','#E4E419','#DDE318','#D7E219','#D0E11C','#C9E020',
                     '#C2DF23','#BBDE28','#B4DE2C','#ADDC30','#A7DB35','#A0DA39','#99D83D','#92D741','#8CD646',
                     '#85D54A','#7FD34E','#78D152','#73D056','#6DCD59','#67CC5C','#61CA60','#5BC863','#56C667',
                     '#51C56A','#4CC26C','#47C06F','#41BE71','#3DBC74','#39BA76','#35B779','#31B67B','#2EB37C',
                     '#2BB07F','#28AE80','#25AC82','#24AA83','#22A785','#20A486','#1FA287','#1FA088','#1F9E89',
                     '#1E9B8A','#1F998A','#1F968B','#20938C','#20928C','#218F8D','#228D8D','#238A8D','#24878E',
                     '#25858E','#26828E','#26818E','#277E8E','#287C8E','#29798E','#2A768E','#2B748E','#2C718E',
                     '#2D708E','#2E6D8E','#2F6B8E','#31688E','#32658E','#33638D','#34608D','#355E8D','#375B8D',
                     '#38598C','#39558C','#3B528B','#3C508B','#3D4D8A','#3E4A89','#3F4788','#404587','#424186',
                     '#433E85','#443B84','#453882','#453581','#46327E','#472F7C','#472C7A','#482878','#482576',
                     '#482173','#481E70','#481B6D','#481769','#481467','#471063','#470D60','#46085C','#450558',
                     '#440154')

    cell_colors_plasma <- c("#F0F921FF", "#F1F426FF", "#F3F027FF", "#F5EC27FF", "#F6E726FF", "#F7E225FF", "#F8DE25FF",
                            "#FADA24FF", "#FBD624FF", "#FCD225FF", "#FCCD25FF", "#FDC926FF", "#FDC527FF", "#FDC129FF",
                            "#FEBD2AFF", "#FEB92CFF", "#FDB52EFF", "#FDB130FF", "#FDAD32FF", "#FCA934FF", "#FCA536FF",
                            "#FBA238FF", "#FA9E3BFF", "#FA9B3DFF", "#F9973FFF", "#F89441FF", "#F79044FF", "#F58C46FF",
                            "#F48948FF", "#F3864BFF", "#F1824CFF", "#F07F4FFF", "#EF7B51FF", "#ED7953FF", "#EB7556FF",
                            "#E97257FF", "#E76F5AFF", "#E66C5CFF", "#E4695EFF", "#E26560FF", "#E06363FF", "#DE6065FF",
                            "#DC5D67FF", "#DA5A6AFF", "#D8576BFF", "#D5546EFF", "#D35271FF", "#D14E72FF", "#CE4B75FF",
                            "#CC4977FF", "#CA457AFF", "#C8437BFF", "#C5407EFF", "#C23D81FF", "#C03A83FF", "#BD3786FF",
                            "#BB3488FF", "#B7318AFF", "#B52F8CFF", "#B22B8FFF", "#AF2892FF", "#AC2694FF", "#A92296FF",
                            "#A62098FF", "#A21D9AFF", "#9F1A9DFF", "#9C179EFF", "#99149FFF", "#9511A1FF", "#910EA3FF",
                            "#8E0CA4FF", "#8A09A5FF", "#8707A6FF", "#8305A7FF", "#7F03A8FF", "#7B02A8FF", "#7701A8FF",
                            "#7301A8FF", "#6F00A8FF", "#6B00A8FF", "#6700A8FF", "#6300A7FF", "#6001A6FF", "#5B01A5FF",
                            "#5701A4FF", "#5302A3FF", "#4F02A2FF", "#4B03A1FF", "#47039FFF", "#43039EFF", "#3E049CFF",
                            "#39049AFF", "#350498FF", "#300597FF", "#2C0594FF", "#270591FF", "#220690FF", "#1B068DFF",
                            "#150789FF", "#0D0887FF")

    datos <- x
    tdata <- ""

    for(k in seq_along(datos)) {

        # Si no falla borrar este bloque
        #if(numeric_indicator[k]){
        #    numeric_section <- T
        #}else{
        #    numeric_section <- F
        #}
        tabla <- datos[[k]]
        if(is.null(tabla)){
            next
        }

        if(sum(class(tabla) == 'character')>0){
            span_length <- length(names(datos[[1]]))
            if(groupSeparator){
                span_length <- span_length +length(group_separators)
            }
            tdata <- paste(tdata,"<tr><td class='report-section-header' colspan='",span_length,"'>", tabla,"</td></tr>", sep = "")
            next;
        }
        row_max <- get_row_max(tabla)
        row_min <- get_row_min(tabla)

        out <- ""
        if(!grayTitles){
            # Solo mostrar una celda con el titulo de la sección
            # la celda se expade por tota la tabla
            tdata <-
                paste(
                    tdata,
                    "<tr><td class='section-header' colspan='",
                    length(tabla),
                    "' >",
                    names(datos)[k],
                    "</td></tr>",

                    sep = ""
                )
        }else{
            # Se repite el encabezado para cada sección
            # agregar titulos
            hline <- ""
            hnames <- names(datos[[1]])
            hnames[1] <- ""
            for(i in seq_along(hnames)){
                col_name <- hnames[i]

                if(i==1){
                    hline <- paste(hline,"<td class='section-header'>",names(datos)[k],"</td>")
                }else{
                    if(groupSeparator && i %in% group_separators){
                        hline <- paste(hline,"<td class='group-separator'></td>")
                    }
                    hline <- paste(hline,"<td class='gray-header'>",col_name,"</td>")
                }
            }
            tdata <-
                paste(
                    tdata,
                    "<tr>",
                    hline,
                    "</tr>",
                    sep = ""
                )
        }

        for (i in 1:nrow(tabla)) {
            cursor <- tabla[i, ]
            tr <- ""
            for (j in 1:length(cursor)) {
                value <-  cursor[[1, j]]
                if(j==1){
                    row_name <- value
                    class_text <- " class='row-name' "
                    if(row_name %in% numericRows){
                        numeric_row <- TRUE
                    }else{
                        numeric_row <- FALSE
                    }

                    if(row_name %in% plasmaRows){
                        plasma_row <- TRUE
                    } else{
                        plasma_row <- FALSE
                    }
                }else if(names(cursor)[j] %in% boldColumns){
                    class_text <- " class='bolder-column' "
                }else{
                    class_text <- "class='cell-data'"
                }
                td <- ""

                if(is.na(value)){
                    value = ""
                }

                if (c("numeric")  %in%  class(value)) {
                    value <- round(value,2)

                    if(!is.na(value) & value>45 & value<= 100 ){
                        text_color = '#ffffff'
                    }else{
                        text_color = '#000000'
                    }

                    #if(!numeric_section){

                    if(!numeric_row){
                        if(row_name %in% noFormatRows){
                            background_color <- "transparet"
                            text_color =  '#333'
                        }else{
                            if(plasma_row){
                                background_color <- cell_colors_plasma[value]
                            }else{
                                background_color <- cell_colors[value]
                            }
                        }
                        td <-
                            paste(
                                "<td  ",class_text," style='background: ",
                                background_color,
                                ";color:",text_color,";'>",
                                round(value, 2),
                                "</td>",
                                sep = ""
                            )
                    }else{
                        if(value ==round(row_max[i],2)){
                            background_color <- "#FFEEEE"
                            text_color = '#333'
                        }else  if(value == round(row_min[i],2)){
                            background_color <- "#EEEEFF"
                            text_color = '#333'
                        }else{
                            background_color <- "transparent"
                            text_color <- "#333"
                        }

                        if(row_name %in% noFormatRows){
                            background_color <- "transparent"
                        }
                        td <-
                            paste(
                                "<td  ",class_text," style='background: ",
                                background_color,
                                ";color:",text_color,";'>",
                                round(value, 2),
                                "</td>",
                                sep = ""
                            )
                    }

                }else{
                    if(is.na(value)){
                        value = ''
                    }
                    td <- paste("<td ",class_text,">", value, "</td>", sep = "")
                }

                if(groupSeparator &&j %in% group_separators){
                    sep_cel <- paste("<td class='group-separator'></td>")
                    tr <-  paste(tr,sep_cel, td)
                }else{
                    tr <-  paste(tr, td)
                }



            }
            tdata <- paste(tdata, tag("tr", tr))
        }
    }


    #agregar grupos
    if(showGroups){
        if(has_groups){
            gline <- "<tr><td></td>" # La prmera columna libre
            #gline <- paste(gline, "<td class='group_header' colspan='", group_widths[1]-1 ,"' >",group_names[1],"</td>",sep = "")

            for(m in seq_along(group_widths)){
                gline <- paste(gline, "<td class='group_header' colspan='", group_widths[m]-1 ,"' >",group_names[m],"</td>",sep = "")
            }
            gline <- paste(gline, "</tr>",sep = "")
            tdata <- paste(gline,tdata)
        }
    }


    #agregar titulos aqui
    hline <- ""
    hnames <- names(datos[[1]])
    hnames[1] <- ""
    for(i in seq_along(hnames)){
        col_name <- hnames[i]

        if(i==1){
            hline <- paste(hline,"<th></th>")
        }else{
            if(groupSeparator && i %in% group_separators){
                hline <- paste(hline,"<th class='group-separator'></th>")
            }
            if(hide_titles){
                hline <- paste(hline,"<th><div><span>"," ","</span></div></th>")
            }else{
                hline <- paste(hline,"<th class='rotate'><div><span>",col_name,"</span></div></th>")
            }
        }
    }


    hline <-  tag("tr", hline)
    tdata <- paste(hline,tdata)
    tdata <- paste("<table class='table-style'>",tdata,"</table>")
    if(!is.null(tableWidth) |  !is.null(tableHeight) ){
        tdata <- paste("<div class='table-container' style='max-width: ",tableWidth,"px; max-height: ",tableHeight,"px'>",tdata,"</div>",sep = "")
    }else{
        tdata <- paste("<div class='table-container' >",tdata,"</div>",sep = "")
    }


    html_data <- tag("body",tdata)
    html_data <- paste("<head> <meta charset='UTF-8'><style>",hm_style,"</style></head>",html_data)
    html_data <- paste("<html id='all-page'>",html_data,"</html>")


    return (html_data)
}





sbc_viewer  <- function(html_data){
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, "index.html")
    writeLines(html_data, htmlFile)
    viewer <- getOption("viewer")
    viewer(htmlFile)
}



sbc_bind_summary_rows <- function(x,y){
    out_list <- append(x,y)
    numeric_indicator <-  c(attr(x,"numeric_indicator"),attr(y,"numeric_indicator"))
    attr(out_list,"numeric_indicator") <- numeric_indicator
    attr(out_list,"group_width") <- attr(x,"group_width")
    return(out_list)
}


sbc_get_section_count <- function(x, col_name, na.rm = TRUE, show_percentage = T , totalLabel = "Total", countLabel= 'Casos', percentageLabel = 'Porcentaje'){
    in_df <- x

    if(show_percentage){
        tmp_df <- in_df %>%
            group_by(!!sym(col_name)) %>%
            summarise(count=n()) %>%
            mutate(pct = count/sum(count)*100) %>%
            sbc_add_col_sum(sumLabel = totalLabel)
    }else{
        tmp_df <- in_df %>%
            group_by(!!sym(col_name)) %>%
            summarise(count=n()) %>%
            sbc_add_col_sum(sumLabel = totalLabel)
    }


    # change NA names for 'NA string
    tmp_df[[1]][is.na(tmp_df[[1]] )] <- 'NA'

    tmp_df <- tmp_df %>% sbc_transpose() %>%
        mutate(row_label = NA) %>%
        select(row_label,everything())

    tmp_df$row_label[1] <- countLabel

    if(show_percentage){
        tmp_df$row_label[2] <- percentageLabel
    }

    return (tmp_df)
}


sbc_penetracion_productos <- function(encuestas, productos, colVar , colFamilia = 'familia', sectionName = 'Penetración familias', totalLabel = "Total", countLabel= 'Casos', percentageLabel = 'Porcentaje'){
    z <- productos %>% inner_join(encuestas %>% select(id_encuesta,colVar))
    header <- sbc_get_section_count(z,colVar)
    names(header)[1] <- colFamilia
    casos_grupo <- encuestas %>% group_by(!!sym(colVar)) %>% summarize(count_grupo = n())

    a <- z %>% group_by(!!sym(colVar),!!sym(colFamilia)) %>%
        summarise(count = n()) %>%
        left_join(casos_grupo) %>%
        mutate(penetracion = count/count_grupo* 100) %>%
        select(-count,-count_grupo) %>% spread(!!sym(colVar),penetracion)

    b <- z %>% group_by(!!sym(colFamilia)) %>% summarise(Total = n()) %>% mutate(Total = Total /nrow(encuestas) * 100)

    z <- full_join(a,b)
    z <- bind_rows(header,z)

    size <- length(names(z))
    names(z)[length(names(z))] <- totalLabel
    z <- list(z)
    names(z) <- sectionName
    attr(z,"numeric_indicator") <- c(F)
    return(z)
}


sbc_facturacion_productos <- function(encuestas, productos, colVar , colMonto = 'monto', colFamilia = 'familia', sectionName = 'Facturacion familias', totalLabel = "Total", countLabel= 'Casos', percentageLabel = 'Porcentaje'){
    z <- productos %>% inner_join(encuestas %>% select(id_encuesta,colVar))
    header <- sbc_get_section_count(z,colVar)
    names(header)[1] <- colFamilia

    a <- z %>% group_by(!!sym(colVar),!!sym(colFamilia)) %>%
        summarise(gasto = sum(!!sym(colMonto),na.rm = T))  %>%
        mutate(total_gasto = sum(gasto,na.rm = T)) %>%
        mutate(perc = gasto / total_gasto*100) %>%
        select(colVar,colFamilia,perc)  %>% spread(!!sym(colVar),perc)


    b <- z %>% group_by(!!sym(colFamilia)) %>%
        summarise(gasto = sum(!!sym(colMonto),na.rm = T)) %>%
        mutate(total_gasto = sum(gasto,na.rm = T))  %>%
        mutate(perc = gasto / total_gasto*100) %>%
        select(colFamilia,Total =perc)
    z <-full_join(a,b)
    z <- bind_rows(header,z)

    size <- length(names(z))
    names(z)[length(names(z))] <- totalLabel

    z <- list(z)
    names(z) <- sectionName
    attr(z,"numeric_indicator") <- c(F)
    return(z)
}


sbc_bind_summary_cols <- function(x,y){
    for(i in seq_along(x)){
        section_x <- x[[i]]
        section_y <- y[[i]]
        x[[i]] <- full_join(section_x,section_y ,by = c( paste(names(section_x[1])) ))

    }
    attr(x,"group_width") <- c(attr(x,"group_width"),attr(y,"group_width"))
    return(x)
}

# Regresa el top n de una variable, el resto de los valores los agrupa enm Otros
sbc_top_n <- function(x, n,otherLabel = 'Otros'){
    x <- tibble(value = x)
    o <- x %>% group_by(value) %>% summarise(n=n()) %>% arrange(desc(n)) %>% .[1:n,]
    x$value[! x$value %in% o$value] <- otherLabel
    return(x$value)
}

# Regresa solo los valores que cumplen con el porcentaje mínimo, el resto de los valores los agrupa enmOtros
sbc_min_perc <- function(x, minPerc,otherLabel = 'Otros'){
    x <- tibble(value = x)
    o <- x %>% group_by(value) %>% summarise(n=n()) %>% mutate(perc = n/sum(n) *100) %>% filter(perc >= minPerc)
    x$value[! x$value %in% o$value] <- otherLabel
    return(x$value)
}

# Remueve una columna de una lista summario
sbc_remove_summary_column <- function(x,colNames){
    for(i in seq_along(x)){
        x[[i]] <- x[[i]] %>% select(-colNames)
    }
    attr(x,"group_width") <- length(names(x[[i]]))
    return(x)
}


sbc_xls_write_element <- function(wb, sheetName, listElement, position, sectionName = NULL){
    hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
                       border = "Bottom")
    if(!is.null(sectionName)){
        openxlsx::writeData(wb,sname, sectionName, startRow = position)
        position <- position +1
    }

    if(class(listElement) == 'list'){
        for(element in listElement){

            sbc_xls_write_element(wb, sheetName, element,position)
            position <- position + nrow(element) +2
        }
    }else{
        openxlsx::writeData(wb,sname, listElement, startRow = position, headerStyle = hs1)

        #openxlsx::writeDataTable(wb,sname, listElement, startRow = position,  tableStyle = "TableStyleLight9")
        position <- position + nrow(listElement) +2
    }
    return(position)
}

sbc_xls_write_list <- function(wb, sheetName, outputList, position){
    hs1 <- createStyle(fgFill = "#FF0000", halign = "CENTER", textDecoration = "italic",
                       border = "Bottom")
    for(i in 1:length(outputList) ){
        ds_salida <- outputList[[i]]
        if(is.null(ds_salida)){
            next;
        }
        if(!is.null(names(outputList) )){
            openxlsx::writeData(wb,sname, names(outputList)[i], startRow = position)
            position <- position +1
        }
        newpos <- sbc_xls_write_element(wb, sheetName, ds_salida, position )

        position <-  newpos
    }
}

#' Extrae un data.frame de varias columnas de una tabla
#'
#' @param x source dataframe
#' @param idvar The key or primary key of the dataset
#' @param pattern Common text of the columns names to collect
#' @param outColumn The output column name
#'
#' @examples
#' sbc_multi_column_to_df(atsrv19_encuestas, "folio", "preferencia_medios", "medio")
#'
#' Si tenemos las columnas preferencias_medios_01 a preferencias_medios_05
#' Haríamos lo siguiente: sbc_multi_column_to_df(atsrv19_encuestas, "folio", "preferencia_medios", "medio")
#' Como resultado tendríamos un df con las columnas folio y medio
sbc_multi_column_to_df <- function(x, idvar, pattern, outColumn){
    col_names <- names(x) %>% str_subset(pattern)
    if(length(col_names) == 0){
        outDf <- data.frame(idvar = character(),outColumn =  character()) %>% as.tibble()
        names(outDf) <- c(idvar, outColumn)
        return(outDf)
    }
    outDf  <- NULL
    for(col_name in col_names){
        y <- x %>% select(idvar,outColumn =col_name) %>% filter(!is.na(outColumn))
        if(is.null(outDf)){
            outDf <- y
        }else{
            if(nrow(y)>0){
                outDf <- bind_rows(outDf,y)
            }
        }

    }

    names(outDf) <- c(idvar, outColumn)
    return(outDf)
}

#' Convert the first letter of a text to upper case and the rest to lower case
#'
#' Transform every word in the string, also clean blank spaces at the begining and end
#' @param x text to be transofmed
#'
#' @examples
#' camel("Helo world. my name is")
#'
#' marcas15$marca <- map_chr(marcas15$marca ,camel) used to match case across dataframe brands
sbc_camel <- function(x){
    c <- strsplit(trimws(x) , " ")[[1]]
    s <- paste(toupper(substring(c, 1,1)), tolower(substring(c, 2)), sep="", collapse=" ")
    return(s)
}

#' Return rows with minimal value
#'
#' All rows that are less than the provided value are grouped in the last row
#'
#' @param  x The dataset
#' @param  colName The column to compare
#' @param  minValue The value to compare with
#' @param  sort True if we need to sort the result on using de colName values
#' @param  desc Order on descending order, if FALSE order in ascending order
#'
sbc_min_perc <- function(x, colName, minValue , sort = TRUE, desc = TRUE, otherLabel = 'Otros'){
    main_rows <- x %>%  filter(!!sym(colName) >= minValue)
    if(sort){
        if(desc){
            main_rows <- main_rows %>% arrange(desc(!!sym(colName)))
        }else{
            main_rows <- main_rows %>% arrange(!!sym(colName))
        }
    }
    sum_row <- x %>% filter(!!sym(colName) < minValue) %>%
        sbc_get_col_sum_row()
    sum_row[1,1] <- otherLabel

    top_summary <- bind_rows(main_rows,sum_row)
    return(top_summary)
}

#' Return Cramer V values
#'#'
#' @param  x Data from column 1
#' @param  x Data from column 2
#'
sbc_cramer_v <-  function(x,y) {
    CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
                  (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
    return(as.numeric(CV))
}
