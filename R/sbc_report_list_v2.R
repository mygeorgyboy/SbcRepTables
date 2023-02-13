#'  Regresa una lista con cruces de informaciín
#'
#' @param x El dataframe que se desea procesar
#' @param meta_data Los metadatos que describen a las variables que deseamos reportar
#' Generalmente se encuentran en un archivo de excel
#' @param groups Los grupos de columnas por los que queremos realizar los cruces
#' @param MARGIN 2 - Suma 100 vertical , 1. Suma 100 horizonta
#' @return Lista conteniendo los cruces
#'
#' @export
sbc_report_list_v2 <- function(x,
                               meta_data,
                               groups,
                               MARGIN = 2,
                               show_global_count = T,
                               show_section_count = T,
                               show_section_percentage = F,
                               total_first = T
) {
    options(dplyr.summarise.inform = FALSE)

    # Asignaciones iniciales
    {
        # columna total al inicio


        groups <- groups %>% unique()
        for(i in seq_along(groups)){
            grp <-groups[i]
            assert_that(x %>% filter(is.na(!!sym(grp))) %>% nrow() ==0,
                        msg =  paste("La columna '",grp,"' tiene valores nulos",sep = ""))
        }
 
 
        # Verifica que se pueda ejecutar en paralelo
        if (parallel::detectCores() < 4) {
            print("No se detectaron suficientes procesadores, se usara la version simple")
            return(sbc_report_list(x,
                meta_data,
                groups,
                MARGIN = MARGIN,
                show_global_count = show_global_count,
                show_section_count = show_section_count,
                show_section_percentage = show_section_percentage
            ))
        }


        # Columna que liga a las respuestas multiples
        # En éste caso es la primera columna
        id_column = names(x)[1]
        sort_column_name <- "Total"

        # renglones suman 100, columnas suman 100 o todo suma 100
        default_margin <- MARGIN
        remove_na <- T

        # calcula el número de cambios de seccion para agregar los encabezados
        # a la lista de salida
        header_sections <- 0
        last_section = '@1987%$%$·4'
        for(section in meta_data$section){
            if(section != last_section){
                last_section =section
                header_sections <- header_sections +1
            }
        }


        # Encontrar el ancho de los grupos
        for(i in seq_along(groups)) {
            w <- x %>% select(!!sym(groups[i])) %>% unique() %>% nrow()
            if (i == 1) {
                group_widths <- c(w)
            } else{
                group_widths <- c(group_widths, w)

            }
        }

        if(total_first){
            group_widths <- c(1,group_widths)
        }

    }

    # Declaración de funciones
    {


        #' Obtiene datos de grupo
        #'
        #' Para cada columna del dataframe, obtiene los casos y el porcentaje
        #' Estos datos van en el primer renglón de cada sección
        get_section_count_v3 <- function(x){
            z <- x %>%   map(table)
            z <-unlist(z)
            names(z) <- str_replace(names(z),"[^.]*.","")


            z <- tibble(dummy_column = names(z),count = z, perc = round(z/nrow(x)*100,1))

            z$dummy_column[is.na(z$dummy_column)] <- "NA"
            if(class(z$dummy_column)!= "factor"){
                z$dummy_column[z$dummy_column == ''] <- "no_value"
                z$dummy_column  <- make.unique(z$dummy_column, sep = "_")
            }

            z <- z %>%
                bind_rows(tibble(dummy_column = "Total",
                                 count = nrow(x),
                                 perc = 100))


            # Transponer
            col_names <- z[[1]]
            z <- as_tibble(t(z[,-1]))
            names(z) <- col_names

            z$concepto <- ""
            z$concepto[1] <- "Casos"
            z$concepto[2] <- "Porcentaje"
            names(z) <- make.unique(names(z), sep=" ")
            null_names <- names(z) %>% is.na() %>% which()
            names(z)[null_names] <- "NA"
            z <- z %>% select(concepto, everything())
            return(z)
        }



        # regresa el indice del primer elemento nulo de una lista
        # se usa para insertar las secciones a la lista de salida
        get_first_null_index <- function(x){
            for(i in 1:length(x)){
                if(is.null(x[[i]])){
                    break;
                }
            }
            return(i)
        }

        # Esta es la función principal, que se encarga de cruzar todos los datos
        get_sections_data <-
            function(section_idx,input_df) {
                print(paste(section_idx, meta_data$column[section_idx]))


                section_data <- NULL
                # get section data
                sec_name <- meta_data$column[section_idx]

                out <-  meta_data$out[section_idx]
                type <-  meta_data$type[section_idx]
                label <-  meta_data$report_label[section_idx]
                sort_col <- meta_data$sort[section_idx]
                fn_params <- str_split(out, "_")[[1]]
                fn <- fn_params[1]
                fn_params <- fn_params[-1]
                #input_df <- x
                # for every group in the section
                if (type == "multiple") {
                    input_df <- input_df %>%
                        sbc_multi_column_to_df(
                            idvar = id_column,
                            pattern = paste(sec_name, "\\d", sep = ""),
                            outColumn = "dummy_column"
                        ) %>%
                        left_join(input_df %>%
                                      select(id_column,
                                             groups))
                    sec_name <- "dummy_column"
                } else{
                    if (remove_na) {
                        input_df <- input_df %>% filter(!is.na(!!sym(sec_name)))
                    } else{
                        input_df <- input_df
                    }
                }
                if (nrow(input_df) == 0) {
                    emptyds <- tribble( ~ label,  "No hay datos")
                    return(emptyds)
                }


                section_data_list  <- vector("list", length(groups))
                # Para cada grupo
                for (group_idx in seq_along(groups)) {
                    # Calcula el total de la seciín en porcentaje al inicio de cada seccion
                    if (group_idx == 1) {
                        section_totals <- input_df %>%
                            group_by(!!sym(sec_name)) %>%
                            summarise(count = n()) %>%
                            mutate(Total = count / sum(count) * 100) %>%
                            select(-count)
                        section_totals[[1]] <- as.character(section_totals[[1]])
                    }

                    switch(
                        fn,

                        percentage = {
                            input_df[[sec_name]] <- as.character(input_df[[sec_name]])
                            group_data <- input_df  %>%
                                sbc_table_2_var_perc(colVar = groups[group_idx] ,
                                                     rowVar = sec_name,
                                                     MARGIN = default_margin)

                        },
                        incidence = {
                            incidence_ds <- input_df %>%
                                select(names(.)[1],
                                       sec_name,
                                       groups[group_idx]) %>%
                                unique()
                            group_data <- incidence_ds  %>%
                                sbc_table_2_var_perc(
                                    colVar = groups[group_idx] ,
                                    rowVar = sec_name,
                                    MARGIN = default_margin,
                                    incidenceVariable = names(.)[1]
                                )

                            div_by <- incidence_ds[, 1] %>% unique() %>% nrow()

                            if (group_idx == 1) {
                                section_totals <- incidence_ds %>%
                                    group_by(!!sym(sec_name)) %>%
                                    summarise(n = n()) %>%
                                    mutate(Total = n / div_by * 100)
                            }
                        },
                        count = {
                            print('percentage')
                            input_df %>%
                                sbc_table_2_var_count(colVar = groups[1] ,
                                                      rowVar = sec_name)
                            break

                        },
                        sum_percentage = {
                            # case 'bar' here...
                            print('sum_percentage')
                            break

                        },
                        summary = {
                            wmin <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, min)
                            wq1 <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, q1)
                            wmedian <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, median)
                            wmean <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, mean)
                            wq3 <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, q3)
                            wmax <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, max)
                            group_data <-
                                bind_rows(wmin, wq1, wmedian, wmean, wq3, wmax)

                            tot_col_name <-
                                names(group_data)[length(group_data)]
                            names(group_data)[names(group_data) == ""] <-
                                "no_value"
                            names(group_data) <-
                                make.unique(names(group_data), sep = "_")
                            section_totals <-
                                group_data %>% select(1, tot_col_name) %>% rename(Total = tot_col_name)
                            group_data <- group_data %>% select(-tot_col_name)

                        },
                        fn = {
                            for (i in seq_along(fn_params)) {
                                switch(
                                    fn_params[i],
                                    min = {
                                        fn_out <-
                                            sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, min)
                                    },
                                    mean = {
                                        fn_out <-
                                            sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, mean)
                                    },
                                    median = {
                                        fn_out <-
                                            sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, median)
                                    },
                                    max = {
                                        fn_out <-
                                            sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, max)
                                    },
                                    q1 = {
                                        fn_out <-
                                            sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, q1)
                                    },
                                    q3 = {
                                        fn_out <-
                                            sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, q3)
                                    },
                                    sumpct = {
                                        suma <-
                                            sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, sum)
                                        tot_sum <- suma$`A total`[1]
                                        suma[1, 1] <- "sum_pct"
                                        divide_by <-
                                            function(x, na.rm = FALSE)
                                                (x / tot_sum * 100)
                                        fn_out <-
                                            suma %>% mutate_if(is.numeric, divide_by)
                                    }

                                )
                                names(fn_out)[length(names(fn_out))] <- "Total"
                                if (i == 1) {
                                    group_data <- fn_out
                                } else{
                                    group_data <- bind_rows(group_data, fn_out)
                                }
                            }
                            if (group_idx == 1) {
                                section_totals <- group_data %>% select(1, Total)
                            }



                        },
                        summary_perc = {
                            wmin <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, min)
                            wq1 <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, q1)
                            wmedian <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, median)
                            wmean <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, mean)
                            wq3 <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, q3)
                            wmax <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, max)
                            wsum <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, sum)
                            group_data <-
                                bind_rows(wmin, wq1, wmedian, wmean, wq3, wmax, wsum)

                            tot_col_name <-
                                names(group_data)[length(group_data)]
                            section_totals <-
                                group_data %>% select(1, tot_col_name) %>% rename(Total = tot_col_name)
                            group_data <- group_data %>% select(-tot_col_name)

                        },
                        median_mean = {
                            wmedian <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, median)
                            wmean <-
                                sbc_get_num_col_fun(input_df, groups[group_idx], sec_name, mean)
                            group_data <- bind_rows(wmedian, wmean)
                            tot_col_name <-
                                names(group_data)[length(group_data)]
                            section_totals <-
                                group_data %>% select(1, tot_col_name) %>% rename(Total = tot_col_name)
                            group_data <- group_data %>% select(-tot_col_name)

                        }
                        ,
                        {
                            print('default 124')
                            print(fn)
                            print(sec_name)



                            break

                        }
                    )

                    #if(group_idx == 1){
                    #    section_data <- group_data
                    #}else{
                    #    print("--->")
                    #    section_data <- full_join(section_data, group_data, by=names(section_data)[1])
                    #    print("<---")
                    #
                    #}
                    section_data_list[[group_idx]] <- group_data

                }
                #section_data <-
                #    suppressMessages(reduce(section_data_list, full_join, by = names(section_data)[1]))

                section_data <-
                    reduce(section_data_list, full_join, by = names(section_data)[1])

                # Add total column to section data
                if (!is.null(section_data)) {
                    # Agregar total al inicio y al final
                    if (total_first) {
                        section_data <-
                            suppressMessages(
                                full_join(section_totals , section_data) %>% full_join(section_totals %>% rename(`Total ` = Total))
                            )
                    } else{
                        section_data <- full_join(section_data, section_totals)
                    }
                    if (sort_col) {
                        section_data <-
                            section_data %>% arrange(desc(!!sym(sort_column_name)))
                    }
                    if (show_section_count) {
                        section_count <-
                            get_section_count_v3(input_df %>% select(all_of(groups)))
                        section_count <-
                            section_count %>% mutate(`Total ` = Total) %>% select(1, Total, everything())
                        names(section_count)[1] <- names(section_data)[1]
                        if (!show_section_percentage) {
                            section_count <- section_count[1, ]
                        }
                        section_data <- bind_rows(section_count, section_data)

                    }

                }
                return(section_data)
            }

    }



    # Llena la primera sección que contiuene el total de casos y los porcentajes
    if(show_global_count) {
        out_list <- vector("list", nrow(meta_data)+1+header_sections)
        names(out_list)[1] <- "Casos"
        global_section_count <- get_section_count_v3(x %>% select(all_of(groups)))

        if(total_first){
            global_section_count <- global_section_count %>% mutate(`Total ` = Total) %>% select(1,Total,everything())
        }
        out_list[[get_first_null_index(out_list)]] <- global_section_count
        numeric_indicator <- vector("logical",length = nrow(meta_data) +1+header_sections)
    } else{
        out_list <- vector("list", nrow(meta_data)+header_sections)
        numeric_indicator <- vector("logical",length = nrow(meta_data) +header_sections)
    }


    last_section = '@1987%$%$·4'
    # for each section in data
    # Las secciones son cada uno de los renglones del metadato


    lout <-
        mclapply(seq_along(meta_data$column),
                 get_sections_data,x,
                 mc.cores = parallel::detectCores() - 2)


    # Llena la primera sección que contiuene el total de casos y los porcentajes
    if(show_global_count) {
        out_list <- vector("list", nrow(meta_data)+1+header_sections)
        names(out_list)[1] <- "Casos"
        global_section_count <- get_section_count_v3(x %>% select(all_of(groups)))

        if(total_first){
            global_section_count <- global_section_count %>% mutate(`Total ` = Total) %>% select(1,Total,everything())
        }
        out_list[[get_first_null_index(out_list)]] <- global_section_count
    } else{
        out_list <- vector("list", nrow(meta_data)+header_sections)
        numeric_indicator <- vector("logical",length = nrow(meta_data) +header_sections)
    }

    last_section = '@1987%$%$·4'

    for (section_idx in seq_along(meta_data$column)) {

        out_list_idx <- get_first_null_index(out_list)


        # Si se cambia de sección Agregar el nombre de la sección a la salida como texto
        if(meta_data$section_label[section_idx] !=last_section ){
            last_section = meta_data$section_label[section_idx]
            out_list[[out_list_idx]] <-last_section
            names(out_list)[out_list_idx] <-last_section
            out_list_idx <- out_list_idx +1
        }
        out_list[[out_list_idx]] <- lout[[section_idx]]
        names(out_list)[out_list_idx] <- meta_data$report_label[section_idx]

    }

    # review sections and add missing columns (if )
    full_cols <- names(out_list[[1]])[-1]
    for (ds_num in 1:length(out_list)) {
        if(!is.null(out_list[[ds_num]])){
            if(sum(class(out_list[[ds_num]])=='character')==0){
            # if(class(out_list[[ds_num]])!='character'){

                cols_faltantes <- setdiff(full_cols,  names(out_list[[ds_num]]))
                if (!is.null(cols_faltantes)) {
                    for (faltante in cols_faltantes) {
                        out_list[[ds_num]][faltante] <- 0
                    }
                }
                out_list[[ds_num]] <-
                    out_list[[ds_num]]  %>% select(1,full_cols)
            }
        }
    }

    # add group widths to outlist
    attr(out_list ,"group_widths") <-group_widths
    attr(out_list ,"numeric_indicator") <- numeric_indicator
    return(out_list)
}

