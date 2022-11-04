sbc_report_list <- function(x,
                            meta_data,
                            groups,
                            MARGIN = 2,
                            show_global_count = T,
                            show_section_count = T,
                            show_section_percentage = F
) {
    # columna total al inicio
    total_first <- T

    groups <- groups %>% unique()
    # Columna que liga a las respuestas multiples
    id_column = names(x)[1]
    sort_column_name <- "Total"

    # renglones suman 100, columnas suman 100 o todo suma 100
    default_margin <- MARGIN
    remove_na <- T

    # Regresa la cuenta de casos por grupo
    get_section_count <-  function(section_data){
        for(group_idx in seq_along(groups)){
            tmp_ds <- section_data %>%
                group_by(!!sym(groups[group_idx])) %>%
                summarise(count = n()) %>%
                mutate(perc = count / sum(count) * 100) %>%
                arrange(!!sym(groups[group_idx]))
            names(tmp_ds)[1] <- "dummy_column"

            if(group_idx ==1){
                count_ds <- tmp_ds
                total_rows <- tmp_ds %>% sbc_get_col_sum_row()
                total_rows[1,1] <- "Total"
                #total_rows <- total_rows %>% sbc_transpose()

            }else{
                count_ds <- bind_rows (count_ds,tmp_ds)
            }

        }

        count_ds$dummy_column[is.na(count_ds$dummy_column)] <- "NA"
        if(class(count_ds$dummy_column)!= "factor"){
            count_ds$dummy_column[count_ds$dummy_column == ''] <- "no_value"
            count_ds$dummy_column  <- make.unique(count_ds$dummy_column, sep = "_")
        }

        total_rows <- bind_rows(count_ds, total_rows) %>% sbc_transpose()
        total_rows$concepto <- ""
        total_rows$concepto[1] <- "Casos"
        total_rows$concepto[2] <- "Porcentaje"
        names(total_rows) <- make.unique( names(total_rows), sep=" ")
        null_names <- names(total_rows) %>% is.na() %>% which()
        names(total_rows)[null_names] <- "NA"
        total_rows <- total_rows %>% select(concepto, everything())

        return(total_rows)
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


    if (show_global_count) {
        out_list <- vector("list", nrow(meta_data)+1+header_sections)
        names(out_list)[1] <- "Casos"
        global_section_count <- get_section_count(x)
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
    for (section_idx in seq_along(meta_data$column)) {
        #print(paste(section_idx,meta_data$column[section_idx],sep = " -> "))
        if(meta_data$section_label[section_idx] !=last_section ){
            out_list_idx <- get_first_null_index(out_list)
            last_section = meta_data$section_label[section_idx]
            out_list[[out_list_idx]] <-last_section
            names(out_list)[out_list_idx] <-last_section
        }


        names(out_list)[get_first_null_index(out_list)] <- meta_data$report_label[section_idx]
        section_data <- NULL
        # get section data
        sec_name <- meta_data$column[section_idx]

        out <-  meta_data$out[section_idx]
        type <-  meta_data$type[section_idx]
        label <-  meta_data$report_label[section_idx]
        sort_col <- meta_data$sort[section_idx]
        fn_params <- str_split(out,"_")[[1]]
        fn <- fn_params[1]
        fn_params <- fn_params[-1]
        section_ds <- x
        # for every group in the section
        if(type== "multiple"){
            section_ds <- section_ds %>%
                sbc_multi_column_to_df(idvar = id_column,
                                       pattern = paste(sec_name,"\\d",sep = ""),
                                       outColumn = "dummy_column" ) %>%
                left_join(section_ds %>%
                              select(id_column,
                                     groups))
            sec_name <- "dummy_column"
        }else{
            if(remove_na){
                section_ds <- section_ds %>% filter(!is.na(!!sym(sec_name)))
            }else{
                section_ds <- section_ds
            }
        }
        if(nrow(section_ds) == 0){
            emptyds <- tribble(~label,  "No hay datos" )
            names(emptyds)[1] <- label
            out_list[[get_first_null_index(out_list)]] <- emptyds
            next;
        }
        for (group_idx in seq_along(groups)) {
            #print(paste("grupo ->",groups[group_idx]))
            # Section totals just once
            if (group_idx == 1) {
                section_totals <- section_ds %>%
                    group_by(!!sym(sec_name)) %>%
                    summarise(count = n()) %>%
                    mutate(Total = count / sum(count) * 100) %>%
                    select(-count)
                section_totals[[1]]<- as.character(section_totals[[1]])
            }
            switch(
                fn,
                percentage = {
                    section_ds[[sec_name]] <- as.character(section_ds[[sec_name]])
                    group_data <- section_ds  %>%
                        sbc_table_2_var_perc(
                            colVar = groups[group_idx] ,
                            rowVar = sec_name,
                            MARGIN = default_margin
                        )

                },
                incidence = {
                    incidence_ds <- section_ds %>%
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

                    div_by <- incidence_ds[,1] %>% unique() %>% nrow()

                    if (group_idx == 1){
                        section_totals <- incidence_ds %>%
                            group_by(!!sym(sec_name)) %>%
                            summarise(n=n()) %>%
                            mutate( Total = n / div_by * 100)
                    }
                },
                count = {
                    print('percentage')
                    section_ds %>%
                        sbc_table_2_var_count(colVar = groups[1] ,
                                              rowVar = sec_name)
                    break;
                },
                sum_percentage = {
                    # case 'bar' here...
                    print('sum_percentage')
                    break;
                },
                summary = {
                    wmin <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,min)
                    wq1 <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,q1)
                    wmedian <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,median)
                    wmean <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,mean)
                    wq3 <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,q3)
                    wmax <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,max)
                    group_data <- bind_rows(wmin,wq1,wmedian,wmean,wq3, wmax)

                    tot_col_name <- names(group_data)[length(group_data)]
                    names(group_data)[names(group_data) ==""] <- "no_value"
                    names(group_data) <-  make.unique(names(group_data), sep = "_")
                    section_totals <- group_data %>% select(1,tot_col_name) %>% rename(Total = tot_col_name)
                    group_data <- group_data %>% select(-tot_col_name)
                    numeric_indicator[[get_first_null_index(out_list)]] <- TRUE
                },
                fn = {

                    for(i in seq_along(fn_params)) {
                        switch(
                            fn_params[i],
                            min = {
                                fn_out <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,min)
                            },
                            mean = {
                                fn_out <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,mean)
                            },
                            median = {
                                fn_out <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name, median)
                            },
                            max = {
                                fn_out <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,max)
                            },
                            q1 = {
                                fn_out <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,q1)
                            },
                            q3 = {
                                fn_out <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,q3)
                            }, sumpct = {
                                suma <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,sum)
                                tot_sum <- suma$`A total`[1]
                                suma[1,1] <- "sum_pct"
                                divide_by <- function(x, na.rm = FALSE) (x /tot_sum*100)
                                fn_out <- suma %>% mutate_if(is.numeric,divide_by)
                            }

                        )
                        names(fn_out)[length(names(fn_out))] <- "Total"
                        if(i==1){
                            group_data <- fn_out
                        }else{
                            group_data <- bind_rows(group_data,fn_out)
                        }
                    }
                    if (group_idx == 1) {
                        section_totals <- group_data %>% select(1,Total)
                    }


                    numeric_indicator[[get_first_null_index(out_list)]] <- TRUE
                },
                summary_perc = {
                    wmin <- sbc_get_num_col_fun(section_ds, groups[group_idx],sec_name,min)
                    wq1 <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,q1)
                    wmedian <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,median)
                    wmean <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,mean)
                    wq3 <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,q3)
                    wmax <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,max)
                    wsum <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,sum)
                    group_data <- bind_rows(wmin,wq1,wmedian,wmean,wq3, wmax,wsum)

                    tot_col_name <- names(group_data)[length(group_data)]
                    section_totals <- group_data %>% select(1,tot_col_name) %>% rename(Total = tot_col_name)
                    group_data <- group_data %>% select(-tot_col_name)
                    numeric_indicator[[get_first_null_index(out_list)]] <- TRUE
                },
                median_mean = {
                    wmedian <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,median)
                    wmean <- sbc_get_num_col_fun(section_ds,groups[group_idx],sec_name,mean)
                    group_data <- bind_rows(wmedian,wmean)
                    tot_col_name <- names(group_data)[length(group_data)]
                    section_totals <- group_data %>% select(1,tot_col_name) %>% rename(Total = tot_col_name)
                    group_data <- group_data %>% select(-tot_col_name)
                    numeric_indicator[[get_first_null_index(out_list)]] <- TRUE
                }
                ,
                {
                    print('default 124')
                    print(fn)
                    print(sec_name)



                    break;
                }
            )

            if(group_idx == 1){
                section_data <- group_data
            }else{
               # print(paste("nrow ->",nrow(group_data)))
                section_data <- full_join(section_data, group_data, by=names(section_data)[1])
            }

        }

        # Add total column to section data
        if(!is.null(section_data)){
            # Agregar total al inicio y al final
            if(total_first){
                section_data <- full_join(section_totals , section_data) %>% full_join( section_totals %>% rename(`Total `= Total))
            }else{
                section_data <- full_join(section_data, section_totals)
            }
            if(sort_col ){
                section_data <- section_data %>% arrange(desc(!!sym(sort_column_name)))
            }
            if(show_section_count){
                section_count <- get_section_count(section_ds)
                section_count <- section_count %>% mutate(`Total ` = Total) %>% select(1,Total, everything())
                names(section_count)[1] <- names(section_data)[1]
                if(!show_section_percentage){
                    section_count <- section_count[1,]
                }
                section_data <- bind_rows(section_count, section_data)

            }

            out_list[[get_first_null_index(out_list)]] <- section_data
        }
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



