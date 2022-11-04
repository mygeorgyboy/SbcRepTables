sbc_html_heatmap_v3  <- function(x, noFormatRows = c("Casos") ,
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
                                 groupSeparator = F){
    datos <- x

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



    datos <- x
    tdata <- ""

    get_row_data <- function(k){
        tdata <- ""
        tabla <- datos[[k]]
        if(is.null(tabla)){
            next
        }
        if(class(tabla) == 'character'){
            span_length <- length(names(datos[[1]]))
            if(groupSeparator){
                span_length <- span_length +length(group_separators)
            }

            tdata <- paste(tdata,"<tr><td class='rpt-shdr' colspan='",span_length,"'>", tabla,"</td></tr>", sep = "")
            return(tdata)
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
                    "<tr><td class='shdr' colspan='",
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
                    hline <- paste(hline,"<td class='shdr'>",names(datos)[k],"</td>")
                }else{
                    if(groupSeparator && i %in% group_separators){
                        hline <- paste(hline,"<td class='gs'></td>")
                    }
                    hline <- paste(hline,"<td class='gh'>",col_name,"</td>")
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
                    class_text <- "rn"
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
                    class_text <- "bldcol"
                }else{
                    class_text <- "cd"
                }
                td <- ""

                if(is.na(value)){
                    value = ""
                }

                if (class(value) %in% c("numeric")) {
                    value <- round(value,2)

                    if(!is.na(value) & value>45 & value<= 100 ){
                        text_color = 'twt'
                    }else{
                        text_color = 'tblk'
                    }

                    #if(!numeric_section){

                    if(!numeric_row){
                        if(row_name %in% noFormatRows){
                            background_color <- "btrns"
                            text_color =  'tgry'
                        }else{
                            if(plasma_row){
                                background_color <- paste0('pbc',as.integer(value))
                            }else{
                                background_color <- paste0('bc',as.integer(value))
                            }
                        }
                        td <-
                            paste(
                                "<td  class='",class_text, background_color,text_color, "'>",
                                round(value, 2),
                                "</td>",
                                sep = " "
                            )
                    }else{
                        if(value ==round(row_max[i],2)){
                            background_color <- "bmax"
                            text_color = 'tgry'
                        }else  if(value == round(row_min[i],2)){
                            background_color <- "bmin"
                            text_color = 'tgry'
                        }else{
                            background_color <- "btrns"
                            text_color <- "tgry"
                        }

                        if(row_name %in% noFormatRows){
                            background_color <- "btrns"
                        }
                        td <-
                            paste(
                                "<td  class='",class_text, background_color, text_color,"'>",
                                round(value, 2),
                                "</td>",
                                sep = " "
                            )
                    }

                }else{
                    if(is.na(value)){
                        value = ''
                    }
                    td <- paste("<td class='",class_text,"'>", value, "</td>", sep = "")
                }

                if(groupSeparator &&j %in% group_separators){
                    sep_cel <- paste("<td class='gs'></td>")
                    tr <-  paste(tr,sep_cel, td)
                }else{
                    tr <-  paste(tr, td)
                }

            }
            tdata <- paste(tdata, tag("tr", tr))
        }
        return(tdata)
    }

    tdata <-
        mclapply(seq_along(datos),
                 get_row_data,
                 mc.cores = parallel::detectCores() - 2)

    tdata <- unlist(tdata)  %>%  paste(collapse = ' ')


    #agregar grupos
    if(showGroups){
        if(has_groups){
            gline <- "<tr><td></td>" # La prmera columna libre
            #gline <- paste(gline, "<td class='group_header' colspan='", group_widths[1]-1 ,"' >",group_names[1],"</td>",sep = "")

            for(m in seq_along(group_widths)){
                gline <- paste(gline, "<td class='gh' colspan='", group_widths[m]-1 ,"' >",group_names[m],"</td>",sep = "")
            }
            gline <- paste(gline, "</tr>",sep = "")
            tdata <- paste(gline,tdata)
        }
    }


    #agregar titulos
    hline <- ""
    hnames <- names(datos[[1]])
    hnames[1] <- ""
    for(i in seq_along(hnames)){
        col_name <- hnames[i]

        if(i==1){
            hline <- paste(hline,"<th></th>")
        }else{
            if(groupSeparator && i %in% group_separators){
                hline <- paste(hline,"<th class='gs'></th>")
            }
            hline <- paste(hline,"<th class='rotate'><div><span>",col_name,"</span></div></th>")
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
    html_data <- paste("<head> <meta charset='UTF-8'><style>",hm_style_min,"</style></head>",html_data)
    html_data <- paste("<html id='all-page'>",html_data,"</html>")


    return (html_data)
}
