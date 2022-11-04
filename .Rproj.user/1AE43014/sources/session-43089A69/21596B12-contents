sbc_html_heatmap_v2  <- function(x, noFormatRows = c("Casos") ,
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

    get_row_data <- function(k){
        tdata <- ""
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

                if (class(value) %in% c("numeric")) {
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
                gline <- paste(gline, "<td class='group_header' colspan='", group_widths[m]-1 ,"' >",group_names[m],"</td>",sep = "")
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
                hline <- paste(hline,"<th class='group-separator'></th>")
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
    html_data <- paste("<head> <meta charset='UTF-8'><style>",hm_style,"</style></head>",html_data)
    html_data <- paste("<html id='all-page'>",html_data,"</html>")


    return (html_data)
}
