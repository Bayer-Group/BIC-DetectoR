#' A function to plot the double_dot_plot / Relative risk / adjusted p-values and p-values
#'
#'@param data Result data set from function calcDoubleDotPlot.
#'@param variable AEDECOD or AEBODSYS.
#'@param display RR or RD.
#'@param adjustment FDR or DFDR or BB.
#'@param numberAEs Number of Adverse Events shown (default = 50, have to be a even number)
#'@param label Label variable.
#'@param color_theme Select color theme.
#'@param measure Measure variable (proportions or incidence rates)
#'
#'@return An object with 4 ggplot graphics
#'
double_dot_plot_p1 <- function(
  data = results2_data,
  adjustment,
  variable,
  display, 
  numberAEs = 50,
  label = PT,
  color_theme = "dark",
  measure = "proportions"
){
  
  xaxis_label <- ifelse(measure == "proportions", "Proportion", "Incidence rates (per 100 patient years)")
  
  p1 <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = prop,
      y = !!rlang::sym(label),
      colour = TRTA_DetectoR, 
      shape = TRTA_DetectoR
    ), 
    colors = c('red','blue')) +
    ggplot2::theme_minimal() +
    ggplot2::geom_point(size = 5) +
    ggplot2::scale_colour_manual(values = c('#EE2C2CC8','#1E90FFC8')) + # add hexcode for transparancy
    ggplot2::scale_shape_manual(values = c(17, 16)) +
    ggplot2::scale_x_continuous(labels = 
      ifelse(
        measure == "proportions",
        scales::percent, 
        scales::label_percent(suffix="")
      ),
      sec.axis = ggplot2::dup_axis()
    ) +
    ggplot2::xlab(xaxis_label) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark","#353535","#dbdbdb"))) +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      axis.text = ggplot2::element_text(colour = ifelse(color_theme == "dark","white","black")),
      panel.grid.major = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf")),
      panel.grid.minor = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf"), size = 0.25),
      text = ggplot2::element_text(
        family = "",
        face = "plain",
        colour = ifelse(color_theme == "dark","white","black"),
        size = 12,
        lineheight = 0.9, 
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = ggplot2::margin(),
        debug = FALSE
      )
    ) 
  p1
}

double_dot_plot_p2 <- function(
  data = results2_data,
  adjustment,
  variable,
  display,
  numberAEs = 50,
  alpha = 95,
  color_theme = "dark"
){
  if (display == "RD") {
    data$rd <- data$rd * 100
    data$rd.ucl <- data$rd.ucl * 100
    data$rd.lcl <- data$rd.lcl * 100
    p2 <- ggplot2::ggplot(
      data, 
      ggplot2::aes(
        y = PT,
        x = !! rlang::sym(tolower(display)),
        xmin =!! rlang::sym(paste0(tolower(display),".lcl")),
        xmax =!! rlang::sym(paste0(tolower(display),".ucl"))
      )
    ) +
      ggplot2::theme_minimal() +
      ggplot2::geom_vline(xintercept = ifelse(display == "RR", 1, 0), linetype = 3, colour = ifelse(color_theme == "dark","white","black")) +
      ggplot2::geom_point(size = 2.5, na.rm = TRUE, color =ifelse(color_theme == "dark","white","black")) +
      ggplot2::geom_errorbarh(na.rm = TRUE, color = ifelse(color_theme == "dark","white","black")) +
      ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis()) +
      ggplot2::xlab("Risk difference (%)") +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark","#353535","#dbdbdb"))) +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text=ggplot2::element_text(colour = ifelse(color_theme == "dark","white","black")),
        panel.grid.major = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf")),
        panel.grid.minor = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf"), size = 0.25),
        text = ggplot2::element_text(
          family = "",
          face = "plain",
          colour = ifelse(color_theme == "dark","white","black"),
          size = 12,
          lineheight = 0.9,  
          hjust = 0.5,
          vjust = 0.5,
          angle = 0,
          margin = ggplot2::margin(), 
          debug = FALSE
        )
      ) 
  }
  if (display == "RR") {
    
    p2 <- ggplot2::ggplot(
      data, 
      ggplot2::aes(
        y = PT,
        x = !! rlang::sym(tolower(display)),
        xmin =!! rlang::sym(paste0(tolower(display),".lcl")),
        xmax =!! rlang::sym(paste0(tolower(display),".ucl"))
      )
    ) +
      ggplot2::theme_minimal() +
      ggplot2::geom_vline(xintercept = ifelse(display == "RR",1,0), linetype = 3, colour = ifelse(color_theme == "dark","white","black")) +
      ggplot2::geom_point(size = 2.5, na.rm = TRUE, color = ifelse(color_theme == "dark","white","black")) +
      ggplot2::geom_errorbarh(na.rm = TRUE, color = ifelse(color_theme == "dark","white","black")) +
      ggplot2::xlab("Relative Risk") +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark","#353535","#dbdbdb"))) +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text=ggplot2::element_text(colour = ifelse(color_theme == "dark","white","black")),
        panel.grid.major = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf")),
        panel.grid.minor = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf"), size = 0.25),
        text = ggplot2::element_text(
          family = "",
          face = "plain",
          colour = ifelse(color_theme == "dark","white","black"), 
          size = 12,
          lineheight = 0.9, 
          hjust = 0.5,
          vjust = 0.5, 
          angle = 0,
          margin = ggplot2::margin(), 
          debug = FALSE
        )
      ) 
    
    tmp_range <- exp(mean(log(range(c(data$rr.lcl, data$rr.ucl), na.rm = TRUE))))
    
    p2 <- p2  + 
      ggplot2::scale_x_log10(
        breaks = c(0.1,0.5,1,2,10),
        sec.axis = ggplot2::dup_axis()
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = PT, 
          x = tmp_range,
          label = ifelse(is.na(rr.missing_label),"",rr.missing_label)
        )
      )
  }
  
  if (display != "RR" && display !="RD") {
    print("wrong display input")
    p2 <- qplot(1, 1, label = "Wrong display input", geom = "text")
  }
  
  p2
}

double_dot_plot_p3 <- function(
  data = results2_data,
  adjustment,
  variable,
  display,
  numberAEs = 50,
  order_by = "p-value",
  color_theme = "dark"
){
  
  numberAEs <- numberAEs * 2
  
  if (order_by == "p-value") {
    if (adjustment == "DFDR") {
      if (!is.null(variable) & variable == "AEBODSYS") {
        var1 = "p"
        var2 <- "rr.missing_label"
      } else if (!is.null(variable) & variable == "AEDECOD") {
        var1 <- "p"
        var2 <- "DFDR"
      } else if (!is.null(variable) & variable == "MLG_label") {
        var1 <- "p"
        var2 <- "DFDR"
      }
    } else {
      var1 <- "p"
      var2 <- "p_adj"
    }
    data <- data %>% 
      dplyr::arrange(!!rlang::sym(var2), !!rlang::sym(var1))
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        .[1:numberAEs,]
    }
  } else if (order_by == "effect") {
    data <- data %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(tolower(display))))
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        .[1:numberAEs,]
    }
  }
  
  if(color_theme == "report") {
    data$col.p_adj <- ifelse(data$col.p_adj=="white", "black", data$col.p_adj)
  }
  
  #### Third plot (p3) ####
  if (adjustment == "FDR" || variable !="AEBODSYS"){
    p3 <- ggplot2::ggplot(
      data, 
      ggplot2::aes(y = PT,
                   x = 1,
                   label = if (adjustment == "FDR") {
                     ifelse(is.na(p_adj.value), "", p_adj.value)
                   } else if (adjustment == "DFDR") {
                     ifelse(is.na(DFDR.value),"", DFDR.value)
                   } else {
                     BB.value.lab
                   }
      )
    ) +
      ggplot2::theme_minimal() +
      ggplot2::geom_text(color = data$col.p_adj) +
      ggplot2::scale_colour_manual(values =  ifelse(c(color_theme == "dark",color_theme == "dark"), c( col_red = "#FF5733","white" = "#ffffff"),c(col_red = "#FF5733","black" = "#000000"))) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(~.)) +
      ggplot2::xlab(ifelse(color_theme == "dark", "", paste0(adjustment," adjusted"))) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark", "#353535", "#dbdbdb"))) +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.position = "none",
        axis.text = ggplot2::element_text(colour = ifelse(color_theme == "dark","white","black")),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        text = ggplot2::element_text(
          family = "", 
          face = "plain",
          colour = ifelse(color_theme == "dark","white","black"), 
          size = 12,
          lineheight = 0.9,  
          hjust = 0.5,
          vjust = 0.5, 
          angle = 0,
          margin = ggplot2::margin(),
          debug = FALSE
        )
      )
    
  } else if (variable == "AEBODSYS" && adjustment != "FDR") {
    text <- paste(
      "Double False ","\n",
      "Discovery Rate","\n",
      "adjustment does","\n", 
      "not work" ,"\n",
      "with Variable  \n",
      "'System Organ" ,"\n",
      "Class'!" ," \n",
      "Change it to" ,"\n", 
      "'Preferred Terms'","\n",
      "or 'Medical" ,"\n",
      "labeling grouping'","\n",
      "(if applicable)","\n",
      "for adjustment"
    )
    p3 <- ggplot2::ggplot(
      data, 
      ggplot2::aes(
        y = PT,
        x = 1,
        label = "",
        colour = col.p_adj
      )
    ) +
      ggplot2::theme_minimal() +
      ggplot2::geom_text() +
      ggplot2::annotate(
        "text",
        y = levels(data$PT)[round(length(data$PT) / 4)],
        x = 1,
        label = text,
        angle = 0,
        colour = ifelse(color_theme == "dark","white","black")
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(~.)) +
      ggplot2::xlab(ifelse(color_theme == "dark", "", paste0(adjustment, " adjusted"))) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark","#353535","#dbdbdb"))) +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.position = "none",
        axis.text=ggplot2::element_text(colour = ifelse(color_theme == "dark","white","black")),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        text = ggplot2::element_text(
          family = "", 
          face = "plain",
          colour = ifelse(color_theme == "dark","white","black"),
          size = 12,
          lineheight = 0.9,  
          hjust = 0.5,
          vjust = 0.5,
          angle = 0,
          margin = ggplot2::margin(),
          debug = FALSE
        )
      )
    
  } else {
    print("Wrong adjustment input")
    p3 = qplot(1, 1, label = "Wrong adjustment input", geom = "text")
  }
  p3
}


double_dot_plot_p4 <- function(
  data = results2_data,
  adjustment, 
  variable,
  display,
  study_strat = "None",
  numberAEs = 50,
  order_by = "p-value",
  color_theme = "dark"
) {
  
  numberAEs <- numberAEs * 2
  
  if (order_by == "p-value") {
    if (adjustment == "DFDR") {
      if (!is.null(variable) & variable == "AEBODSYS") {
        var1 = "p"
        var2 <- "rr.missing_label"
      } else if (!is.null(variable) & variable == "AEDECOD") {
        var1 <- "p"
        var2 <- "DFDR"
      } else if (!is.null(variable) & variable == "MLG_label") {
        var1 <- "p"
        var2 <- "DFDR"
      }
    } else {
      var1 <- "p"
      var2 <- "p_adj"
    }
    data <- data %>% 
      dplyr::arrange(!!rlang::sym(var2), !!rlang::sym(var1))
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        .[1:numberAEs,]
    }
  } else if (order_by == "effect") {
    data <- data %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(tolower(display))))
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        .[1:numberAEs,]
    }
  }
  
  if (color_theme == "report") {
    data$col.p <- ifelse(data$col.p == "white","black",data$col.p)
  }
  
  p4 <- ggplot2::ggplot(
    data, 
    ggplot2::aes(
      y = PT, 
      x = 1, 
      label = ifelse(is.na(p.value.lab), "", p.value.lab)
    )
  ) +
    ggplot2::theme_minimal() +
    ggplot2::geom_text(color = data$col.p) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(values =  ifelse(c(color_theme == "dark",color_theme == "dark"), c( col_red = "#FF5733","white" = "#ffffff"),c(col_red = "#FF5733","black" = "#000000"))) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(~.)) +
    ggplot2::xlab(ifelse(color_theme == "dark", "", ifelse(study_strat!="None","Study-size adjusted p-values","Fishers exact p-values"))) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark","#353535","#dbdbdb"))) +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      axis.text = ggplot2::element_text(colour = ifelse(color_theme == "dark","white","black")),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(
        family = "",
        face = "plain",
        colour = ifelse(color_theme == "dark","white","black"), 
        size = 12,
        lineheight = 0.9, 
        hjust = 0.5,
        vjust = 0.5, 
        angle = 0,
        margin = ggplot2::margin(),
        debug = FALSE
      )
    )
  p4
}

#' A function to plot the legend of the double dot plot
#'
#'@param data Data set.
#'@param adjustment If adjustment should be peformed ("DFDR"/"FDR")
#'@param variable A character string with the variable name ("AEBODSYS"/"AEDECOD")
#'@param display Order to display.
#'@param numberAEs Number of adverse events shown.
#'@param color_theme Theme for the legend.
#'
#'@return An object with legend graphics

double_dot_plot_legend <- function(
  data = results2_data,
  adjustment, 
  variable, 
  display,
  numberAEs = 50, 
  color_theme = "dark"
) {
  

  numberAEs <- numberAEs * 2
  if (adjustment == "DFDR" & 
      variable != "AEBODSYS" &
      order_by == "p-value") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(DFDR) %>%
        .[1:numberAEs,]
    }
  } else if (adjustment == "FDR" & 
             variable != "AEBODSYS" &
             order_by == "p-value") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(p_adj.value) %>%
        .[1:numberAEs,]
    }
  } else if (adjustment == "DFDR" & 
             variable != "AEBODSYS" &
             order_by == "effect") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(tolower(display)))) %>%
        .[1:numberAEs,]
    }
  } else if (adjustment == "FDR" & 
             variable != "AEBODSYS" &
             order_by == "effect") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(tolower(display)))) %>%
        .[1:numberAEs,]
    }
  } else if (adjustment == "DFDR" & 
             variable == "AEBODSYS" &
             order_by == "p-value") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(p) %>%
        .[1:numberAEs,]
    }
  } else if (adjustment == "FDR" & 
             variable == "AEBODSYS" &
             order_by == "p-value") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(p_adj.value) %>%
        .[1:numberAEs,]
    }
  } else if (adjustment == "DFDR" & 
             variable == "AEBODSYS" &
             order_by == "effect") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(tolower(display)))) %>%
        .[1:numberAEs,]
    }
  } else if (adjustment == "FDR" & 
             variable == "AEBODSYS" &
             order_by == "effect") {
    if (dim(data)[1] >= numberAEs) {
      data <- data %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(tolower(display)))) %>%
        .[1:numberAEs,]
    }
  }
  
  
  g_legend <- function(a.gplot){
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  p1 <- ggplot2::ggplot(
    data, 
    ggplot2::aes(
      x = prop, 
      y = PT, 
      colour = TRTA_DetectoR, 
      shape = TRTA_DetectoR
    ), colors = c('blue','red')
  ) +
    ggplot2::theme_minimal() +
    ggplot2::geom_point(size = 5) +
    ggplot2::scale_colour_manual(values = c('#1E90FFC8','#EE2C2CC8')) + # add 99 at hexcode for 60% transparancy
    ggplot2::scale_shape_manual(values = c(17, 16)) +
    ggplot2::scale_x_continuous(labels = scales::percent, sec.axis = ggplot2::dup_axis()) +
    ggplot2::xlab("Proportion") +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark","#353535","#dbdbdb"))) +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      axis.text = ggplot2::element_text(colour = ifelse(color_theme == "dark", "white", "black")),
      panel.grid.major = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf")),
      panel.grid.minor = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf"), size = 0.25),
      text = ggplot2::element_text(
        family = "", 
        face = "plain",
        colour = ifelse(color_theme == "dark","white","black"), 
        size = 12,
        lineheight = 0.9,  
        hjust = 0.5,
        vjust = 0.5, 
        angle = 0,
        margin = ggplot2::margin(), 
        debug = FALSE
      )
    )
  
  g_legend(p1 + 
             ggplot2::theme(
               plot.background = ggplot2::element_rect(fill = ifelse(color_theme == "dark","#353535","#dbdbdb")),
               axis.text = ggplot2::element_text(colour = ifelse(color_theme == "dark","white","black")),
               panel.grid.major = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf")),
               panel.grid.minor = ggplot2::element_line(colour = ifelse(color_theme == "dark","#424242","#bfbfbf"), size = 0.25),
               text = ggplot2::element_text(
                 family = "", 
                 face = "plain",
                 colour = ifelse(color_theme == "dark","white","black"),
                 size = 12,
                 lineheight = 0.9,  
                 hjust = 0.5,
                 vjust = 0.5, 
                 angle = 0,
                 margin = ggplot2::margin(), 
                 debug = FALSE
               ),
               legend.position = "bottom",
               legend.text = ggplot2::element_text(size = 14),
               legend.background = ggplot2::element_rect(colour = ifelse(color_theme == "dark","#353535","#dbdbdb")),
               legend.title = ggplot2::element_blank()
             )
  )
}
