#### CALCDOUBLEDOTPLOT FUNCTION ####
#' Calculate all Variables for the double_dot_plot function
#'
#'@param data_adsl Subject level data set.
#'@param data_adae Adverse event data set.
#'@param variable AEDECOD or AEBODSYS or MLG.
#'@param display Display option RR (relative risks) or RD (risk differences).
#'@param adjustment Adjustment variable FDR or DFDR or BB.
#'@param order_by Order of adverse events shown in double dot plot (p-value or effect).
#'@param study_strat If study stratification should be used (TRUE or FALSE).
#'@param n.iterations Number of iterations for BB.
#'@param n.burn.in Number of burn in phase for BB.f
#'@param seed Random Number Generator number.
#'@param alternative A character string describing the alternative hypotesis (one or two sided)
#'@param heatmap Transform returned data into structure of the heatmap
#'@param alpha Alpha level of the test
#'@param filter A character string if filter on adverse events should be performed
#'@param measure Measurement used (proportions or incidence rates)
#'@importFrom rlang :=
#'@importFrom dplyr %>%
#'
#'@return results2_data
#'@export
calcDoubleDotPlot2 <- function(
  data_adsl,
  data_adae,
  variable = "AEDECOD",
  display = "RR",
  adjustment = "DFDR",
  order_by = "p-value",
  study_strat = "None",
  n.iterations = 100000,
  n.burn.in = 5000,
  seed = 2006,
  alternative = "two.sided",
  heatmap = FALSE,
  alpha = 0.05,
  filter = "nomethod",
  measure = "proportions"
) {
  
  suppressWarnings(
    comb_data <- dplyr::left_join(data_adsl, data_adae, by = c("STUDYID","USUBJID")) %>% 
      dplyr::filter(!is.na(!!rlang::sym(variable))) %>% 
      dplyr::filter(!!rlang::sym(variable) != "")
  )
  empt <- FALSE
  
  # Create data set bign which count the total of Comparator and Verum
  bign <- data_adsl %>%
    dplyr::group_by(TRTA_DetectoR) %>%
    dplyr::summarise(bign = dplyr::n_distinct(USUBJID),.groups = 'drop') %>%
    dplyr::select(bign, TRTA_DetectoR)
  
  # Value for creating random numbers
  set.seed(seed)
  
  # Create a grid with all AEDECOD/AEBODSYS in the data and TRTA_DetectoR values
  exp_grid_new <- tidyr::crossing(!!variable := unique(dplyr::pull(data_adae, variable)),
                                  TRTA_DetectoR = comb_data$TRTA_DetectoR)
  suppressWarnings(
    double_dot_plot_data <-
      comb_data %>%
      dplyr::group_by(!!rlang::sym(variable), TRTA_DetectoR) %>%
      dplyr::summarise(count2 = dplyr::n_distinct(USUBJID),.groups = 'drop_last') %>%
      dplyr::full_join(exp_grid_new, by = c(variable,"TRTA_DetectoR")) %>%
      dplyr::full_join(bign, by = "TRTA_DetectoR") %>%
      dplyr::filter(!is.na(!! rlang::sym(variable))) %>%
      dplyr::mutate(count = ifelse(is.na(count2), 0, count2)) %>%
      dplyr::mutate(prop = count / bign) %>%
      dplyr::mutate(no_count = bign - count) %>%
      dplyr::select(-count2) %>%
      dplyr::mutate(TRTA_DetectoR_bign = paste0(TRTA_DetectoR," (N=", bign,")")))
  
  if(measure == "incidence rates") {
     adae_SOC_subset <- data_adae %>%
      dplyr::select(USUBJID,AEDECOD,AAESDURN,!!rlang::sym(variable))
    adae_SOC_first <- adae_SOC_subset %>%
      dplyr::group_by(USUBJID,!!rlang::sym(variable)) %>% dplyr::summarize(time = min(AAESDURN)) %>% dplyr::mutate(event=1)

    data_adsl_reduced <- data_adsl %>% 
      dplyr::select(USUBJID, TRTA_DetectoR, DUREXP)
    adae_SOC_summ <- c()
    unique_aes <- unique(data_adae[[variable]])
    length_aes <- length(unique(data_adae[[variable]]))
    
    tmp <- dplyr::full_join(data_adsl_reduced,unique_aes, by = character(), copy = TRUE) %>%
      dplyr::rename(!!rlang::sym(variable):="y")
     
    tmp <- tmp %>%
      dplyr::left_join(adae_SOC_first, by = c("USUBJID",variable))
     
    tmp <- tmp %>%
      dplyr::group_by(!!rlang::sym(variable),TRTA_DetectoR)
    tmp <- tmp %>%
        dplyr::mutate(
          event = ifelse(is.na(event),0,1),
          time = ifelse(is.na(time),DUREXP,time)
        ) %>%
        dplyr::filter(!is.na(TRTA_DetectoR)) %>%
        dplyr::group_by(!!rlang::sym(variable),TRTA_DetectoR) %>%
        dplyr::summarize(
          count = sum(event),
          bign = length(event),
          pattime = sum(time),
          prop = 365.25*100*count/pattime,
          no_count = bign-count,
          .groups="drop_last"
    ) %>% dplyr::mutate(TRTA_DetectoR_bign = paste0(TRTA_DetectoR," (N=", bign,")"))
    double_dot_plot_data <- tmp
  }

  ## end function when filter creates empty data set
  if (dim(double_dot_plot_data)[1] == 0) {
    results2_data <- NULL
  } else {  
    
    if (filter == "onepercent") {
      tmp <- double_dot_plot_data %>%
        dplyr::group_by(!! rlang::sym(variable)) %>%
        dplyr::mutate(
          sum_count = sum(count),
          sum_bign = sum(bign),
          total_prop = sum_count/sum_bign
        ) %>%
        dplyr::ungroup()
      tmp <- tmp %>%
        dplyr::filter(total_prop >= 0.01)  
      double_dot_plot_data <- tmp %>%
        dplyr::select(-c(sum_count, sum_bign, total_prop)) 
    }
    
    if (filter == "min") {
      N_1 <- double_dot_plot_data %>%
        dplyr::ungroup() %>%
        dplyr::select(TRTA_DetectoR, bign) %>%
        dplyr::filter(TRTA_DetectoR == "Verum") %>%
        dplyr::distinct() %>%
        dplyr::pull(bign) %>%
        as.numeric()
      N_2 <- double_dot_plot_data %>%
        dplyr::ungroup() %>%
        dplyr::select(TRTA_DetectoR, bign) %>%
        dplyr::filter(TRTA_DetectoR == "Comparator") %>% 
        dplyr::distinct() %>% 
        dplyr::pull(bign) %>% 
        as.numeric()
      minE <- find.min.event(N1 = N_1, N2 = N_2, alternative = alternative, alpha = alpha)
      aeName <- double_dot_plot_data[stringr::str_detect(double_dot_plot_data $TRTA_DetectoR, "Verum"),] %>% 
        dplyr::filter(count >= minE) %>% 
        dplyr::pull(variable)
      double_dot_plot_data  <- double_dot_plot_data[double_dot_plot_data %>%
                                                      dplyr::pull(!!rlang::sym(variable)) %in% aeName, ] 
    }
    
    # Calculation of RR / RD and p-values based on the condition of variable study_strat {FALSE/TRUE}
    if (study_strat == "None") {
      if(measure == "proportions"){
        results_data <- fisher.and.rr_ip(ds = double_dot_plot_data, alternative = alternative, alph = alpha, vari = variable)
      } else {
        
        results_data <- fisher.and.rr_ir(ds = double_dot_plot_data, alternative = alternative, alph = alpha, vari = variable)
      }
      if (dim(results_data)[1] == 0) {
        return(NULL)
      }
    } else if (study_strat != "None") {
      
      results_data <- study.strat.RR.and.RD(
        comb_data = comb_data,
        variable = variable,
        display = display,
        alpha = alpha,
        stratify_variable = study_strat
      )
      
    }
    
    # Benjamini-Hochberg procedure adjusted p-values
    results_data <- tibble::as_tibble(c212::c212.BH.adjust.pvals(as.data.frame(results_data)))
    suppressWarnings(
      results2_data <- results_data %>%
        dplyr::mutate(
          p_adj.value = ifelse(
            round(results_data$p_adj,4) == 0, 
            "<0.0001",
            format(round(results_data$p_adj,4), scientific = FALSE)
          ),
          col.p = ifelse(
            results_data$p < alpha,
            col_red,
            "white"
          ),
          col.p_adj = ifelse(
            results_data$p_adj < alpha, 
            col_red, 
            "white"
          )
        ) %>%
        dplyr::inner_join(double_dot_plot_data, by = variable)
    )
    if (adjustment == "DFDR") {
      if (variable %in% c("AEDECOD", "MLG_label")) {
        if(variable == "AEDECOD") {
          suppressWarnings(
            ADAE.tier2 <- results2_data %>%
              dplyr::select(AEDECOD,count,bign,p) %>%
              dplyr::group_by(AEDECOD) %>%
              dplyr::summarise(
                Count = sum(count),
                Total = sum(bign),
                p = as.numeric(p[1]),
                .groups = 'drop'
              ) %>%
              dplyr::mutate(prop = Count/Total)
          )
          suppressWarnings(
            ADAE.tier2 <- ADAE.tier2 %>%
              dplyr::left_join(
                comb_data %>%
                  dplyr::select(AEDECOD, AEBODSYS) %>%
                  dplyr::distinct(),
                by = 'AEDECOD'
              )
          )
          
          p_rep_adj <- ADAE.tier2 %>%
            dplyr::group_by(AEBODSYS) %>%
            tidyr::nest() %>%
            dplyr::mutate(data = purrr::map(data, ~ FDR.fun(.))) %>%
            dplyr::mutate(data = purrr::map(data, ~ dplyr::pull(., FDR) %>% min())) %>%
            tidyr::unnest_legacy() %>%
            dplyr::mutate(p = data) %>%
            FDR.fun(.) %>%
            dplyr::select(AEBODSYS,FDR)
          suppressWarnings(
            tmp <- ADAE.tier2 %>%
              dplyr::full_join(p_rep_adj , by = 'AEBODSYS') %>%
              dplyr::filter(FDR <= alpha) %>%
              dplyr::select(-c(FDR,prop)) %>%
              FDR.fun(.) %>%
              dplyr::rename(DFDR = FDR)
          )
          if (tmp %>%
              dim() %>%
              .[1] > 0) {
            suppressWarnings(
              results2_data <-  dplyr::full_join(tmp, results2_data, by = c(variable, "p")) %>%
                dplyr::mutate(
                  DFDR.value = dplyr::if_else(
                    is.na(.$DFDR), 
                    "",
                    ifelse(.$DFDR < 0.0001 ,
                           "<0.0001",
                           as.character(format(round(.$DFDR, 4), scientific = FALSE))
                    )
                  ),
                  DFDR.value.lab = .$DFDR
                )
            )
          } else {
            empt <- TRUE
            results2_data <- results2_data %>%
              dplyr::mutate(DFDR = as.numeric(NA))
            results2_data[,"DFDR.value"] <- ""
            results2_data <- results2_data %>%
              dplyr::mutate(DFDR.value.lab = as.numeric(NA))
          }
        } else if (variable == "MLG_label") {
          suppressWarnings(
            ADAE.tier2 <- results2_data %>%
              dplyr::select(MLG_label,count,bign,p) %>%
              dplyr::group_by(MLG_label) %>%
              dplyr::summarise(
                Count = sum(count), 
                Total = sum(bign), 
                p = as.numeric(p[1]),
                .groups = 'drop'
              ) %>%
              dplyr::mutate(prop = Count/Total)
          )
          suppressWarnings(
            ADAE.tier2  <- ADAE.tier2 %>% 
              dplyr::left_join(
                comb_data %>%
                  dplyr::select(MLG_label, SOC_MLG) %>%
                  dplyr::distinct(), 
                by = variable
              )
          )
          
          p_rep_adj <- ADAE.tier2 %>%
            dplyr::group_by(SOC_MLG) %>%
            tidyr::nest() %>%
            dplyr::mutate(data = purrr::map(data, ~ FDR.fun(.))) %>%
            dplyr::mutate(data = purrr::map(data, ~ dplyr::pull(.,FDR) %>%
                                              min())) %>%
            tidyr::unnest_legacy() %>%
            dplyr::mutate(p = data) %>%
            FDR.fun(.) %>%
            dplyr::select(SOC_MLG,FDR)
          suppressWarnings(
            tmp <- ADAE.tier2 %>% 
              dplyr::full_join(p_rep_adj , by = 'SOC_MLG') %>%
              dplyr::filter(FDR <= alpha) %>%
              dplyr::select(-c(FDR,prop)) %>%
              FDR.fun(.) %>%
              dplyr::rename(DFDR = FDR)
          )
          if(dim(tmp)[1] > 0) {
            suppressWarnings(
              results2_data <- dplyr::full_join(
                tmp, results2_data, by = c(variable, "p")
              ) %>%
                dplyr::mutate(
                  DFDR.value = dplyr::if_else(
                    is.na(.$DFDR),
                    "", 
                    ifelse(.$DFDR < 0.0001 , 
                           "<0.0001",
                           as.character(format(round(.$DFDR, 4), scientific = FALSE))
                    )
                  ),
                  DFDR.value.lab = .$DFDR
                )
            )
            results2_data <-  results2_data %>%
              dplyr::mutate(
                p_adj.value = ifelse(
                  round(results2_data$p_adj,4) == 0, 
                  "<0.0001", 
                  format(round(results2_data$p_adj,4), scientific = FALSE)
                ),
                col.p = ifelse(results2_data$p < alpha, col_red, "white"),
                col.p_adj = ifelse(DFDR < alpha, col_red, "white")
              )
          } else {
            empt <- TRUE
            
            results2_data <- results2_data %>%
              dplyr::mutate(DFDR = as.numeric(NA))
            results2_data[,"DFDR.value"] <- ""
            results2_data <- results2_data %>%
              dplyr::mutate(DFDR.value.lab = as.numeric(NA))
          }
        }
        results2_data <-  results2_data %>%
          dplyr::mutate(col.p_adj = ifelse(DFDR < alpha, col_red, "white"))
        
      }else{
        print("adjust = DFDR does not work with variable = AEBODSYS")
      }
    }
    ## if DFDR is missing, set color to white
    results2_data$col.p_adj[is.na(results2_data$col.p_adj)] <- "white"
    
    if(order_by == "effect") {
      ## Add Factor Variable PT
      results2_data <- results2_data %>%
        dplyr::mutate(
          PT = factor(
            results2_data %>%
              dplyr::pull(variable),
            levels = results_data %>%
              dplyr::pull(variable) %>%
              .[(order(
                results_data %>%
                  dplyr::pull(stringr::str_to_lower(display)),
                na.last = FALSE
              )
              )]
          )
        )
      
      results2_data <- results2_data %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(stringr::str_to_lower(display))), !!rlang::sym(variable), TRTA_DetectoR)
    } else {
      if(adjustment == "FDR") {
        results2_data <- results2_data %>%
          dplyr::mutate(
            PT = factor(results2_data %>%
                          dplyr::pull(variable),
                        levels = results2_data %>%
                          dplyr::pull(variable) %>%
                          .[rev(order(results2_data$p))] %>%
                          unique()
            )
          )
        
        results2_data <- results2_data %>%
          dplyr::arrange(p, !! rlang::sym(variable), TRTA_DetectoR)
        
      } else if(adjustment == "DFDR") {
        
        if(variable == "AEDECOD"){
          
          tmp <- dplyr::distinct(results2_data, AEDECOD, DFDR, p) %>%
            dplyr::arrange(DFDR, p)
          
          results2_data <- results2_data %>%
            dplyr::mutate(
              PT = factor(
                results2_data %>%
                  dplyr::pull(variable),
                levels = rev(tmp$AEDECOD)
              )
            )
          
          results2_data <- results2_data %>%
            dplyr::arrange(DFDR, p, AEDECOD)
          
        }else if(variable == "AEBODSYS"){ 
          
          results2_data <- results2_data %>%
            dplyr::mutate(
              PT = factor(results2_data %>%
                            dplyr::pull(variable),
                          levels = results_data[[variable]][rev(order(results_data$p))]
              )
            )
          
          results2_data <- results2_data %>%
            dplyr::arrange(p, !! rlang::sym(variable), TRTA_DetectoR)
        } else if (variable == "MLG_label") {
          tmp <- dplyr::distinct(results2_data, MLG_label, DFDR, p) %>%
            dplyr::arrange(DFDR, p)
          
          results2_data <- results2_data %>%
            dplyr::mutate(
              PT = factor(results2_data %>%
                            dplyr::pull(variable),
                          levels = rev(tmp$MLG_label)
              )
            )
          
          results2_data <- results2_data %>%
            dplyr::arrange(DFDR, p, MLG_label)
          
        }
      }
    }
    if (variable =="AEDECOD" && adjustment == "DFDR") {
      if (empt == TRUE) {
        if (any(colnames(data_adae) == "MT_HLT")) {
          suppressWarnings(
            results2_data <- results2_data %>%
              dplyr::right_join(
                (data_adae %>%
                   dplyr::select(AEBODSYS,AEDECOD, MT_HLGT, MT_HLT) %>%
                   dplyr::distinct()
                ), by = 'AEDECOD'
              )
          )
        } else {
          suppressWarnings(
            results2_data <- results2_data %>%
              dplyr::right_join(
                (data_adae %>%
                   dplyr::select(AEBODSYS,AEDECOD) %>%
                   dplyr::distinct()), by = 'AEDECOD'
              )
          )
        }
      } else {
        if (any(colnames(data_adae) == "MT_HLT")) {
          suppressWarnings(
            results2_data <- results2_data %>%
              dplyr::select(-AEBODSYS) %>%
              dplyr::right_join(
                (data_adae %>%
                   dplyr::select(AEBODSYS,AEDECOD, MT_HLGT, MT_HLT) %>%
                   dplyr::distinct()), by = 'AEDECOD'
              )
          )
        } else {
          suppressWarnings(
            results2_data <- results2_data %>%
              dplyr::select(-AEBODSYS) %>%
              dplyr::right_join((data_adae %>%
                                   dplyr::select(AEBODSYS,AEDECOD) %>%
                                   dplyr::distinct()), by = 'AEDECOD')
          )
        }
      }
    }
    if (variable == "AEDECOD" && adjustment != "DFDR") {
      
      if (any(colnames(data_adae) == "MT_HLT")) {
        suppressWarnings(
          results2_data <- results2_data %>%
            dplyr::right_join(
              (data_adae %>%
                 dplyr::select(AEBODSYS, AEDECOD, MT_HLGT, MT_HLT) %>%
                 dplyr::distinct()), by = 'AEDECOD')
        )
      } else {
        suppressWarnings(
          results2_data <- results2_data %>%
            dplyr::right_join(
              (data_adae %>%
                 dplyr::select(AEBODSYS, AEDECOD) %>%
                 dplyr::distinct()), 
              by = 'AEDECOD'
            )
        ) 
      }
    }
    if (variable == "MLG_label" && heatmap == TRUE) {
      suppressWarnings(
        results2_data <- results2_data %>%
          dplyr::right_join(
            data_adae %>%
              dplyr::select(AEBODSYS, MLG, MLG_label, MT_HLT, MT_HLGT) %>%
              dplyr::distinct(),
            by = variable
          ) %>%
          dplyr::mutate(PT = MLG_label)
        
      )
      if (any(colnames(results2_data) == "SOC_MLG")) {
        results2_data <- results2_data %>% 
          dplyr::left_join(
            comb_data %>%
              dplyr::select(tidyselect::all_of(c("MLG_label", "SOC_MLG"))) %>%
              dplyr::distinct() ,
            by = c("MLG_label", "SOC_MLG")
          )
      } else {
        results2_data <- results2_data %>% 
          dplyr::left_join(comb_data %>% 
                             dplyr::select(tidyselect::all_of(c("MLG_label", "SOC_MLG"))) %>% 
                             dplyr::distinct() ,
                           by = "MLG_label")
      }
    }
    results2_data
  }
  
  if(!is.null(results2_data)){
    if (dim(results2_data)[1] == 0) {
      results2_data <- NULL
    }
  }
  
  ## remove rows with missing p-values
  results2_data <- results2_data[!is.na(results2_data$p), ]
  
  results2_data
}

