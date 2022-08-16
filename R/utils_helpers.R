#' Select adverse event data (used in DetectoR app)
#'
#' @param meddra_Version Number of MedDRa Version
#' @param mode Different modes like SAS files, demo data, data from Server are available
#' @param adae_file Adverse event file (default:NULL)
#' 
#' @return adae Transformed adverse event data set

select_adae_data <- function(meddra_Version, mode, adae_file = NULL) {
  # assertthat::assert_that(meddraVersion %in% unique(meddra$meddraVersion),
  #                         msg = paste0("MedDRA Version must be one of ", paste(unique(meddra$meddraVersion), collapse = ", "), "."))
  assertthat::assert_that(mode %in% c("sas", "demo", "server", "NoMeddra"),
                          msg = "mode must be one of 'sas', 'demo', 'NoMeddra'."
  )
  
  if (mode == "sas") {
    # assertthat::assert_that(!is.null(adae_file), msg = "SAS file must be provided!")
    
    adae <- haven::read_sas(adae_file$datapath)
    adae <- adae %>%
      dplyr::select(setNames(colnames(adae), toupper(colnames(adae))))
  } else if (mode == "portin") {
    shiny::req(input$ok_studies)
    ok_studies <- input$ok_studies
    shiny::isolate({
      studiesl <- studies()
    })
    shiny::isolate({
      instudies <- input$sel_studies
    })
    if (instudies != "" &&
        !is.null(instudies)) {
      selected_studies <- as.character(studiesl$UNIQUEID[studiesl$STUDYID %in% instudies])
      
      adae <- merge_studies(selected_studies, "get_AE")
      adae <- adae %>%
        dplyr::select(setNames(colnames(adae), toupper(colnames(adae))))
    }
  } else if (mode == "demo") {
    # transform all variable names to uppercase
    adae <- adae_data %>%
      dplyr::select(setNames(colnames(adae_data), toupper(colnames(adae_data))))
    demo_adae <- adae
    adae <- adae %>%
      dplyr::filter(AEDECOD != "Periodontitis")
  }
  
  ## meddra
  if (meddra_Version != "NoMeddra") {
    smq <- smq_view_sub %>%
      dplyr::filter(meddraVersion == as.numeric(meddra_Version))
    meddra <- meddra %>%
      dplyr::filter(meddraVersion == as.numeric(meddra_Version))
    smq.red <- smq[which(smq$SMQ_TYPE == "MLG" & (is.element(smq$SMQ_STATUS, c("RRL", "PRL"))) & 
                           as.numeric(smq$SMQ_MEDDRA_VERSION) == as.numeric(meddra_Version)), ]
    
    childs <- unique(smq.red$PT_SMQ_NAME[which(smq.red$SMQ_NAME != smq.red$PT_SMQ_NAME)])
    
    smq <- smq.red %>%
      dplyr::filter(!(SMQ_NAME %in% childs))
    
    if (!(adae %>% colnames() %in% "AEPTCD" %>% any()) && (adae %>% colnames() %in% "M_PT" %>% any())) {
      adae <- adae %>%
        dplyr::rename(AEPTCD = M_PT, AEBODSCD = M_SOC)
    }
    # For older versions change variable name pt_code into AEPTCD (if pt_code is available)
    if (!(any(colnames(adae) %in% "AEPTCD")) && (any(colnames(adae) %in% "pt_code"))) {
      adae <- adae %>%
        dplyr::rename(AEPTCD = pt_code)
    }

    # Merge adae and meddra data
    tmp <- smq %>%
      dplyr::select(SMQ_TYPE, SMQ_NAME, PT_CODE, PT_NAME, SMQ_ASS_SOC_NAME, SMQ_ASS_SOC_CODE, SMQ_STATUS) %>%
      dplyr::filter(SMQ_TYPE == "MLG") %>%
      dplyr::mutate(PT_CODE = as.character(PT_CODE))
    
    adae <- adae %>%
      dplyr::mutate(AEPTCD = as.character(AEPTCD))
    
    adae <- adae %>%
      dplyr::left_join(tmp %>%
                         dplyr::rename(AEPTCD = PT_CODE),
                       by = "AEPTCD"
      )
    adae <- adae %>%
      dplyr::mutate(
        MLG = ifelse(is.na(SMQ_NAME), AEDECOD, SMQ_NAME),
        MLG_label = ifelse(is.na(SMQ_NAME), paste0(AEDECOD, " (PT)"), paste0(SMQ_NAME, " (MLG)")),
        SOC_MLG = ifelse(is.na(SMQ_ASS_SOC_NAME), AEBODSYS, SMQ_ASS_SOC_NAME)
      )
    if (adae %>% tibble::has_name("AEBODSCD")) {
      adae <- adae %>%
        dplyr::mutate(SOC_MLG_CODE = ifelse(is.na(SMQ_ASS_SOC_CODE), AEBODSCD, SMQ_ASS_SOC_CODE))
    } else if (adae %>% tibble::has_name("AESOCCD")) {
      adae <- adae %>%
        dplyr::mutate(SOC_MLG_CODE = ifelse(is.na(SMQ_ASS_SOC_CODE), AESOCCD, SMQ_ASS_SOC_CODE))
    } else if (adae %>% tibble::has_name("AEBDSYCD")) {
      adae <- adae %>%
        dplyr::mutate(SOC_MLG_CODE = ifelse(is.na(SMQ_ASS_SOC_CODE), AEBDSYCD, SMQ_ASS_SOC_CODE))
    }
    suppressWarnings(
      adae <- adae %>%
        dplyr::left_join(meddra, by = c("AEDECOD" = "MT_PT"))
    )
    
    tmp <- adae %>%
      dplyr::select(AEBODSYS, MT_HLGT) %>%
      unique() %>%
      tidyr::drop_na()
    tmp2 <- plyr::ddply(tmp, plyr::.(MT_HLGT), dplyr::mutate, id = seq_along(AEBODSYS))
    tmp3 <- tmp2 %>%
      dplyr::mutate(MT_HLGT2 = ifelse(id == 1, MT_HLGT, paste0(MT_HLGT, " (", id, ")"))) %>%
      dplyr::select(-id)
    adae <- adae %>%
      dplyr::left_join(tmp3, by = c("AEBODSYS", "MT_HLGT")) %>%
      dplyr::select(-MT_HLGT) %>%
      dplyr::rename(MT_HLGT = MT_HLGT2)
  }
  return(adae)
}

#' Select subject level data (used in DetectoR app)
#'
#' @param mode Different modes like SAS files, demo data, data from Server are available
#' @param adsl_file Subject level file (default:NULL)
#' 
#' @return adae Transformed subject level data set
select_adsl_data <- function(mode, adsl_file = NULL) {
  assertthat::assert_that(mode %in% c("sas", "demo", "NoMeddra"),
                          msg = "mode must be one of 'sas', 'demo', 'server', 'NoMeddra'."
  )
  
  # adsl_data <- NULL #return null if none of the req is met
  if (mode == "sas") {
    shiny::req(adsl_file)
    
    adsl_data <- haven::read_sas(adsl_file$datapath)
    adsl_data <- adsl_data %>%
      dplyr::select(setNames(colnames(adsl_data), toupper(colnames(adsl_data))))
  }
  
  if (mode == "demo") {
    adsl_data <- adsl_data %>%
      dplyr::select(setNames(colnames(adsl_data), toupper(colnames(adsl_data))))
    demo_adsl_data <- adsl_data
  }
  
  if ("SAFFN" %in% toupper(colnames(adsl_data))) {
    adsl_data <- dplyr::filter(adsl_data, SAFFN == 1 | SAFFN == "Y")
  }
  
  if (is.element(("EXPDUR"),names(adsl_data))) {adsl_data <- adsl_data %>% dplyr::rename("DUREXP"="EXPDUR")}
  
  return(adsl_data)
}

#' Calculate the minimum number of events in verum
#'
#' @param N1 Number of patients in Verum
#' @param N2 Number of patients in Comparator
#' @param alternative A character string describing the alternative hypotesis (one or two sided)
#' @param alpha Alpha level of the test
#' @return minimun number of events in verum
#'

find.min.event <- function(N1, N2, alternative = "two.sided", alpha = 0.05) {
  if (N1 < 1000) {
    x1 <- seq(1, round(.1 * N1, 0), 1)
  } else if (N1 < 10000) {
    x1 <- seq(1, round(.05 * N1, 0), 1)
  } else if (N1 < 100000) {
    x1 <- seq(1,round(.01 * N1, 0), 1)
  } else {
    x1 <- seq(1, round(.001 * N1, 0), 1)
  }
  min(
    which(
      sapply(
        x1,
        FUN <- function(x1,...) {
          stats::fisher.test(matrix(c(0, N2 , x1 , N1 - x1) , nrow = 2), alternative = alternative )$p.value
        },
        N1,
        N2
      ) < alpha
    )
  )
}

#' Perform fisher test and calculate the relative risk and risk difference (Version 2)
#'
#'@param ds Data set.
#'@param alternative Either one or two sided test.
#'@param alph Significance level.
#'@param vari Target column.
#'@return output A tibble with the results.
#'
fisher.and.rr <- function(ds, alternative = "two.sided", alph = 0.05, vari) {
  
  shiny::withProgress(message = 'Detecting...', value = 0, {
    shiny::incProgress(0)
    
    ds_mod <- tidyr::pivot_wider(
      data = ds %>%
        dplyr::select(-TRTA_DetectoR_bign),
      names_from = TRTA_DetectoR,
      values_from = c(count, no_count, prop, bign)
    )
    
    if (all(!c("count_Comparator", "no_count_Comparator", "prop_Comparator", "bign_Comparator") %in% names(ds_mod))) {
      ds_mod <- ds_mod %>% 
        dplyr::mutate(
          count_Comparator = 0,
          no_count_Comparator = 0,
          prop_Comparator = 0,
          bign_Comparator = as.integer(0)
        )
    }
    
    if (all(!c("count_Verum", "no_count_Verum", "prop_Verum", "bign_Verum") %in% names(ds_mod))) {
      ds_mod <- ds_mod %>% 
        dplyr::mutate(
          count_Verum = 0,
          no_count_Verum = 0,
          prop_Verum = 0,
          bign_Verum = as.integer(0)
        )
    }
    
    ds_mod <- rbind(
      ds_mod %>% 
        dplyr::filter(
          count_Verum == 0 |
            no_count_Verum == 0 |
            count_Comparator == 0 |
            no_count_Comparator == 0
        ) %>% 
        dplyr::mutate(
          Verum_count = as.double(count_Verum + 0.5),
          Comparator_count = as.double(count_Comparator + 0.5),
          Verum_no_count = as.double(no_count_Verum + 0.5),
          Comparator_no_count = as.double(no_count_Comparator + 0.5),
          Verum_bign = bign_Verum + as.integer(1),
          Comparator_bign = bign_Comparator + as.integer(1),
          prob_Verum = (Verum_count / Verum_bign),
          prob_Comparator = (Comparator_count / Comparator_bign)
        ),
      ds_mod %>% 
        dplyr::filter(
          count_Verum > 0 &
            no_count_Verum > 0 &
            count_Comparator > 0 &
            no_count_Comparator > 0
        ) %>% 
        dplyr::mutate(
          Verum_count = as.double(count_Verum),
          Comparator_count = as.double(count_Comparator),
          Verum_no_count = as.double(no_count_Verum),
          Comparator_no_count = as.double(no_count_Comparator),
          Verum_bign = bign_Verum,
          Comparator_bign = bign_Comparator,
          prob_Verum = prop_Verum,
          prob_Comparator = prop_Comparator
        )
    )
    
    ds_mod <- ds_mod %>% 
      dplyr::mutate(rr = prob_Verum / prob_Comparator,
                    rr.missing_label = "",
                    rr.se = stats::qnorm(1-(alph/2)) * sqrt((Verum_no_count/(Verum_bign * Verum_count)) + (Comparator_no_count/(Comparator_bign * Comparator_count))),
                    rr.ucl = exp(log(rr)+rr.se),
                    rr.lcl = exp(log(rr)-rr.se),
                    rd = !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("prop_V"))])) - !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("prop_C"))])),
                    rd.se = stats::qnorm(1 - (alph/2)) * sqrt(((!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("count_V"))])) * !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("no_count_V"))]))) / as.numeric(!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("bign_V"))]))) ^3) +
                                                         ((!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("count_C"))])) * !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("no_count_C"))]))) / as.numeric(!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("bign_C"))]))) ^3)),
                    rd.ucl = rd + rd.se,
                    rd.lcl = rd - rd.se
      )
    
    ds_mod <- ds_mod[stats::complete.cases(ds_mod), ]
    shiny::incProgress(0.2)
    
    if (dim(ds_mod)[1] > 0 ) {
      ds_mod$p <- apply(ds_mod, 1, function(x) {
        stats::fisher.test(
          matrix(
            c(
              as.numeric(x["count_Comparator"]),
              as.numeric(x["count_Verum"]),
              as.numeric(x["no_count_Comparator"]),
              as.numeric(x["no_count_Verum"])
            ), nrow = 2
          ), alternative = alternative, simulate.p.value = TRUE 
        )$p.value
      })
      
      ds_mod <- ds_mod %>%
        dplyr::mutate(
          p.value.lab = ifelse(
            round(p, 4) == 0,
            "<0.0001",
            format(round(p, 4), scientific = FALSE)
          )
        )
      
      shiny::incProgress(0.8)
      
      ds_mod <- ds_mod %>%
        dplyr::ungroup() %>%
        dplyr::select(
          !!rlang::sym(vari),
          !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("prop_C"))])),
          !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("prop_V"))])),
          p,
          p.value.lab,
          rr,
          rr.missing_label,
          rr.lcl,
          rr.ucl,
          rd,
          rd.lcl,
          rd.ucl
        ) %>% dplyr::rename(
          prob2 = !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("prop_V"))])),
          prob1 = !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("prop_C"))]))
        )
    }
    shiny::incProgress(1, detail = paste("Finish!"))
  })
  ds_mod
}

#' Calculate the study-size adjusted incidence rates
#'
#'@param split.data Data set with Counts.
#'@param alph Significance level.
#'@param strat_var Stratification variable.
#'
#'@return res A tibble with relative risk and risk differences.

study.adj.incidence <- function(split.data, alph = 95, strat_var = stratify_variable) {
  
  split.d <- split.data
  split.data <- split.data %>% 
    dplyr::group_by(!!rlang::sym(strat_var)) %>%
    tidyr::drop_na() %>%
    tidyr::nest() %>%
    dplyr::mutate(dim_ = purrr::map(data, ~ dim(.)[1])) %>%
    tidyr::unnest(cols = c(data,dim_)) %>%
    dplyr::filter(dim_ == 2) %>% 
    dplyr::select(-dim_) %>% 
    dplyr::ungroup()
  
  suppressWarnings(
    split.data <- split.data %>%
      dplyr::mutate(prob.str = events/n ) %>%
      dplyr::full_join(
        (
          tibble::tibble(
            !!rlang::quo_name(strat_var) := 
              unique(split.data[[strat_var]]),
            FREQ = (
              split.data %>%
                dplyr::group_by(!!rlang::sym(strat_var)) %>%
                dplyr::summarise(total = sum(n),.groups = 'drop') %>%
                .$total/(sum(split.data$n))
            )
          )
        )
        , by = strat_var
      ) %>%
      dplyr::mutate(weighted.prob = .$prob.str * .$FREQ) %>%
      dplyr::mutate(
        prob1 = (
          dplyr::filter(., TRTA_DetectoR == "Comparator") %>%
            dplyr::select("weighted.prob") %>%
            sum()
        ),
        prob2 = (dplyr::filter(., TRTA_DetectoR == "Verum") %>%
                   dplyr::select("weighted.prob") %>%
                   sum()
        )
      )
  )
  
  ai <- split.data %>%
    dplyr::filter(TRTA_DetectoR == "Verum") %>%
    dplyr::select(events) %>%
    dplyr::pull()
  bi <- split.data %>%
    dplyr::filter(TRTA_DetectoR == "Verum") %>%
    dplyr::select(no.event) %>%
    dplyr::pull()
  n1i <- split.data %>%
    dplyr::filter(TRTA_DetectoR == "Verum") %>%
    dplyr::select(n) %>%
    dplyr::pull()
  ci <- split.data %>%
    dplyr::filter(TRTA_DetectoR == "Comparator") %>%
    dplyr::select(events) %>%
    dplyr::pull()
  di <- split.data %>%
    dplyr::filter(TRTA_DetectoR == "Comparator") %>%
    dplyr::select(no.event) %>%
    dplyr::pull()
  n2i <- split.data %>%
    dplyr::filter(TRTA_DetectoR == "Comparator") %>%
    dplyr::select(n) %>%
    dplyr::pull()
  
  res_raw <- metafor::rma.mh(ai, bi, ci, di, n1i, n2i, measure = "RD", add = 0, drop00 = FALSE, level = alph)
  
  res_extract.effect.ci <- res_raw %>%
    confint() %>%
    .$fixed
  
  if (any(ci == 0) | any(ai == 0 )) {
    ai <- ai + 0.5
    bi <- bi + 0.5
    ci <- ci + 0.5
    di <- di + 0.5
    n1i <- n1i + 1
    n2i <- n2i + 1
    res_raw2 <- metafor::rma.mh(ai, bi, ci, di, n1i, n2i, measure = "RR", add = 0, level = alph)
    res_extract.effect.ci2 <- res_raw2 %>%
      confint() %>%
      .$fixed
  } else {
    res_raw2 <- metafor::rma.mh(ai, bi, ci, di, n1i, n2i, measure = "RR", add = 0, level = alph)
    res_extract.effect.ci2 <- res_raw2 %>%
      confint() %>%
      .$fixed
  }
  
  res <- tibble::tibble(
    "prob1" = split.data$prob1[1],
    "prob2" = split.data$prob2[1],
    "rr" :=  exp(res_extract.effect.ci2[1]),
    "rr.lcl" := exp(res_extract.effect.ci2[2]),
    "rr.ucl" := exp(res_extract.effect.ci2[3]),
    "p.rr" := res_raw2$pval,
    
    "rd" := res_extract.effect.ci[1],
    "rd.lcl" := res_extract.effect.ci[2],
    "rd.ucl" := res_extract.effect.ci[3],
    "p.rd" := res_raw$pval
  )
  
}

#'Calculate stratified relative risk and risk differences
#'
#'@param comb_data adsl and adae data set combined
#'@param variable Description.
#'@param display Description. 
#'@param alpha Description.
#'@param stratify_variable Stratification variable.
#'
#'@return res_new

study.strat.RR.and.RD <- function(
  comb_data, 
  variable, 
  display, 
  alpha = 0.95,
  stratify_variable) {
  
  # Calculation of number of subjects within Studies and treatment groups
  N_study.stratified <- comb_data %>%
    dplyr::select(!!rlang::sym(stratify_variable), "USUBJID", "TRTA_DetectoR") %>%
    unique() %>%
    dplyr::count(TRTA_DetectoR, !!rlang::sym(stratify_variable))
  
  eventcount_stratified <- comb_data %>%
    dplyr::select(tidyselect::all_of(c(variable,"USUBJID", stratify_variable,"TRTA_DetectoR"))) %>%
    unique() %>%
    dplyr::count(!!rlang::sym(variable), !!rlang::sym(stratify_variable), TRTA_DetectoR) %>%
    dplyr::rename("events" = "n") %>%
    tidyr::complete(!!rlang::sym(variable), !!rlang::sym(stratify_variable), TRTA_DetectoR) %>%
    tidyr::replace_na(list(events = 0))
  suppressWarnings(
    count.data.by.group <- dplyr::left_join(eventcount_stratified, N_study.stratified, by = c(stratify_variable, "TRTA_DetectoR")) %>%
      dplyr::mutate(no.event = n - events)  %>%
      dplyr::group_by(!!rlang::sym(variable)) %>%
      tidyr::nest()
  )
  count.data.by.group <- na.omit(count.data.by.group)
  
  res_new <- count.data.by.group %>%
    dplyr::mutate(data = purrr::map(data, ~ study.adj.incidence(., alph = (100 * (1 - alpha)), strat_var = stratify_variable)))
  
  res_new <- res_new %>%
    tidyr::unnest_legacy() %>%
    dplyr::mutate(p = !!rlang::sym(paste0("p.", tolower(display))), rr.missing_label = "")
  
  res_new <- res_new %>%
    dplyr::mutate(p.value.lab = ifelse(round(as.numeric(p),4) == 0, "<0.0001", format(round(as.numeric(p), 4), scientific = FALSE))) %>%
    na.omit()
  
  return(res_new)
}

#' Calculate the false discovery rate
#'
#'@param ADAE The adae data set.
#'@return ordered.data
#'

FDR.fun <- function(ADAE) {
  
  if (is.null(ADAE)) {
    print("NULL ADAE")
    return(NULL)
  }
  if (is.character(ADAE)) {
    file <- ADAE
    ADAE <- utils::read.table(file, header = TRUE, stringsAsFactors = FALSE)
  }
  if (!("p" %in% colnames(ADAE))) {
    print("p column missing")
    return(NULL)
  }
  m <- nrow(ADAE)
  if (is.null(m)) {
    print("No data found")
    return(NULL)
  }
  if (m == 0) {
    ADAE[,"FDR"] <- numeric()
    return(ADAE)
  }
  
  ordered.data <- ADAE[order(ADAE$p), , drop = FALSE]
  ordered.data$FDR <- rep(NA, m)
  ordered.data$FDR[m] <- ordered.data$p[m]
  if (m > 1){
    for (j in (m - 1):1){
      ordered.data$FDR[j] <- min(ordered.data$p[j] * m / j, ordered.data$FDR[j + 1])
    }
  }
  ordered.data
}


#### CALCDOUBLEDOTPLOT FUNCTION ####
#' Calculate all Variables for the double_dot_plot function
#'
#'@param data_adsl adsl data set.
#'@param data_adae adae data set.
#'@param variable AEDECOD or AEBODSYS or MLG.
#'@param display RR or RD.
#'@param adjustment FDR or DFDR or BB.
#'@param order_by p-value or effect.
#'@param study_strat TRUE or FALSE.
#'@param n.iterations Number of iterations for BB.
#'@param n.burn.in Number of burn in phase for BB.
#'@param seed Random Number Generator number.
#'@param alternative A character string describing the alternative hypotesis (one or two sided)
#'@param heatmap Transform returned data into structure of the heatmap
#'@param alpha Alpha level of the test
#'@param filter A character string if filter on adverse events should be performed
#'@importFrom rlang :=
#'@importFrom dplyr %>%
#'@return results2_data
#'

calcDoubleDotPlot <- function(
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
  filter = "nomethod"
) {
  
  suppressWarnings(
    comb_data <- dplyr::left_join(data_adsl, data_adae, by = c("STUDYID","USUBJID"))
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
      dplyr::group_by(!! rlang::sym(variable), TRTA_DetectoR) %>%
      dplyr::summarise(count2 = dplyr::n_distinct(USUBJID),.groups = 'drop_last') %>%
      dplyr::full_join(exp_grid_new, by = c(variable,"TRTA_DetectoR")) %>%
      dplyr::full_join(bign, by = "TRTA_DetectoR") %>%
      dplyr::filter(!is.na(!! rlang::sym(variable))) %>%
      dplyr::mutate(count = ifelse(is.na(count2), 0, count2)) %>%
      dplyr::mutate(prop = count / bign) %>%
      dplyr::mutate(no_count = bign - count) %>%
      dplyr::select(-count2) %>%
      dplyr::mutate(TRTA_DetectoR_bign = paste0(TRTA_DetectoR," (N=", bign,")")))
  
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
      
      results_data <- fisher.and.rr(double_dot_plot_data, alternative = alternative, alph = alpha, vari = variable)
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
        dplyr::full_join(double_dot_plot_data, by = variable)
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
            results2_data[,"DFDR"] <- numeric()
            results2_data[,"DFDR.value"] <- ""
            results2_data[,"DFDR.value.lab"] <- numeric()
          }
        }
        results2_data <-  results2_data %>%
          dplyr::mutate(col.p_adj = ifelse(DFDR < alpha, col_red, "white"))
        
      }else{
        print("adjust = DFDR does not work with variable = AEBODSYS")
      }
    }
    
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
  
  if (dim(results2_data)[1] == 0) {
    results2_data <- NULL
  }
  results2_data
}