#' Perform fisher test and calculate the relative risk and risk difference based in incidence proportions
#'
#'@param ds Data set.
#'@param alternative Either one or two sided test.
#'@param alph Significance level.
#'@param vari Target column.
#'@return output A tibble with the results.
fisher.and.rr_ip <- function(ds, alternative = "two.sided", alph = 0.05, vari) {
  
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
  dplyr::as_tibble(ds_mod)
}

#' Perform fisher test and calculate the relative risk and risk difference based on incidence rates
#'
#'@param ds Data set.
#'@param alternative Either one or two sided test.
#'@param alph Significance level.
#'@param vari Target column.
#'@return output A tibble with the results.
#'
fisher.and.rr_ir <- function(ds, alternative = "two.sided", alph = 0.05, vari) {
  
  ds_mod <- tidyr::pivot_wider(
    data = ds %>%
      dplyr::select(-TRTA_DetectoR_bign),
    names_from = TRTA_DetectoR,
    values_from = c(count, no_count, prop, bign, pattime)
  )
  
  if (all(!c("count_Comparator", "no_count_Comparator", "prop_Comparator", "bign_Comparator", "pattime_Comparator") %in% names(ds_mod))) {
    ds_mod <- ds_mod %>% 
      dplyr::mutate(
        count_Comparator = 0,
        no_count_Comparator = 0,
        prop_Comparator = 0,
        bign_Comparator = as.integer(0),
        pattime_Comparator = 0
      )
  }
  
  if (all(!c("count_Verum", "no_count_Verum", "prop_Verum", "bign_Verum", "pattime_Verum") %in% names(ds_mod))) {
    ds_mod <- ds_mod %>% 
      dplyr::mutate(
        count_Verum = 0,
        no_count_Verum = 0,
        prop_Verum = 0,
        bign_Verum = as.integer(0),
        pattime_Verum = 0
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
        prob_Comparator = (Comparator_count / Comparator_bign),
        Verum_pattime = pattime_Verum + as.integer(1),
        Comparator_pattime = pattime_Comparator + as.integer(1)
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
        prob_Comparator = prop_Comparator,
        Verum_pattime = pattime_Verum,
        Comparator_pattime = pattime_Comparator
      )
  )
  
  ## Compute Relative differences
 
  rd <- dplyr::mutate(ds_mod, rd = 365.25 * DescTools::BinomDiffCI(count_Verum, pattime_Verum,
                                                                   count_Comparator, pattime_Comparator,
                                                method = "wald", sides = "two.sided"))$rd %>% 
    as.data.frame()
  
  
  
  colnames(rd) <- c("rd", "rd.lcl", "rd.ucl")
  
  ## Compute Relative risk
  rr <- dplyr::mutate(ds_mod, rr = DescTools::BinomRatioCI(Verum_count, Verum_pattime,
                                                           Comparator_count, Comparator_pattime,
                                                           method = "katz.log", sides = "two.sided"))$rr %>% 
    as.data.frame() 
  colnames(rr) <- c("rr", "rr.lcl", "rr.ucl")
  
  ds_mod <- cbind(ds_mod, rd, rr) %>% 
    dplyr::mutate(rr.missing_label = "",
                  rr.se = stats::qnorm(1-(alph/2)) * sqrt((Verum_no_count/(Verum_bign * Verum_count)) + (Comparator_no_count/(Comparator_bign * Comparator_count))),
                  rd.se = stats::qnorm(1 - (alph/2)) * sqrt(((!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("count_V"))])) * !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("no_count_V"))]))) / as.numeric(!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("bign_V"))]))) ^3) +
                                                       ((!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("count_C"))])) * !!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("no_count_C"))]))) / as.numeric(!!rlang::sym(names(ds_mod[which(colnames(ds_mod) %>% startsWith("bign_C"))]))) ^3)),
    )
  
  ds_mod <- ds_mod[stats::complete.cases(ds_mod), ]
  
  if (dim(ds_mod)[1] > 0 ) {
    ds_mod$p <- apply(ds_mod, 1, function(x) {
      stats::fisher.test(
        matrix(
          c(
            as.numeric(x["count_Comparator"]),
            as.numeric(x["count_Verum"]),
            as.numeric(x["pattime_Comparator"]),
            as.numeric(x["pattime_Verum"])
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
  ds_mod
}
