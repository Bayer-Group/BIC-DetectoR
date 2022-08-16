## list all meddra directories

if (!file.exists(here::here("data-raw", "Meddra"))) {
  
  meddra <- tibble::tibble(
    MT_PT = character(),
    MT_HLT = character(),
    MT_HLGT = character(),
    meddraVersion = character()
  )
  usethis::use_data(meddra, overwrite = TRUE)
  
  smq_view_sub <- tibble::tibble(
    SMQ_NAME = character(),
    SMQ_TYPE = character(),
    SMQ_STATUS = character(),
    SMQ_MEDDRA_VERSION = character(),
    SMQ_ASS_SOC_CODE = character(),
    SMQ_ASS_SOC_NAME = character(),
    PT_CODE = character(),
    PT_NAME = character(),
    PT_SMQ_NAME = character(),
    meddraVersion = character()
  )
  usethis::use_data(smq_view_sub, overwrite = TRUE)
} else {

  dirs <- list.dirs(here::here("data-raw", "Meddra"))[-1]
  
  ## meddra
  meddra <- lapply(dirs, function(dir){
    readRDS(paste0(dir, "/meddra.Rdta")) %>% 
      dplyr::mutate(meddraVersion = unlist(strsplit(dir, "/"))[length(unlist(strsplit(dir, "/")))])
  }) %>% 
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    mutate(meddraVersion = as.numeric(meddraVersion))
  usethis::use_data(meddra, overwrite = TRUE)
  
  ## smq view sub
  smq_view_sub <- lapply(dirs, function(dir){
    readRDS(paste0(dir, "/smq_view_sub.Rdta")) %>% 
      dplyr::mutate(meddraVersion = unlist(strsplit(dir, "/"))[length(unlist(strsplit(dir, "/")))])
  }) %>% 
    do.call(rbind, .) %>% 
    as_tibble() %>% 
    mutate(meddraVersion = as.numeric(meddraVersion))
  usethis::use_data(smq_view_sub, overwrite = TRUE)
} 