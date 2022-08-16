#' mainPanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @export
#' @importFrom shiny NS tagList 
mod_mainPanel_ui <- function(id) {
  ns <- NS(id)
  tagList(
      shiny::tags$head(
      shiny::tags$style(
        HTML(".shiny-notification {
        position:fixed;
        top: calc(50%);
        left: calc(40%);
        width: 350px;
        font-size: 30px;
        background-color: white;
        font-color: black;
        color: #424242;
        }"
        )
      )
    ),
    shiny::tags$head(shiny::tags$style(".fa-plus {color:#61a337}", ".fa-minus {color:#EE2C2C}")),
    shiny::tags$style(paste0("body {color: white}")),
    shiny::tags$head(shiny::tags$style(
      HTML(paste0("
      .content-wrapper, .right-side {
      background-color: ", col_lightgrey, ";
      }
      
      .skin-blue .main-header .logo {
      background-color: ", col_darkgrey, ";
      }
      .skin-blue .main-header .logo:hover {
      background-color: ", col_darkgrey, ";
      }
      
      .skin-blue .main-header .navbar {
      background-color: ", col_darkgrey, ";
      }
      
      /* main sidebar */
      .skin-blue .main-sidebar {
      background-color: ", col_darkgrey, ";
      }
      
      /* active selected tab in the sidebarmenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: ", col_midgrey, ";
      }
      
      /* other links in the sidebarmenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
      background-color: ", col_darkgrey, ";
      color: #ffffff;
      }
      
      /* other links in the sidebarmenu when hovered */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
      background-color: ", col_lightgrey, ";
      }
      /* toggle button when hovered  */
      .skin-blue .main-header .navbar .sidebar-toggle:hover{
      background-color: ", col_blue, ";
      }
      
      .skin-blue .main-header .navbar .sidebar-toggle:hover{
      background-color: #353535;
      }"))
    )),
     tags$head(tags$style(HTML('
         .skin-blue .left-side, .skin-blue .wrapper {
                        background-color: #595959;
                        }
         '))),
    shiny::tags$script(HTML('
                            $(document).ready(function() {
                            $("header").find("nav").append(\' <h4 style = "color:white"> Interactive Safety Data Insights Tool </h4>\');
                            })')),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "safety", mod_safety_ui(ns("safety_ui_1"))),
      shinydashboard::tabItem(tabName = "graph", mod_graph_ui(ns("graph_ui_1"))),
      shinydashboard::tabItem(tabName = "heatmap", mod_heatmap_ui(ns("heatmap_ui_1"))),
      shinydashboard::tabItem(tabName = "helptext", mod_helpText_ui(ns("helpText_ui_1"))),
      shinydashboard::tabItem(tabName = "datamanual", mod_dataManual_ui(ns("dataManual_ui_1")))
    )
  )
}
    
#' mainPanel Server Function
#' @export
#' @noRd 
mod_mainPanel_server <- function(input, output, session, mod_dataUpload_server_vars){
  ns <- session$ns

  #################### Grab input values from UI ####################
  ## safety ui module
  mod_safety_ui_vars <- callModule(mod_safety_ui_vars, "safety_ui_1")
  
  ## graph ui module

  mod_graph_ui_vars <- callModule(mod_graph_ui_vars, "graph_ui_1")
  
  ## heatmap ui module
  mod_heatmap_ui_vars <- callModule(mod_heatmap_ui_vars, "heatmap_ui_1")
  
  #################### Call server modules ####################
  SI_var <- shiny::reactiveValues(val = "AEBODSYS")
  SI_disp <- shiny::reactiveValues(val = "RR")
  SI_order_by <- shiny::reactiveValues(val = "p-value")
  SI_adjust <- shiny::reactiveValues(val = "FDR")
  alpha <- shiny::reactiveValues(val = 0.05)
  SI_filter <- shiny::reactiveValues(val = "nomethod")
  SI_strat <- shiny::reactiveValues(val = "None")
  SI_fisher_alternative <- shiny::reactiveValues(val = "two.sided")

  ## determine if incidence rates can be computed
  # Calculate Double dot plot values - reactive calcDDP()

  calcDDP <- shiny::eventReactive(c(
    mod_safety_ui_vars$go_tab1(),
    mod_graph_ui_vars$go_tab2(),
    mod_dataUpload_server_vars$apply(),
    mod_dataUpload_server_vars$apply2()
  ), {
    shiny::req(
      mod_dataUpload_server_vars$adae_filtered(), 
      mod_dataUpload_server_vars$adsl_filtered(),
      SI_disp$val, alpha$val, SI_filter$val
    )

    
    data_adsl_ <- mod_dataUpload_server_vars$adsl_filtered()
    data_adsl_ <- data_adsl_[apply(data_adsl_, 1, function(x) {
      !all(is.na(x))
    }), ]
    data_adae_ <- mod_dataUpload_server_vars$adae_filtered()
    
    variable <- shiny::isolate(SI_var$val)
    display1 <- shiny::isolate(SI_disp$val)
    adjustment <- shiny::isolate(SI_adjust$val)
    order_by <- shiny::isolate(SI_order_by$val)
    study_strat <- shiny::isolate(SI_strat$val)
    fisher_alternative <- shiny::isolate(SI_fisher_alternative$val)
    alph <- as.numeric(shiny::isolate(alpha$val))
    filt <- shiny::isolate(SI_filter$val)
    measure <- shiny::isolate(mod_dataUpload_server_vars$measure())
    
      tmp <- calcDoubleDotPlot2(
        data_adsl = data_adsl_,
        data_adae = data_adae_,
        variable = variable,
        display = display1,
        adjustment = adjustment,
        order_by = order_by,
        study_strat = study_strat,
        n.iterations = 100000,
        n.burn.in = 5000,
        seed = 2006,
        alternative = fisher_alternative,
        alpha = alph,
        filter = filt,
        measure = measure
      )
    tmp
  })
  
  
  max <- shiny::reactive({
    shiny::req(calcDDP(), SI_var$val)
    calc <- calcDDP()
    tmp <- calc %>%
      dplyr::pull(SI_var$val) %>%
      unique() %>%
      length()
    tmp
  })
  
  # N_1 <- shiny::reactive({
  #   shiny::req(calcDDP())
  #   as.numeric(strsplit(unique(calcDDP()$TRTA_DetectoR)[which(startsWith(unique(calcDDP()$TRTA_DetectoR), "Verum"))], "\\=|\\)")[[1]][2])
  # })
  # 
  # N_2 <- shiny::reactive({
  #   shiny::req(calcDDP())
  #   as.numeric(strsplit(unique(calcDDP()$TRTA_DetectoR)[which(startsWith(unique(calcDDP()$TRTA_DetectoR), "Comparator"))], "\\=|\\)")[[1]][2])
  # })
  
  
  N_1 <- shiny::reactive({
    sum(mod_dataUpload_server_vars$adsl_data_TRTA()$TRTA_DetectoR == "Verum")
  })
  
  N_2 <- shiny::reactive({
    sum(mod_dataUpload_server_vars$adsl_data_TRTA()$TRTA_DetectoR == "Comparator")
  })
  
  
  min.event <- shiny::reactive({
    shiny::req(N_1(), N_2(), SI_fisher_alternative$val, alpha$val)
    tmp <- find.min.event(N1 = N_1(), N2 = N_2(), alternative = SI_fisher_alternative$val, alpha = as.numeric(alpha$val))
    tmp
  })
  
  # view dataset (Safety) module
  mod_safety_server_vars <- callModule(
    mod_safety_server,
    "safety_ui_1",
    mod_dataUpload_server_vars,
    go_tab2 = mod_graph_ui_vars$go_tab2,
    SI_var = SI_var,
    SI_disp = SI_disp, 
    SI_order_by = SI_order_by,
    SI_adjust = SI_adjust,
    alpha = alpha, 
    SI_filter = SI_filter,
    SI_strat = SI_strat,
    SI_fisher_alternative = SI_fisher_alternative,
    calcDDP = calcDDP,
    min.event = min.event
  )
  
  # dot plot
  callModule(
    mod_graph_server,
    "graph_ui_1",
    mod_dataUpload_server_vars,
    go_tab1 = mod_safety_ui_vars$go_tab1, 
    go_tab3 = mod_heatmap_ui_vars$go_tab3,
    SI_var = SI_var, 
    SI_disp = SI_disp,
    SI_order_by = SI_order_by,
    SI_adjust = SI_adjust,
    alpha = alpha, 
    SI_filter = SI_filter, 
    SI_strat = SI_strat,
    SI_fisher_alternative = SI_fisher_alternative,
    calcDDP = calcDDP,
    min.event = min.event,
    max = max
  )
  
  # heatmap
  callModule(
    mod_heatmap_server,
    "heatmap_ui_1", 
    mod_dataUpload_server_vars, SI_var = SI_var, 
    SI_strat = SI_strat,
    min.event = min.event,
    N_1 = N_1,
    N_2 = N_2
  )
  
  ## Help text
  callModule(mod_helpText_server, "helpText_ui_1")
  
  ## Data manual
  callModule(mod_dataManual_server, "dataManual_ui_1")
  
  return(list(
    switch = mod_safety_ui_vars$switch,
    switch2 = mod_graph_ui_vars$switch2,
    switch3 = mod_heatmap_ui_vars$switch3,
    go_tab3 = mod_heatmap_ui_vars$go_tab3,
    SI_var = SI_var,
    SI_filter = SI_filter,
    min.event = min.event,
    SI_disp = SI_disp,
    calcDDP = calcDDP,
    SI_order_by = SI_order_by,
    SI_adjust = SI_adjust,
    SI_strat = SI_strat,
    SI_fisher_alternative = SI_fisher_alternative,
    alpha = alpha
  ))
}
    
## To be copied in the UI
# mod_mainPanel_ui("mainPanel_ui_1")
    
## To be copied in the server
# callModule(mod_mainPanel_server, "mainPanel_ui_1")
 
