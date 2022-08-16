shinydashboard::dashboardPage(
      title = "DetectoR",
      #### HEADER ####
      shinydashboard::dashboardHeader(
        title = shiny::img(
          src = "AppSign_white_BAG_DetectoR_220x76mm_RGB.png",
          height = 50,
          align = "left"
        ),
        titleWidth = 250
      ),
      #### SIDEBAR ####
      shinydashboard::dashboardSidebar(width = 315, mod_dataUpload_ui("dataUpload_ui_1")),
      #### BODY ####
      shinydashboard::dashboardBody(
        #useShinyalert(),
        mod_mainPanel_ui("mainPanel_ui_1")
      )
    )

