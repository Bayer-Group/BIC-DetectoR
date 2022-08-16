#' dataManual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataManual_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("datamanual1"))
  )
}

#' dataManual Server Function
#'
#' @noRd
mod_dataManual_server <- function(input, output, session) {
  ns <- session$ns
  col_midgrey <- "#4d4d4d"
  col_lightgrey <- "#595959"
  col_darkgrey <- "#353535"
  output$datamanual1 <- shiny::renderUI({
    list(
      shiny::HTML(
        paste0(
          "<h1> Data Manual Tab </h1>
          <p> <img src = 'AppIcon_BAG_DetectoR_210x210mm_RGB.png' alt = 'Graphic cannot be displayed' width = '210' height = '210' align = 'right'>
          <h2> File Format and Structure </h2>
          
          <h4> File Format </h4>
          
          <p> DetectoR is designed to upload the CDISC data sets ADSL and ADAE in SAS format. </p>
          
          <h4> File Structure </h4>
          
          <p> In order to use the DetectoR, the two SAS data sets have to include the following variables: </p>
          
          <style>
            table {
              font-family: arial, sans-serif;
              border-collapse: collapse;
              width: 100%;
            }
            
            td, th {
              border: 1px solid ", col_lightgrey, ";
              text-align: left;
              padding: 8px;
            }
            tr:nth-child(odd) {
              background-color: ", col_midgrey, ";
            }
            tr:nth-child(even) {
              background-color: ", col_darkgrey, ";
            }
          </style>
          
          <table>
            <tr>
              <th> Dataset </th>
              <th> required variables </th>
            </tr>
            <tr>
              <th> ADSL
                <th> <p> STUDYID (Study identifier) : Character variable </p>
                <p> USUBJID (Unique subject identifier) : Character variable 
                <p> SAFFN (Safety analysis set flag):  Numeric variable to identify subjects in the Safety analysis set. </p>
                <p> DUREXP (Duration of exposure):  Integer variable defining the treatment duration. Only needed, if it is intended to present incidence rates. </p>
                <p> DetectoR restricts to subjects with SAFFN=1 </p>
                <p> One additional variable to define the groups to be compared, usually information about the treatment arms.
                Flexible treatment selection based on any variable in ADSL is possible.</p>
              </th>
            </tr>
            <tr>
              <th>  ADAE </th>
              <th>  <p> STUDYID (Study identifier): Character variable </p>
                <p> USUBJID (Unique subject Identifier): Character variable </p>
                <p> AEBODSYS (Body System or Organ Class): Character variable </p>
                <p> AEDECOD (Dictionary-Derived Preferred Term): Character variable </p>
                <p> AAESDURN (Time until AE): Integer variable defining the number of days from reference day to start date of the event. Only needed, if it is intended to present incidence rates.  </p>
                <p> ADSL and ADAE will be merged by STUDYID and USUBJID. </p>
              </th>
            </tr>
          </table>
          <h2> MedDRA data information </h2>
          
          To use MedDRA within the app, the MedDRA data sets are required.
          If these data sets are available, they can put into the package folder 
          data-raw. Therefore a folder named 'Meddra' is required with subfolder(s)
          named as the version number(s) (e.g. '11.0','11.1','12.0'...).
          In the subfolder the files 'meddra.Rdta' and 'smq_view_sub.Rdta' are needed.
          The R files 'meddra.R', 'meddra_numbers.R' in data-raw create the RDA-files
          'meddra.rda', 'meddra_numbers.rda' and 'smq_view_sub.rda'. These .R files need to be 
          executed manually to update the .rda files in the data folder. 
          
          <h4> Important points to consider </h4>
          <p> 
            In order to enable the usage of MedDRA Medical Labelling Groupings ADAE needs to have the (either character or numeric) variable AEPTCD OR pt_code OR M_PT included.
            All variable names in the table above are case sensitive, i.e. if ADSL contains for example the row saffn (lower case) DetectoR does not work.
          </p> 
          "
        )
      )
    )
  })
}

## To be copied in the UI
# mod_dataManual_ui("dataManual_ui_1")

## To be copied in the server
# callModule(mod_dataManual_server, "dataManual_ui_1")
