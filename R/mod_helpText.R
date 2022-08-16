#' helpText UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_helpText_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("helptext1"))
  )
}


#' helpText Server Function
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_helpText_server <- function(input, output, session) {
  ns <- session$ns
  output$helptext1 <- shiny::renderUI({
    list(HTML("
              <h1> The Concept of the DetectoR R Shiny app for Clinical Trial Safety Data </h1>
              <p> <img src='AppIcon_BAG_DetectoR_210x210mm_RGB.png' alt='Graphic cannot be displayed' width='210' height='210' align = 'right'>
              The DetectoR R Shiny app provides a handy platform allowing for early identification of signals and an ongoing monitoring of safety along the medical product development phase and lifecycle.
              <p> The application of DetectoR in the submission process supports the label creation and simplifies the identification of label-relevant adverse events. State-of-the-art statistical methodology is incorporated in the R Shiny application, including the use of graphical displays as well as statistical tests and multiplicity-adjusted p-values.
              </p>
              
              SAS data sets (ADSL & ADAE) from studies or pools can be uploaded easily without any further pre-processing into the DetectoR app. For further information on data set requirements, please see the Data manual tab. PORTIN access will be available in the productive version of DetectoR after PORTIN is updated to the most recent R version. Demo data is available to become easily acquainted with the functionality of the DetectoR app.
              </p>
              <p>
              While uploading the data sets the MedDRA version included in the ADAE data set has to be chosen, in case an analysis using Medical Labelling Groupings (MLGs) is desired. If not 'Run without MedDRA' can be chosen instead.
              </p>
              <p>
              After data sets are uploaded the variable defining the (treatment) groups to be compared has to be selected out of the variables available in ADSL. Subsequently, the categories defining the Verum and the Comparator need to be identified.
              </p>
              <p>
              Based on subject level characteristics and adverse event categories generic data filtering can be applied any time by using the select Filter tab. For example, the analyses can be restricted to serious adverse events or adverse events leading to discontinuation to focus on events of higher severity or impact. Additionally, on the patient level generic filtering is possible which means it can not only be filtered for baseline characteristics like the usual covariates sex, age or BMI, but also for any co-morbidity or risk factor included in the input data set.
              It is to note that multiple filters can be selected and added by ticking the  +.
              </p>
              <p>
              After the data is uploaded, treatment is defined and potentially filters are applied the three tabs View Data set,
              Double dot plot & Heatmap are available to discover the data
              </p>
              <p>
              The heart of DetectoR is the double dot plot which allows for a clear overview of dense information gaining quickly data insights.
              The double dot plot shows for each adverse event of interest the incidence proportion per treatment group combined with an effect estimate.
              For comparison of the treatments risk differences and relative risks can be chosen.
              While risk differences allow the user to identify the absolute impact of the difference between the two treatments compared,
              the relative risk allows to assess the relative impact. Alternatively, incdence rates by patient years with corresponding estimates incidence rate differences and incidence rate ratios can be presented.
              </p>
              <p>
              For a more straightforward detection of relevant signals, different techniques for calculation of multiplicity adjusted p-values are implemented and can be chosen from the parameter settings, i.e. the False Discovery rate (FDR) and the new Double FDR (which in contrast to the original DFDR does not need resampling).
              With the option to order the adverse events based on either the adjusted p-values or the chosen risk estimates the relevant safety signals can easily be detected.
              </p>
              <p>
              A first glance on the overall safety profile can be drawn from comparing the Body System Organ Classes (SOCs). Nevertheless, since the labelling at Bayer is usually done based on the Medical Labeling Groupings (MLGs) and Preferred Terms (PTs), these adverse event categorizations/types can be also investigated.
              </p>
              <p>
              The following advance settings can be adapted:
              
              <ul> <li>	Calculation of  stratified estimates is available, e.g. stratification by study in case data from an integrated database is used. While the stratified incidence proportions are study-size adjusted the risk differences and relative risk are derived using Mantel-Haenszel stratification (not yet available for incidence rates).
              </li>
              <li>
              The analyses can be restricted to Tier 2 events in two ways
              <ul> <li>
              Events can be restricted to minimum number of events in verum required to achieve a significant result
              </li> <li>
              Events can be restricted to only such with an incidence  of at least 1%
              </li> </ul>
              </li> <li>
              p-values can be calculated either one or two-sided.
              
              </li> <li>
              Alpha can be chosen as 1, 5 or 10%.
              </li> </ul>
              </p>
              <p>
              All data provided in the double dot plot can also be found in the View dataset tab and easily be filtered and sorted as required.
              </p>
              <p>
              
              A heatmap based on the MedDRA hierarchy presents the second highlight of DetectoR.
              It provides an appealing and interactive overview of the distribution of adverse events across different groupings.
              With this graphical display either MLGs within SOCs or PTs within SOCs can be discovered by zooming in and out of the SOC.
              The size of the presented boxes is based on the number of events in the corresponding AE category.
              A color coding either based on p-values or effect estimates allows the users to adjust the heat map according to their particular needs. 
              Similar to the generic filtering for the double dot plot described above, multiplicity adjustments,
              consideration of risk estimates and risk differences as well as study stratification are possible for the heatmap.
              </p>
              "))
  })
}

## To be copied in the UI
# mod_helpText_ui("helpText_ui_1")

## To be copied in the server
# callModule(mod_helpText_server, "helpText_ui_1")
