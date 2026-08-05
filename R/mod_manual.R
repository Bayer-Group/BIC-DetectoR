#' manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for `{shiny}`.
#'
#' @noRd

mod_manual_ui <- function(id) {
  bslib::card(
    class = "about border-0 shadow-sm mx-auto",
    fill = FALSE,
    shiny::h1("Data Manual Tab"),
    shiny::img(
      src = "www/logos/AppIcon_BAG_DetectoR_210x210mm_RGB.png",
      class = "detector-logo-about",
      alt = "DetectoR logo"
    ),
    shiny::h2("File Format and Structure"),
    shiny::h3("File Format"),
    shiny::p(
      "DetectoR is designed to upload the CDISC datasets ADSL and ADAE."
    ),
    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"),
      shiny::strong("Note: "),
      "Accepted formats are SAS (.sas7bdat), R (.rds) or coma-separated (.csv)."
    ),
    shiny::h3("File Structure"),
    shiny::p(
      "In order to use the DetectoR, the two SAS data sets have to
      include the following variables:"
    ),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$th("Dataset"),
        shiny::tags$th("Required variables")
      ),
      shiny::tags$tr(
        shiny::tags$th("ADSL", rowspan = "5"),
        shiny::tags$td("STUDYID (Study identifier): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "USUBJID (Unique subject identifier): Character variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "Safety analysis set flag:  Variable to
        identify subjects in the Safety analysis set. Can be set manually from
        the available ADSL variables"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "DUREXP (Duration of exposure): Integer variable defining
            the treatment duration. Only needed, if it is intended to
            present incidence rates"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "Groups to be compared,
            usually treatment arms. Any ADSL variable can be selected"
        )
      ),
      shiny::tags$tr(
        shiny::tags$th("ADAE", rowspan = "7"),
        shiny::tags$td("STUDYID (Study identifier): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "USUBJID (Unique subject identifier): Character variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "AEBODSYS (Body System or Organ Class): Character variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "AEDECOD (Dictionary-Derived Preferred Term): Character
          variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "AEPTCD, M_PT or pt_code (Preferred Term code): Character
          variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "AAESDURN (Time until AE): Integer variable defining the
          number of days from reference day to start date of the event.
          Only needed, if it is intended to present incidence rates"
        )
      )
    ),
    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"),
      shiny::strong("Note: "),
      "ADSL and ADAE will be merged by STUDYID and USUBJID."
    ),
    shiny::h3("MedDRA data information"),
    shiny::div(
      class = "alert alert-info",
      shiny::icon("info-circle"),
      shiny::strong("Note: "),
      "To use MedDRA within the app, the MedDRA datasets are
      required, in SAS (.sas7bdat), R (.rds) or .csv format"
    ),
    shiny::tags$table(
      shiny::tags$tr(
        shiny::tags$th("Dataset"),
        shiny::tags$th("Required variables")
      ),
      shiny::tags$tr(
        shiny::tags$th("MedDRA Medical Terms data", rowspan = "5"),
        shiny::tags$td("MT_PT (Preferred Term): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td("MT_HLT (High Level Term): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td("MT_HLGT (High Level Group Term): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "MT_SOC (Body System or Organ Class): Character variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "VERSION (MedDRA Version number): Character or numeric
          variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$th(
          "MedDRA SMQ (Standardised MedDRA Queries) data",
          rowspan = "9"
        ),
        shiny::tags$td("PT_NAME (Preferred Term): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td("PT_CODE (Preferred Term code): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td("PT_SMQ_NAME (SMQ Preferred Term): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td("SMQ_NAME (SMQ name): Character variable")
      ),
      shiny::tags$tr(
        shiny::tags$td(
          'SMQ_TYPE (SMQ type): Character variable ("SMQ" or "MLG")'
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          'SMQ_STATUS (SMQ status): Character variable (currently used:
            "RRL" or "PRL")'
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "SMQ_ASS_SOC_NAME (SMQ-associated Body System or Organ
            Class): Character variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "SMQ_ASS_SOC_CODE (SMQ-associated Body System or Organ
            Class code): Character variable"
        )
      ),
      shiny::tags$tr(
        shiny::tags$td(
          "SMQ_MEDDRA_VERSION (MedDRA Version number): Character or
            numeric variable"
        )
      )
    ),
    shiny::div(
      class = "alert alert-warning",
      shiny::icon("warning"),
      shiny::strong("Important! "),
      "All variable names in the table above are case-sensitive, i.e., if
      ADSL contains the row 'usubjid' (lower case), DetectoR will
      not work."
    )
  )
}

#' manual Server Function
#'
#' @noRd
mod_manual_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {})
}
