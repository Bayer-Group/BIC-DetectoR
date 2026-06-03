#' help UI Function
#'
#' @description A shiny Module.
#'
#' @inheritParams mod_upload_server
#'
#' @noRd

mod_about_ui <- function(id) {
  shiny::wellPanel(
    class = "about",
    shiny::h1(
      "DetectoR R Shiny app for Clinical Trial Safety Data"
    ),
    shiny::div(
      class = "detector-logo-about",
      alt = "DetectoR logo"
    ),
    shiny::hr(),
    shiny::h2("General concept"),
    shiny::HTML(
      "<p>The DetectoR R Shiny app provides a handy platform allowing for
      <strong>early identification of signals</strong> and an ongoing
      <strong>monitoring of safety</strong> along the medical product
      development phase and lifecycle.</p>"
    ),
    shiny::HTML(
      "<p>The application of DetectoR in the submission process supports the
      label creation and simplifies the identification of
      <strong>label-relevant adverse events</strong>. State-of-the-art
      statistical methodology is incorporated in the R Shiny application,
      including the use of <strong>graphical displays</strong> as well as
      <strong>statistical tests and multiplicity-adjusted p-values</strong>.
      </p>"
    ),
    shiny::hr(),
    shiny::h2("Data requirements"),
    shiny::HTML(
      "<p><strong>CDISC datasets (ADSL & ADAE)</strong> from studies or pools
      can be uploaded easily without any further pre-processing into the
      DetectoR app. For further information on data set requirements, please
      see the <em>Data manual tab</em>.</p>"
    ),
    shiny::HTML(
      "<p><strong>Demo data</strong> is available to become easily acquainted
      with the functionality of the DetectoR app.</p>"
    ),
    shiny::HTML(
      "<p><strong>MedDRA datasets</strong> can be uploaded to perform an
      analysis using Standardized MedDRA
      queries (SMQs). If not, <em>'Run without MedDRA'</em> can be chosen
      instead.</p>"
    ),
    shiny::hr(),
    shiny::h2("Data selection and filtering"),
    shiny::HTML(
      "<p>After datasets are uploaded, the <strong>variable defining the
      (treatment) groups to be compared</strong> has to be selected out of the
      variables available in ADSL. Subsequently, the categories defining
      <strong>the Verum and the Comparator</strong> need to be identified.</p>"
    ),
    shiny::HTML(
      "<p>Based on subject level characteristics and adverse event categories,
      generic <strong>data filtering</strong> can be applied any time by using
      the <em>Filter tab</em>. For example, the analyses can be restricted to
      Serious Adverse Events (SAEs) or adverse events leading to
      discontinuation, to focus on events of higher severity or impact.</p>"
    ),
    shiny::HTML(
      "<p>Additionally, <strong>patient-level filters</strong> can be added,
      e.g., for baseline characteristics like the usual covariates (sex, age or
      BMI), but also for any co-morbidity or risk factor included in the input
      data set.</p>"
    ),
    shiny::hr(),
    shiny::h2("Data visualization"),
    shiny::HTML(
      "<p>After the data is uploaded, treatment is defined and filters are
      applied, the tabs <em>Double Dot Plot</em>, <em>Heatmap</em>, <em>Volcano
      Plot</em>, and <em>View dataset</em> are available to explore the data.
      </p>"
    ),
    shiny::h3("The Double Dot Plot"),
    shiny::img(
      src = "www/screenshots/screenshot-ddp.jpeg",
      alt = "Example of Double Dot Plot with demo data"
    ),
    shiny::HTML(
      "<p>The heart of DetectoR is the Double Dot Plot, which allows for a
      <strong>clear overview</strong> of dense information <strong>gaining data
      insights quickly</strong>.</p>"
    ),
    shiny::HTML(
      "<p>The Double Dot Plot shows, for each adverse event of interest, the
      <strong>incidence proportion</strong> or <strong>incidence rate</strong>
        per treatment group, combined with an <strong>effect estimate</strong>.
      For comparison of the treatments, <strong>risk differences (RD)</strong>
        and <strong>relative risks (RR)</strong> can be chosen.</p>"
    ),
    shiny::h4("p-value calculation"),
    shiny::HTML(
      "<p>For a more straightforward detection of relevant signals, different
      techniques for calculation of <strong>multiplicity adjusted p-values
      </strong> are implemented and can be chosen from the parameter settings,
      i.e. the <strong>False Discovery rate (FDR)</strong> and the <strong>new
      Double FDR (DFDR)</strong> [Add here reference paper].</p>"
    ),
    shiny::div(
      shiny::icon("info-circle"),
      shiny::strong("Note: "),
      "The new Double FDR (DFDR) method is only availabe for Preferred Terms
      (PTs) and custom groupings that are mutually exclusive.",
      class = "callout-info"
    ),
    shiny::HTML(
      "<p>With the option to <strong>order the adverse events based on either
      the adjusted p-values or the risk estimates</strong>,
      the relevant safety signals can be detected easily.</p>"
    ),
    shiny::h4("Adverse Events grouping"),
    shiny::HTML(
      "<p>A first glance on the overall safety profile can be drawn from
      comparing the <strong>Body System Organ Classes (SOCs)</strong>.
      <strong>Preferred Terms (PTs)</strong> can be also investigated.
      Additionally, a presentation by <strong>Standardized MedDRA Queries
      (SMQs)</strong>, including all parent and sub-SMQs is possible.</p>"
    ),
    shiny::div(
      shiny::icon("info-circle"),
      shiny::strong("Note: "),
      "Standardized MedDRA Queries (SMQs)
      are available only if MedDRA files are uploaded.",
      class = "callout-info"
    ),
    shiny::h4("Advanced settings"),
    shiny::HTML(
      "<p>The following advance settings can be adapted:
        <ul>
          <li>Calculation of <strong>stratified estimates</strong> is
         available, e.g., stratification by study, in case data from an
         integrated database is used.
         While the stratified incidence proportions are study-size
        adjusted, the risk differences and relative risk are derived using
        Mantel-Haenszel stratification.</li>
          <li>The analyses can be restricted to <strong>Tier 2 events</strong>
          in two ways:</li>
          <ul>
            <li>Events can be restricted to minimum number of events in verum
           required to achieve a significant result.</li>
            <li>Events can be restricted to only such with an incidence  of at
           least 1%.</li>
          </ul>
          <li>p-values can be calculated either one or two-sided</li>
          <li>The <strong>significance level (alpha)</strong> can be chosen as
        1, 5 or 10%.</li>
        </ul>
      </p>"
    ),
    shiny::h3("The Data View"),
    shiny::HTML(
      "<p>All data provided in the Double Dot Plot can also be found in the
      <em>View dataset tab</em> and easily be filtered and sorted as required.
      </p>"
    ),
    shiny::h3("The Heatmap"),
    shiny::img(
      src = "www/screenshots/screenshot-heatmap.jpeg",
      alt = "Example of Heatmap with demo data"
    ),
    shiny::HTML(
      "<p>A heatmap/treemap based on the MedDRA hierarchy presents the second
      highlight of DetectoR. It provides an appealing and interactive overview
      of the <strong>distribution of adverse events across different groupings
      </strong>.
      With this graphical display either custom groupings within SOCs or PTs within SOCs
      can be discovered by zooming in and out of the SOC.</p>"
    ),
    shiny::HTML(
      "<p>The size of the presented boxes is based on the number of events in
      the corresponding AE category.
      A color coding either based on p-values or effect estimates allows the
      users to adjust the heat map according to their particular needs.</p>"
    ),
    shiny::HTML(
      "<p>Similar to the generic filtering for the double dot plot described
      above, multiplicity adjustments,
      consideration of risk estimates and risk differences as well as study
      stratification are possible for the heatmap.</p>"
    ),
    shiny::h3("The Volcano Plot"),
    shiny::img(
      src = "www/screenshots/screenshot-volcano.jpeg",
      alt = "Example of Volcano Plot with demo data"
    ),
    shiny::HTML(
      "<p>Another feature of the DetectoR app is the Volcano Plot.
      This plot offers an interactive display of the Adverse Event signals by
      <strong>plotting the risk estimate of each adverse event against the
      p-value</strong>.
      Additionally, the red/blue colour coding and plot positioning offers an
      insight in whether each event is significant for the Verum or Comparator.
      Like the Double Dot Plot, the event types shown can be changed.
      Other options include choosing to display the RR or the RD and choosing
      the displayed Alpha level as 1%, 5% or 10%.
      Using the plotly package, this graphic is vastly <strong>interactive
      </strong>, offering the option to select points, alter the display and
      download the plot.</p>"
    )
  )
}


#' help Server Function
#'
#' @inheritParams mod_upload_server
#'
#' @noRd
mod_about_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {})
}
