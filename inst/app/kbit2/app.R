
# Define UI for application that draws a histogram
ui <- shiny::fluidPage(

    shiny::includeCSS("www/kbit2css.css"),

    shiny::div(class = "headImg", shiny::img(src = 'images/logo-abc-ds.svg')),

    # Application title
    shiny::titlePanel("Kaufman Brief Intelligence Test"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("age_at_visit", label = "Age of the participant at visit", value = NULL),
        shiny::numericInput("kbit2verbkraw", label = "Verbal Knowledge Raw Score", value = NULL),
        shiny::numericInput("kbit2ridraw", label = "Riddles Raw Score", value = NULL),
        shiny::numericInput("kbit2vnonvraw", label = "Nonverbal Raw Score", value = NULL)
        ),

        # Show a plot of the generated distribution
      shiny::mainPanel(
        shinny::p("Before entering the raw scores, confirm that you have identified the correct basal item. 
                  This will be either item #1 or the designated basal item based on age (i.e., Verbal Knowledge #5 for ages 6-7, #10 for age 8, #15 for age 9, #20 for ages 11-15, and #25 for ages 16-90).
                  The basal item is determined when the participant correctly answers the first three items in a row. 
                  Only record errors made after the established basal point"),
        shiny::htmlOutput("kbitInputs"),
        shiny::HTML("<hr>"),
        shiny::htmlOutput("kbit2verbstd"),
        shiny::htmlOutput("kbit2nonvbstd"),
        shiny::htmlOutput("kbit2iqcompstd"),
        shiny::htmlOutput("vagequiv"),
        shiny::htmlOutput("nvagequiv"),
        #shiny::tableOutput("kbit2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data <- shiny::reactive({
    shiny::req(input$age_at_visit, input$kbit2verbkraw, input$kbit2ridraw, input$kbit2vnonvraw)
    abcds::calculate_kbit2_score(
      age_at_visit = input$age_at_visit,
      kbit2verbkraw = input$kbit2verbkraw,
      kbit2ridraw = input$kbit2ridraw,
      kbit2vnonvraw = input$kbit2vnonvraw
    )
  })

  output$kbit2 <- shiny::renderTable({ data() })

  output$kbitInputs <- shiny::renderText({
    sprintf(
      "<h4>KBIT-2 results for a <span class='addcolor'>%i-year</span> old participant with a verbal score of <span class='addcolor'>%i</span> and nonverbal score of <span class='addcolor'>%i</span></h4>",
      data()$age_at_visit, data()$kbit2verbraw, data()$kbit2vnonvraw
      )
  })

  output$kbit2verbstd <- shiny::renderText({
    sprintf(
      "<ul><b>Verbal Standard Score</b>: <span class='addcolor'>%i (90%% CI: %s)</span></ul>",
      data()$kbit2verbstd, data()$kbit2verbstdci
    )})

    output$kbit2nonvbstd <- shiny::renderText({
      sprintf(
        "<ul><b>Nonverbal Standard Score</b>: <span class='addcolor'>%i (90%% CI: %s)</span></ul>",
        data()$kbit2nonvbstd, data()$kbit2nonvbstdci
    )})

    output$kbit2iqcompstd <- shiny::renderText({
      sprintf(
        "<ul><b>Intelligent Quotient</b>: <span class='addcolor'>%i (90%% CI: %s)</span></ul>",
        data()$kbit2iqcompstd, data()$kbit2iqcompci
    )})

    output$vagequiv <- shiny::renderText({
      sprintf(
        "<ul><b>Verbal Age Equivalent</b>: <span class='addcolor'>%s:%s</span></ul>",
        data()$kbit2verbaey, data()$kbit2verbae
    )})

    output$nvagequiv <- shiny::renderText({
      sprintf(
        "<ul><b>Nonverbal Age Equivalent</b>: <span class='addcolor'>%s:%s</span></ul>",
        data()$kbit2nonvbaey, data()$kbit2nonvbae
      )})
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
