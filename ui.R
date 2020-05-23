# Project: Compare Corona virus outbreak between countries
# Author: Monique van der Voet

ui <- fluidPage(
  h3("Compare Corona virus outbreak between countries"),
  p(
    em(
      "You can offset the timelines to overlap milestone events, for example lockdowns"
    )
  ),
  
  wellPanel(
    # Select country
    fluidRow(
      column(
        4,
        selectizeInput(
          inputId = "ref_id",
          # do not change name
          label = "Select first country:",
          choices = NULL,
          options = list(
            placeholder = "e.g. Netherlands",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        4,
        selectizeInput(
          inputId = "query_id",
          # do not change name
          label = "Select second country:",
          choices = NULL,
          options = list(
            placeholder = "e.g. China",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        4,
        selectizeInput(
          inputId = "query2_id",
          # do not change name
          label = "Select third country:",
          choices = NULL,
          options = list(
            placeholder = "e.g. Italy",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    ),
    
    # Select date
    fluidRow(
      column(
        4,
        p(
          em(
            "A milestone date in The Netherlands is",
            br(),
            "2020-03-15: start social distancing"
          )
        ),
        selectizeInput(
          inputId = "ref_date",
          # do not change name
          label = "Milestone date in first country:",
          choices = NULL,
          options = list(
            placeholder = "e.g. 2020-03-15",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      
      column(
        4,
        p(
          em(
            "A milestone date in China is",
            br(),
            "2020-01-23: lockdown of Wuhan"
          )
        ),
        selectizeInput(
          inputId = "query_date",
          # do not change name
          label = "Milestone date in second country:",
          choices = NULL,
          options = list(
            placeholder = "e.g. 2020-01-23",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      
      column(
        4,
        p(
          em(
            "A milestone date in Italy is",
            br(),
            "2020-03-09: lockdown of Italy"
          )
        ),
        selectizeInput(
          inputId = "query2_date",
          # do not change name
          label = "Milestone date in third country:",
          choices = NULL,
          options = list(
            placeholder = "e.g. 2020-03-09",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    ),
    
    # Compare_Button
    fluidRow(actionButton("goCompare", "Go!"))
  ),
  br(),
  br(),
  
  # Compare_Output
  fluidRow(plotOutput("plotCompare")),
  br(),
  
  p(
    em(
      "Source data: European Center for Disease Control and Prevention (ECDC)"
    )
  ),
  br()
  
)
