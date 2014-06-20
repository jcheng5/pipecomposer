library(shiny)

fluidPage(
  tags$head(
    tags$style(type="text/css", "
      html, body {
        height: 100%;
      }
      body {
        zoom: 80%;
      }
      h2 {
        text-align: center;
      }
      .stage {
        width: 80%;
        font-family: monospace;
      }
      .stage.active {
        background-color: #FFD;
      }
      #in {
        overflow: scroll;
        height: 100%;
      }
      #out {
        overflow: scroll;
        height: 100%;
      }
      #pipeline-container {
        background-color: #EEE;
        text-align: center;
        height: 100%;
      }
      .container-fluid, .row-fluid {
        height: 100%;
      }
    "),
    tags$script(src="pipecomposer.js")
  ),
  fluidRow(
    column(4,
      h2("Input"),
      uiOutput("in")
    ),
    column(4, id = "pipeline-container",
      h2("Pipeline"),
      lapply(0:8, function(i) {
        tags$textarea(id=paste0("stage", i), class="stage", placeholder="identity()")
      }),
      actionButton("print", "Print to console")
    ),
    column(4,
      h2("Output"),
      uiOutput("out")
    )
  )
)
