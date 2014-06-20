library(shiny)

fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="pipecomposer.css"),
    tags$script(src="pipecomposer.js")
  ),
  fluidRow(
    column(4,
      h2("Input"),
      uiOutput("left")
    ),
    column(4, id = "pipeline-container",
      h2("Pipeline"),
      lapply(1:8, function(i) {
        tags$textarea(id=paste0("stage", i), class="stage", placeholder="identity()")
      }),
      h4("Code"),
      helpText("Double-click the code to select-all"),
      verbatimTextOutput("code")
    ),
    column(4,
      h2("Output"),
      uiOutput("right")
    )
  )
)
