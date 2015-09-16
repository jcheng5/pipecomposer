#' Visualize pipelines of dplyr/ggvis commands
#'
#' Launches a browser-based user interface for composing chains of piped
#' commands, especially ones that return data frames or \code{ggvis} objects.
#' For more about pipes, see \link[magrittr]{magrittr}.
#'
#' @param stage_count Number of stages to display.
#' @param indent String to use to indent stages two and later when printing code
#'   to the console.
#'
#' @import shiny dplyr ggvis
#' @export
pipecomposer <- function(
  stage_count = getOption("pipecomposer.stage_count", 8),
  indent = getOption("pipecomposer.indent", "  ")) {

  df_to_html <- function(df, rows = 100L) {
    table <- renderTable(head(df, rows))() %>% HTML()
    tagList(
      div(nrow(df), " total rows"),
      table
    )
  }

  ui <- fluidPage(
    tags$head(
      includeScript(system.file("www/pipecomposer.js", package = "pipecomposer")),
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
          font-family: Consolas, monospace;
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
        lapply(0:stage_count, function(i) {
          tags$textarea(id=paste0("stage", i), class="stage", placeholder="identity()")
        }),
        tags$br(),
        actionButton("print", "Print to console")
      ),
      column(4,
        h2("Output"),
        uiOutput("out")
      )
    )
  )

  server <- function(input, output, session) {

    # Load saved state
    if (length(.GlobalEnv$.PipeComposerSavedState) > 0) {
      for (i in 1:length(.GlobalEnv$.PipeComposerSavedState)) {
        updateTextInput(session, paste0("stage", i-1), value = .GlobalEnv$.PipeComposerSavedState[[i]])
      }
    }

    rootEnv <- new.env()
    stages <- reactiveValues()
    getStage <- function(i) {
      stages[[paste0("stage", i)]]()
    }
    setStage <- function(i, code) {
      if (code == "" && i > 0)
        code = "identity()"

      force(code)
      rx <- reactive({
        prevEnv <- if (i == 0) rootEnv else getStage(i-1)$env
        thisEnv <- new.env(parent = prevEnv)
        parsed <- parse(text=code)

        # Hack to simulate %>%, which we can't do yet
        if (i > 0) {
          parentVal <- getStage(i-1)$value
          parsed <- substituteDirect(
            call("%>%", quote(..parent..), parsed[[1]]),
            list(..parent.. = parentVal)
          )
        }

        result <- eval(parsed, envir=thisEnv)
        list(env = thisEnv, value = result)
      })
      stages[[paste0("stage", i)]] <- rx
    }

    lapply(0:stage_count, function(i) {
      setStage(i, "")
      observe({
        setStage(i, input[[paste0("stage", i)]])
      })
      outputFunc <- renderUI({
        result <- getStage(i)$value
        if (is.data.frame(result)) {
          return(df_to_html(result))
        } else if (inherits(result, "ggvis")) {
          return(ggvis::view_static(result))
        } else {
          return(tags$pre(
            paste(capture.output(print(result)), collapse = "\n")
          ))
        }
      })
      output[[paste0("leftOut", i)]] <- outputFunc
      output[[paste0("rightOut", i)]] <- outputFunc
    })

    activeStage <- reactive({
      validate(need(input$activeStage, FALSE))
      input$activeStage
    })

    output$`in` <- renderUI({
      if (activeStage() == 0)
        NULL
      else
        uiOutput(paste0("leftOut", activeStage()-1))
    })
    output$`out` <- renderUI({
      uiOutput(paste0("rightOut", activeStage()))
    })

    stage_values <- reactive({
      sapply(0:stage_count, function(i) {
        input[[paste0("stage", i)]]
      })
    })

    code <- reactive({
      sv <- stage_values()
      structure(
        paste(sv[nzchar(sv)], collapse = paste0(" %>%\n", indent)),
        class = c("verbatim", "character")
      )
    })

    observe({
      .GlobalEnv$.PipeComposerSavedState <- stage_values()
    })

    observeEvent(input$done, {
      stopApp(code())
    })

    observeEvent(input$print, {
      message("\n", code(), "\n")
    })

    session$onEnded(function() {
      message("Final code:\n\n", isolate(code()))
    })
  }

  runApp(shinyApp(ui, server, options = list(launch.browser = TRUE)))
}

#' @export
print.verbatim <- function(x, ...) {
  cat(x, "\n")
}

#' Use Pipe Composer on code pipeline from system clipboard
#'
#' Uses the system clipboard to prepopulate and launch the pipe composer
#' interface. The clipboard must contain a single, valid, pipe-separated
#' expression.
#'
#' @param ... Parameters to pass through to \code{\link{pipecomposer}}.
#'
#' @import clipr
#' @export
import_clipboard <- function(...) {
  expr <- parse(text = read_clip())

  unwind <- function(lang) {
    if (is.symbol(lang)) {
      paste(lang, collapse = "\n")
    } else if (lang[[1]] != "%>%") {
      paste(format(lang), collapse = "\n")
    } else {
      c(
        unwind(lang[[2]]),
        paste(format(lang[[3]]), collapse = "\n")
      )
    }
  }

  stage_values <- unwind(expr[[1]])

  .GlobalEnv$.PipeComposerSavedState <- stage_values
  pipecomposer(...)
}
