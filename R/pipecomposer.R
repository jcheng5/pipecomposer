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
#' @import shiny
#' @importFrom dplyr arrange
#' @importFrom ggvis layer_points
#' @export
pipecomposer <- function(
  stage_count = getOption("pipecomposer.stage_count", 8),
  indent = getOption("pipecomposer.indent", "  ")) {

  # Represent dataframe in HTML
  df_to_html <- function(df, rows = 100L) {
    table <- HTML(renderTable(head(df, rows))())
    tagList(
      div(nrow(df), " total rows"),
      table
    )
  }

  ui <- fluidPage(
    tags$head(
      includeScript(system.file("www/pipecomposer.js", package = "pipecomposer")),
      includeCSS(system.file("www/pipecomposer.css", package = "pipecomposer"))
    ),
    fluidRow(
      column(4,
        h2("Input"),
        uiOutput("left")
      ),
      column(4, id = "pipeline-container",
        h2("Pipeline"),
        lapply(1:stage_count, function(i) {
          tags$textarea(id=paste0("stage", i), class="stage",
            placeholder = if (i == 1) "NULL" else "identity()"
          )
        }),
        tags$br(),
        actionButton("print", "Print to console")
      ),
      column(4,
        h2("Output"),
        uiOutput("right")
      )
    )
  )

  server <- function(input, output, session) {

    # Load saved state
    if (length(.GlobalEnv$.PipeComposerSavedState) > 0) {
      for (i in 1:length(.GlobalEnv$.PipeComposerSavedState)) {
        updateTextInput(session, paste0("stage", i), value = .GlobalEnv$.PipeComposerSavedState[[i]])
      }
    }

    stages <- reactiveValues()

    # Gets a list with the env and value of that stage; env is
    # the environment that the code executed in, value is the result
    getStage <- function(i) {
      stages[[paste0("stage", i)]]()
    }
    # Replaces the code for stage `i`--this invalidates stages i and greater
    setStage <- function(i, code) {
      if (code == "" && i > 1)
        code = "identity()"

      force(code)
      rx <- reactive({
        prevEnv <- if (i == 1) .GlobalEnv else getStage(i-1)$env
        thisEnv <- new.env(parent = prevEnv)
        parsed <- parse(text=code)

        if (i > 1) {
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

    # Now initialize all the stages
    lapply(1:stage_count, function(i) {
      # Initially there is no code
      setStage(i, "")
      # Whenever the code changes for a stage, call setStage
      observe({
        setStage(i, input[[paste0("stage", i)]])
      })
      # We'll use the same renderUI logic for the left and right
      # result-viewing panes, so assign it to a variable
      outputFunc <- renderUI({
        # getStage(i) executes whatever is necessary and takes a
        # reactive dependency on the stage (and indirectly, all
        # preceding stages as well)
        result <- getStage(i)$value
        # Now that we've got a result, let's render it.
        # TODO: Make this behavior customizable/extensible
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

    # Returns an integer indicating which stage is being viewed.
    # If none is active, a validation error is raised.
    activeStage <- reactive({
      validate(need(input$activeStage, FALSE))
      input$activeStage
    })

    # The left result-viewing pane shows the previous stage
    # output, unless we're at the first stage, then it's empty
    output$left <- renderUI({
      if (activeStage() == 1)
        NULL
      else
        uiOutput(paste0("leftOut", activeStage()-1))
    })

    # The right result-viewing pane shows the active stage output
    output$right <- renderUI({
      uiOutput(paste0("rightOut", activeStage()))
    })

    # All of the stages, as a character vector of length stage_count.
    stage_values <- reactive({
      sapply(1:stage_count, function(i) {
        input[[paste0("stage", i)]]
      })
    })

    # The combined pipeline code as a single string.
    code <- reactive({
      sv <- stage_values()
      structure(
        paste(sv[nzchar(sv)], collapse = paste0(" %>%\n", indent)),
        class = c("verbatim", "character")
      )
    })

    # Whenever any code changes, save it as a global so we can
    # prepopulate the stages on reload. (Clearly this is a single
    # user system!) Alternatively we could save the state in the
    # browser's localStorage, or as URL data, or...
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
