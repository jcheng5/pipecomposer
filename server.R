library(magrittr)
require(dplyr)
require(ggvis)

# Represent dataframe in HTML
df_to_html <- function(df, rows = 100L) {
  table <- renderTable(head(df, rows))() %>% HTML()
  tagList(
    div(nrow(df), " total rows"),
    table
  )
}

function(input, output, session) {

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
      parsed <- parse(text = code)
      expr <- if (length(parsed) > 0) parsed[[1]] else NULL

      # Hack to simulate %>%, which we can't do yet
      if (i > 1) {
        lastValue <- getStage(i-1)$value
        expr <- substitute(a %>% b, list(a=lastValue, b=expr))
      }

      result <- eval(expr, envir=thisEnv)
      list(env = thisEnv, value = result)
    })
    stages[[paste0("stage", i)]] <- rx
  }

  # Now initialize all the stages
  lapply(1:8, function(i) {
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

  # Whenever any code changes, save it as a global so we can
  # prepopulate the stages on reload. (Clearly this is a single
  # user system!) Alternatively we could save the state in the
  # browser's localStorage, or as URL data, or...
  observe({
    code <- sapply(1:8, function(i) {
      input[[paste0("stage", i)]]
    })
    .GlobalEnv$.PipeComposerSavedState <- code
  })

  # Pretty-print the code
  output$code <- renderText({
    code <- sapply(1:8, function(i) {
      input[[paste0("stage", i)]]
    })
    code <- paste(code[nzchar(code)], collapse = " %>%\n  ")
  })
}
