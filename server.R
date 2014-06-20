library(magrittr)
require(dplyr)
require(ggvis)

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
        if (length(parsed[[1]]) > 1) {
          for (j in length(parsed[[1]]):2) {
            parsed[[1]][[j+1]] <- parsed[[1]][[j]]
          }
        }
        parsed[[1]][[2]] <- as.symbol("..parent..")
        thisEnv[["..parent.."]] <- getStage(i-1)$value
      }

      result <- eval(parsed, envir=thisEnv)
      list(env = thisEnv, value = result)
    })
    stages[[paste0("stage", i)]] <- rx
  }

  lapply(0:8, function(i) {
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

  observe({
    code <- sapply(0:8, function(i) {
      input[[paste0("stage", i)]]
    })
    .GlobalEnv$.PipeComposerSavedState <- code
  })

  observe({
    if (input$print == 0)
      return()
    isolate({
      code <- sapply(0:8, function(i) {
        input[[paste0("stage", i)]]
      })
      code <- paste(code[nzchar(code)], collapse = " %>%\n  ")
      cat("---\n", code)
      cat("\n---\n")
    })
  })
}
