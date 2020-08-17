# Icon made by [author link] from www.freeicons.io
# E.g.: Icon made by Free icons from www.freeicons.io

#' @import shiny
#' @importFrom shinythemes shinytheme
#' @importFrom shinyMultiActionButton multiActionButton subButton
#' @importFrom shinyAce aceEditor
#' @noRd
ui <- function(language, code, parser, theme, fontSize){

  fluidPage(
    theme = shinytheme("cyborg"),
    tags$head(
      tags$script(src = "wwwSP/bootstrap-flash-alert.js"),
      tags$link(rel = "stylesheet", href = "wwwSP/animate.css"),
      tags$script(src = "wwwSP/standalone.js"),
      tags$script(src = "wwwSP/parser-babel.js"),
      tags$script(src = "wwwSP/parser-html.js"),
      tags$script(src = "wwwSP/parser-markdown.js"),
      tags$script(src = "wwwSP/parser-postcss.js"),
      tags$script(src = "wwwSP/shinyPrettier.js"),
      tags$link(rel = "stylesheet", href = "wwwSP/shinyPrettier.css"),
      tags$link(rel = "stylesheet", href = "wwwSP/freeicons.css"),
      tags$link(rel = "stylesheet", href = "wwwSP/SuperTinyIcons.css")
    ),

    br(),

    fluidRow(

      column(
        width = 1,
        multiActionButton(
          bg = "red", fg = "white",
          icon = "cog", direction = "bottom",
          subButtons = list(
            subButton(id = "css", icon = "freeicon-css",
                      tooltip = "css",
                      onclick = "Shiny.setInputValue('language','css');"),
            subButton(id = "html", icon = "freeicon-html",
                      tooltip = "html",
                      onclick = "Shiny.setInputValue('language','html');"),
            subButton(id = "javascript", icon = "supertinyicon-javascript",
                      tooltip = "javascript",
                      onclick = "Shiny.setInputValue('language','javascript');"),
            subButton(id = "markdown", icon = "supertinyicon-markdown",
                      tooltip = "markdown",
                      onclick = "Shiny.setInputValue('language','markdown');"),
            subButton(id = "react", icon = "supertinyicon-react",
                      tooltip = "jsx",
                      onclick = "Shiny.setInputValue('language','jsx');")
          )
        )
      ),

      column(
        width = 11,

        fluidRow(
          column(
            width = 12,
            verbatimTextOutput("error")
          )
        ),

        fluidRow(
          column(
            width = 6,
            fluidRow(
              column(
                width = 6,
                h2(class = "header", "Your code")
              ),
              column(
                width = 6,
                actionButton("prettify", "Prettify", class = "btn-danger btn-lg")
              )
            )
          ),
          column(
            width = 6,
            fluidRow(
              column(
                width = 12,
                h2(class = "header", "Prettified code")
              )
              # column(
              #   width = 6,
              #   actionButton("copy", "Copy",
              #                class = "btn-danger", style = "float: right;")
              # )
            )
          )
        ),

        fluidRow(
          column(
            width = 6,
            aceEditor(
              "code",
              value = code,
              mode = language,
              theme = theme,
              fontSize = fontSize,
              height = "400px"
            )
          ),
          column(
            width = 6,
            aceEditor(
              "ace",
              value = "",
              mode = language,
              theme = theme,
              fontSize = fontSize,
              readOnly = TRUE,
              height = "400px"
            )
          )
        )
      )
    )

  )
}

#' @importFrom shinyAce updateAceEditor
server <- function(language, notfound){
  function(input, output, session){

    if(notfound){
      flashMessage <- list(
        message = "Use the button to select a language.",
        title = "Language not recognized",
        type = "warning",
        icon = "glyphicon glyphicon-ban-circle",
        withTime = FALSE,
        closeTime = 10000,
        animShow = "flash",
        animHide = "backOutDown",
        position = list("center", list(0, 0))
      )
      session$sendCustomMessage("flash", flashMessage)
      session$sendCustomMessage("disableButton", TRUE)
    }

    Language <- reactiveVal(language)

    observeEvent(input[["prettify"]], {
      parser <- switch(
        Language(),
        css = "css",
        html = "html",
        javascript = "babel",
        jsx = "babel",
        markdown = "markdown"
      )
      session$sendCustomMessage("code", list(code = input[["code"]], parser = parser))
    })

    observeEvent(input[["prettyCode"]], {
      updateAceEditor(session, "ace", value = input[["prettyCode"]])
      if(input[["prettyCode"]] != ""){
        flashMessage <- list(
          message = "Pretty code copied to the clipboard",
          title = "Copied!",
          type = "success",
          icon = "glyphicon glyphicon-check",
          withTime = TRUE
        )
        session$sendCustomMessage("flash", flashMessage)
      }
    })

    observeEvent(input[["prettifyError"]], {
      flashMessage <- list(
        message = "Prettifier has failed",
        title = "Error!",
        type = "danger",
        icon = "glyphicon glyphicon-flash",
        withTime = TRUE,
        animShow = "rotateInDownLeft",
        animHide = "bounceOutRight",
        position = list("bottom-left", list(0, 0.01))
      )
      session$sendCustomMessage("flash", flashMessage)
    })

    output[["error"]] <- renderPrint({
      cat(input[["prettifyError"]])
    })

    observeEvent(input[["language"]], {
      session$sendCustomMessage("disableButton", FALSE)
      updateAceEditor(session, "ace", mode = input[["language"]])
      updateAceEditor(session, "code", mode = input[["language"]])
      Language(input[["language"]])
    })

  }
}


#' Shiny prettier
#' @description Launch a Shiny app for code prettifying.
#'
#' @param file file containing the code to be prettified
#' @param language language; if \code{file} is provided, then the language
#'   is guessed from the file extension, and it is not necessary to set this
#'   option unless the extension is not standard; available languages are:
#'   \code{"css"}, \code{"html"}, \code{"javascript"}, \code{"jsx"}, and
#'   \code{"markdown"}
#' @param code ignored if \code{file} is provided, otherwise a character string
#'   containing the code to be prettified; the \code{language} option must be
#'   set if you use this way
#' @param theme,fontSize options passed to
#'   \code{\link[shinyAce:aceEditor]{aceEditor}}
#'
#' @importFrom tools file_ext
#' @export
#'
#' @examples code <- "
#' #  Title
#' *The JavaScript code below will be prettified as well!*
#' ```js
#' function hello(x){return x+1}
#' ```
#' Pilot|Airport|Hours
#' --|:--:|--:
#' John Doe|SKG|1338
#' Jane Roe|JFK|314
#'
#' _______________________
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit.
#' Curabitur consectetur maximus risus, sed maximus tellus tincidunt et.
#' "
#'
#' shinyPrettier(language = "markdown", code = code)
shinyPrettier <- function(file, language, code,
                          theme = "cobalt", fontSize = 16){
  notfound <- FALSE
  if(!missing(file)){
    code <- paste0(readLines(file), collapse = "\n")
    ext <- file_ext(file)
    if(missing(language)){
      language <- switch(
        tolower(ext),
        css = "css",
        html = "html",
        js = "javascript",
        jsx = "jsx",
        md = "markdown",
        markdown = "markdown",
        rmd = "markdown"
      )
      if(is.null(language)){
        notfound <- TRUE
        language <- "text"
      }
    }
  }
  parser <- switch(
    language,
    css = "css",
    html = "html",
    javascript = "babel",
    jsx = "babel",
    markdown = "markdown",
    text = "text"
  )
  if(is.null(parser)){
    stop("Unrecognized language.")
  }
  require(shiny)
  require(shinyAce)
  require(shinythemes)
  require(shinyMultiActionButton)
  shinyApp(ui(language, code, parser, theme, fontSize),
           server(language, notfound))
}



