library(shiny)
library(shinyPrettier)


#library(shinyMultiActionButton)

xsubButton <- function(
  id, bg = NULL, fg = NULL, icon = NULL, iconSize = "3x",
  tooltip = NULL, onclick = NULL
){
  # bg <- if(!is.null(bg)) match.arg(bg, metro4bgcolors())
  # fg <- if(!is.null(fg)) match.arg(fg, metro4fgcolors())
  metro4icon <- FALSE
  if(!is.null(icon)){
    # if(icon %in% metro4icons()){
    #   icon <- sprintf("mif-%s", icon)
    #   iconSize <- sprintf("mif-%s", iconSize)
    #   metro4icon <- TRUE
    # }else{
      iconSize <- character(1L)
    # }
  }
  tags$li(
    class = if(metro4icon) paste0(sprintf("bg-%s ", bg), sprintf("fg-%s", fg)),
    style = if(!metro4icon) "background-color: transparent;",
    tags$a(
      id = id,
      href = "#",
      class = "action-button mui-action-button",
      onclick = onclick,
      tags$span(
        class = sprintf("%s %s", icon, iconSize),
        title = tooltip
      )
    )
  )
}

xmultiActionButton <- function(
  rotate = TRUE, bg = NULL, fg = NULL,
  icon = NULL,
  direction = "right", subButtons
){
  rotate <- ifelse(rotate, "rotate-minus ", "")
  #bg <- if(!is.null(bg)) match.arg(bg, metro4bgcolors())
  #fg <- if(!is.null(fg)) match.arg(fg, metro4fgcolors())
  # icon <- if(!is.null(icon)){
  #   if(icon %in% metro4icons()){
  #     sprintf("mif-%s", icon)
  #   }else{
  #     icon
  #   }
  # }
  ultag <- function(...){
    tags$ul(
      class = sprintf("actions drop-%s", direction),
      ...
    )
  }
  tag <- tags$div(
    class = "multi-action",
    tags$button(
      class = paste0(
        "action-button  mui-action-button ",
        rotate,
        sprintf("bg-%s ",bg),
        sprintf("fg-%s",fg)
      ),
      onclick = "$(this).toggleClass('active')",
      tags$span(
        class = "icon",
        icon
        # tags$span(
        #   class = icon
        # )
      )
    ),
    do.call(ultag, subButtons)
  )
  tag
  # dep <- htmltools::htmlDependency(
  #   name = "metroui",
  #   version = "4.3.10",
  #   src = "www/metro4-dist",
  #   stylesheet = paste0("css/metro-", c(
  #     "colors",
  #     "components",
  #     "icons"
  #   ), ".min.css"),
  #   package = "shinyMultiActionButton"
  # )
  # htmltools::attachDependencies(tag, dep)
}


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "css.css"),
    tags$link(rel = "stylesheet", href = "wwwSP/freeicons.css"),
    tags$link(rel = "stylesheet", href = "wwwSP/SuperTinyIcons.css")
  ),

  xmultiActionButton(
    bg = "red", fg = "white",
    icon = icon("code"), direction = "bottom",
    subButtons = list(
      xsubButton(id = "css", icon = "freeicon-css",
                tooltip = "css",
                onclick = "Shiny.setInputValue('language','css');"),
      xsubButton(id = "html", icon = "freeicon-html",
                tooltip = "html",
                onclick = "Shiny.setInputValue('language','html');"),
      xsubButton(id = "javascript", icon = "supertinyicon-javascript",
                tooltip = "javascript",
                onclick = "Shiny.setInputValue('language','javascript');"),
      xsubButton(id = "markdown", icon = "supertinyicon-markdown",
                tooltip = "markdown",
                onclick = "Shiny.setInputValue('language','markdown');"),
      xsubButton(id = "react", icon = "supertinyicon-react",
                tooltip = "jsx",
                onclick = "Shiny.setInputValue('language','jsx');")
    )
  )
)


server <- function(input, output){

}

shinyApp(ui, server)
