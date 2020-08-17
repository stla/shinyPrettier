library(shinyPrettier)

language <- "markdown"
code <- "function test(x){return x+1;}"
# code <- ".myclass{color:red;font-size:10px}"
code <- "
#  Title
*The JavaScript code below will be prettified as well!*
```js
function hello(x){return x+1}
```
Pilot|Airport|Hours
--|:--:|--:
John Doe|SKG|1338
Jane Roe|JFK|314

_______________________
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Curabitur consectetur maximus risus, sed maximus tellus tincidunt et.
"
# code <- paste0(
#   readLines(
#     "~/Work/R/shinyChakraSlider/srcjs/chakraSlider.jsx"
#   ),
#   collapse = "\n"
# )

shinyPrettier(code = code, language = language)

