library(shinyPrettier)

language <- "jsx"
code <- "function test(x){return x+1;}"
# code <- ".myclass{color:red;font-size:10px}"
code <- "
# Title
```js
function hello(x){return x+1}
```
Pilot|Airport|Hours
--|:--:|--:
John Doe|SKG|1338
Jane Roe|JFK|314
"
# code <- paste0(
#   readLines(
#     "~/Work/R/shinyChakraSlider/srcjs/chakraSlider.jsx"
#   ),
#   collapse = "\n"
# )

shinyPrettier(code = code, language = language)

