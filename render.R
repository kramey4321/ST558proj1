
# Code to Render:

rmarkdown::render(input = 'MyRmd.Rmd',output_format = 'github_document', 
                  output_file = "README.md", 
                  output_options = list(html_preview = FALSE))
