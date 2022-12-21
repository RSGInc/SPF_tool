showWarnings=FALSE
rm(list=ls())

### Read JSON function
parseJSON = function(json_obj) {
  # isString = is.character(json_obj) & length(json_obj) == 1
  isDF = all(grepl('\\[\\\n', json_obj) & length(json_obj) == 1)

  # Check if dataframe object
  if (isDF) {
    return( jsonlite::fromJSON(json_obj) )
  }
  else {
    return( json_obj )
  }
}

### Load serialized data
json_base = jsonlite::fromJSON('data/visualizer/summaries_base.json')
json_build = jsonlite::fromJSON('data/visualizer/summaries_build.json')
json_parameters = jsonlite::fromJSON('data/visualizer/summary_parameters.json')

### Parse JSON data
base_data = lapply(json_base, parseJSON)
build_data = lapply(json_build, parseJSON)
parameters = lapply(json_parameters, parseJSON)

### Send vars to environment
for (var_name in names(parameters)) {
  assign(var_name, parameters[[var_name]])
}
rm(var_name)

### Load required libraries
SYSTEM_REPORT_PKGS = c("DT", "flexdashboard", "leaflet", "geojsonio",
                        "htmltools", "htmlwidgets", "kableExtra",
                        "knitr", "mapview", "plotly", "RColorBrewer",
                        "rgdal", "rgeos", "crosstalk","treemap", "htmlTable",
                        "rmarkdown", "scales", "stringr", "jsonlite",
                        "pander", "ggplot2", "reshape", "raster", "dplyr",
                        "yaml","data.table")

installed_packages = SYSTEM_REPORT_PKGS %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(SYSTEM_REPORT_PKGS[!installed_packages])
}
lapply(SYSTEM_REPORT_PKGS, library, character.only = TRUE)


### Generate dashboard
rmarkdown::render(TEMPLATE_PATH, output_dir = VIS_DIR,
                  intermediates_dir = VIS_DIR, quiet = TRUE)


### Append result HTML file
template.html = readLines(file.path(VIS_DIR, "template.html"))
idx = which(template.html == "window.FlexDashboardComponents = [];")[1]
template.html = append(template.html, "L_PREFER_CANVAS = true;", after = idx)
writeLines(template.html, file.path(VIS_DIR, paste(OUTPUT_HTML_NAME, ".html", sep = "")))
file.remove(file.path(VIS_DIR, "template.html"))