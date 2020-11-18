/* Function to allow reset of plotly_click */
shinyjs.resetClick = function() {
  Shiny.onInputChange("plotly_click-A", "null");
};
