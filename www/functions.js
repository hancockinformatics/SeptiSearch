/* Function to reset the plotly_click variable in the viz tab */
shinyjs.resetClick = function() {
  Shiny.onInputChange("plotly_click-A", "null");
};
