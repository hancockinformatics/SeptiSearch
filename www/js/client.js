const handlers = {
  lazyLoadPackages: num => {
    // First argument is input name
    // Second argument is value to send
    Shiny.onInputChange('sessionInitialized', num);
  }
};

// We must use shiny:sessioninitialized, not DOM Content Loaded
$(document).on('shiny:sessioninitialized', () => {
  handlers.lazyLoadPackages(1);
});

window.onbeforeunload = () => {
  // First check to see whether Shiny has disconnected
  if (document.getElementById('shiny-disconnected-overlay') === null) {
    // If Shiny is NOT disconnected, confirm exit
    return 'If you navigate away, you will lose all of your intermediate results! Are you sure?';
  }
};
