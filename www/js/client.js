const handlers = {
  lazyLoadPackages: num => {
    // First argument is input name, second argument is value to send
    Shiny.onInputChange("sessionInitialized", num);
  },

  initGetStarted: () => {
    const getStartedButton = document.getElementById("get_started");
    const learnMoreButton = document.getElementById("learn_more");

    getStartedButton.innerHTML = "Get started";
    getStartedButton.title = "Click here to start using SeptiSearch";
    getStartedButton.classList.remove("disabled");

    learnMoreButton.classList.remove("btn-hidden");
  }
};

// Using shiny's sessioninitialized to lazy load packages
$(document).on("shiny:sessioninitialized", () => {
  handlers.lazyLoadPackages(1);
});
