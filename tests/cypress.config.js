const { defineConfig } = require("cypress");

module.exports = defineConfig({
  video: true,
  viewportWidth: 1000,
  viewportHeight: 1200,
  e2e: {
    setupNodeEvents(on, config) {
      // implement node event listeners here
    },
  },
});
