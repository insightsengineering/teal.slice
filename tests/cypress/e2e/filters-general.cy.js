describe("General filter panel features", () => {
  beforeEach(() => {
    cy.visit("http://127.0.0.1:5555/");
    cy.waitForStabilityAndCatchError("body");
    cy.get(".cy-active-summary-table tbody tr").as("activeSummary");
  });

  it("should initiate filter panel with the right values", () => {
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "20/20");
  });
});
