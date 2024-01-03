describe("data.frame filters", () => {
  beforeEach(() => {
    cy.visit("http://127.0.0.1:5555/");
    cy.waitForStabilityAndCatchError("body");
    cy.get(".cy-active-summary-table tbody tr").as("activeSummary");
  });

  it("should filter proper values for numeric filters", () => {
    cy.get(".cy-filter-card").contains("numeric").as("numericCard");
    cy.get("@numericCard").click();
    cy.waitForStabilityAndCatchError("body");

    cy.get("@numericCard")
      .get(".cy-numeric-selection-inputs input:first")
      .clear()
      .type("10{enter}");
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "19/20");

    cy.get("@numericCard")
      .get(".cy-numeric-selection-inputs input:last")
      .clear()
      .type("20{enter}");
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "5/20");
  });

  it("should filter proper values for logical filters", () => {
    cy.get(".cy-filter-card").contains("logical").as("logicalCard");
    cy.get("@logicalCard").click();
    cy.waitForStabilityAndCatchError("body");

    cy.get("@logicalCard")
      .get(".cy-logical-selection-inputs .checkbox")
      .contains("TRUE")
      .as("trueCheckbox");
    cy.get("@logicalCard")
      .get(".cy-logical-selection-inputs .checkbox")
      .contains("FALSE")
      .as("falseCheckbox");

    cy.get("@trueCheckbox").click();
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "7/20");

    cy.get("@falseCheckbox").click();
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "0/20");

    cy.get("@trueCheckbox").click();
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "13/20");
  });

  it("should filter proper values for factor filters", () => {
    cy.get(".cy-filter-card").contains("factor").as("factorCard");
    cy.get("@factorCard").click();
    cy.waitForStabilityAndCatchError("body");

    cy.get("@factorCard")
      .get(".cy-factor-selection-inputs .checkbox")
      .contains("C")
      .click();
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "15/20");
  });

  it("should filter proper values for character filters", () => {
    cy.get(".cy-filter-card").contains("character").as("characterCard").click();
    cy.waitForStabilityAndCatchError("body");

    cy.get("@characterCard")
      .get(".cy-character-selection-inputs .dropdown-toggle")
      .as("characterInputs")
      .click();
    cy.waitForStabilityAndCatchError("body");

    cy.get("@characterInputs").get("li").contains("b").click();
    cy.waitForStabilityAndCatchError("body");
    cy.get("body").type("{esc}");
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "19/20");
  });

  it("should filter proper values for datetime filters", () => {
    cy.get(".cy-filter-card").contains("datetime").as("datetimeCard").click();
    cy.waitForStabilityAndCatchError("body");

    cy.get("@datetimeCard").get(".cy-datetime-from-input").click();
    cy.waitForStabilityAndCatchError("body");
    cy.get(
      '.air-datepicker-global-container .air-datepicker-cell[data-year="2001"][data-month="10"][data-date="29"]'
    ).click();
    cy.get("body").type("{esc}");
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "19/20");

    cy.get("@datetimeCard").get(".cy-datetime-to-input").click();
    cy.get(
      '.air-datepicker-global-container .air-datepicker-cell[data-year="2021"][data-month="5"][data-date="1"]'
    ).click();
    cy.get("body").type("{esc}");
    cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "18/20");
  });

  it("should filter proper values for date filters", () => {
    cy.get(".cy-filter-card")
      .contains(/^date$/)
      .as("dateCard")
      .click();
    cy.waitForStabilityAndCatchError("body");

    cy.get("@dateCard").get(".cy-date-inputs input:first").click();
    cy.waitForStabilityAndCatchError("body");

    cy.get(".datepicker-days table td").contains("16").click(),
      cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "19/20");

    cy.get("@dateCard").get(".cy-date-inputs input:last").click();
    cy.waitForStabilityAndCatchError("body");

    cy.get(".datepicker-days table td").contains("5").click(),
      cy.waitForStabilityAndCatchError("body");
    cy.get("@activeSummary")
      .should("contain", "data_frame")
      .should("contain", "18/20");
  });
});
