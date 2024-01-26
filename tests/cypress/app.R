library(teal)

data <- teal_data() |>
  within({
    set.seed(123)
    size <- 20
    data_frame <- data.frame(
      id = seq(size),
      numeric = round(rnorm(size, 25, 10)),
      logical = sample(c(TRUE, FALSE), size = size, replace = TRUE),
      factor = factor(sample(LETTERS[1:4], size = size, replace = TRUE)),
      character = sample(letters, size = size, replace = TRUE),
      datetime = sample(
        seq(as.POSIXct("2000/01/01"), as.POSIXct("2024/01/01"), by = "day"),
        size = size
      )
    )
    iris <- head(iris, 20)
    data_frame$date <- as.Date(data_frame$datetime)
  })
datanames(data) <- c("data_frame", "iris")
app <- teal::init(
  data = data,
  modules = modules(example_module()),
  filter = teal_slices(
    teal_slice("data_frame", "numeric"),
    teal_slice("data_frame", "logical"),
    teal_slice("data_frame", "factor"),
    teal_slice("data_frame", "character"),
    teal_slice("data_frame", "datetime"),
    teal_slice("data_frame", "date")
  )
)

options(shiny.port = 5555)
shinyApp(app$ui, app$server)
