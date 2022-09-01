library(shinytest2)

testthat::test_that("{shinytest2} recording: Initial app screen", {
  if (interactive()) {
    myapp = retentionsApp::run_app()
    app = AppDriver$new(
      myapp,
      name = "Initial loading screen",
      height = 980, width = 1619)
    navbar_items = app$get_text(".nav-item")
    squished_navbar_items = stringr::str_squish(navbar_items)
    expected_navbar_items = c("Tab 1", "Tab 2", "Tab 3", "Tab 4", "Help")
    testthat::expect_equal(squished_navbar_items, expected_navbar_items)
  }
})
