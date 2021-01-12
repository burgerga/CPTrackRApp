
library(golem)

# Configure this test to fit your need
test_that(
  "app launches",{
    skip_on_ci()
    skip_if_not(interactive())
    x <- processx::process$new(
      "R", 
      c(
        "-e", 
        "pkgload::load_all(here::here());run_app()"
      )
    )
    Sys.sleep(1)
    expect_true(x$is_alive())
    x$kill()
  }
)








