context("shiny")

describe("shiny", {
  it("produces an image/iframe for Shiny apps", {
    expect_equal(as.character(launch_shiny("https://www.shinyapps.io/app1",
      height = 700, createimg = FALSE, alt1 = NULL)),
      "<img onclick=\"launchApp('app1', 'https://www.shinyapps.io/app1');\" src=\"images/shinyapps/shinyapp_default.png\" width=\"780\" height=\"700\" class=\"shiny-img\" id=\"imgapp1\"/>\n<iframe width=\"780\" height=\"700\" frameborder=\"0\" scrolling=\"auto\" style=\"display:none\" class=\"shiny-app\" id=\"app1\"></iframe>")
    expect_equal(as.character(launch_shiny("https://www.shinyapps.io/app1/",
      width = 700, height = 400, createimg = FALSE, alt1 = "Some text...")),
      "<img onclick=\"launchApp('app1', 'https://www.shinyapps.io/app1/');\" src=\"images/shinyapps/shinyapp_default.png\" width=\"700\" height=\"400\" class=\"shiny-img\" id=\"imgapp1\"/>\n<iframe width=\"700\" height=\"400\" frameborder=\"0\" scrolling=\"auto\" style=\"display:none\" class=\"shiny-app\" id=\"app1\"></iframe>\nSome text...\n")
  })
})
