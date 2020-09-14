context("shiny")

describe("shiny", {
  it("produces an image/iframe for Shiny apps", {
    expect_equal(as.character(launch_shiny("https://www.shinyapps.io/app1",
      width = 780, height = 700, createimg = FALSE, alt1 = NULL)),
      "[<img onclick=\"launchApp('app1shiny', 'https://www.shinyapps.io/app1');\" src=\"images/shinyapps/shinyapp_default.png\" width=\"780\" height=\"700\" class=\"shiny-img\" id=\"imgapp1shiny\"/>\n<iframe width=\"780\" height=\"700\" frameborder=\"0\" scrolling=\"auto\" style=\"display:none\" class=\"shiny-app\" id=\"app1shiny\"></iframe>]{#app1 }")
  })
})
