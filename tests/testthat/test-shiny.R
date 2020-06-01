context("shiny")

describe("shiny", {
  it("produces an image/iframe for Shiny apps", {
    expect_equal(launch_shiny("app1", url = "https://www.shinyapps.io/app1/", img = "images/app1.png", height = 700, alt = NULL),
      '<img onclick="launchApp(\'app1\', \'https://www.shinyapps.io/app1/\');" src="images/app1.png" width="780" height="700" class="shiny-img" id="imgapp1"/>
<iframe width="780" height="700" frameborder="0" scrolling="auto" style="display:none" class="shiny-app" id="app1"></iframe>')
  })
})
