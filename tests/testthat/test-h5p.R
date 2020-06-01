context("h5p")

describe("h5p", {
  it("produces an iframe", {
    expect_equal(h5p(1, "https://www.example.com"),
      '<iframe src="https://www.example.com/wp-admin/admin-ajax.php?action=h5p_embed&id=1" width="780" height="500" frameborder="0" allowfullscreen="allowfullscreen" class="h5p"></iframe><script src="https://www.example.com/wp-content/plugins/h5p/h5p-php-library/js/h5p-resizer.js" charset="UTF-8"></script>')
  })
})
