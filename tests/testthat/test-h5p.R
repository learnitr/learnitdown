test_that("h5p() produces an iframe", {
  expect_equal(as.character(h5p(1, "https://www.example.com", toc = NULL)),
    "[]{#h5p_1}[![h5p](images/list-h5p.png)](h5p)\n<iframe src=\"https://www.example.com/wp-admin/admin-ajax.php?action=h5p_embed&id=1\" width=\"780\" height=\"500\" frameborder=\"0\" allowfullscreen=\"allowfullscreen\" class=\"h5p\"></iframe><script src=\"https://www.example.com/wp-content/plugins/h5p/h5p-php-library/js/h5p-resizer.js\" charset=\"UTF-8\"></script>")
})
