test_that("wydzielanie slow jest poprawne", {
  expect_equal(find_words("m..", c("test", "me", "right", "now", "man"), report = FALSE), "man")
})

test_that("wydzielanie slow jest poprawne", {
  expect_length(find_words("m..", c("test", "man", "men", "meow", "mix"), report = FALSE), 3)
})

test_that("rodzaj danych wyjsciowych jest poprawny", {
  expect_equal(is.vector(find_words("m..", c("test", "me", "right", "now", "man"), report = FALSE)), TRUE)
})

test_that("rodzaj danych wejsciowych jest poprawny", {
  expect_known_output(is.vector(find_words("m..", data.frame(c("test", "me", "right", "now", "man")), report = FALSE)),"It must be a vector!", fixed = TRUE)
})
