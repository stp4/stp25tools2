test_that("reorder2 sorts by frequency", {
  x <- factor(rep(letters[1:3], c(1, 5, 3)))
  y <- reorder2(x)
  expect_equal(levels(y), c("b", "c", "a"))  # b=5, c=3, a=1
})

test_that("reorder2 lumps by count threshold", {
  x <- factor(rep(letters[1:4], c(1, 5, 3, 8)))
  y <- reorder2(x, threshold = 4)
  expect_true("Other" %in% levels(y))
  #expect_equal(sum(y == "Other"), 1)  # nur 'a' wird gelumpt
})

test_that("reorder2 lumps by proportion threshold", {
  x <- factor(rep(letters[1:4], c(1, 5, 3, 8)))
  y <- reorder2(x, threshold = 0.2)  # <20% lump
  expect_true("Other" %in% levels(y))
  expect_true(all(table(y)["Other"] >= 1))
})

test_that("reorder2 reorders with numeric auxiliary variable", {
  x <- factor(rep(letters[1:3], each = 2))
  X <- c(1, 2, 10, 20, -1, -2)  # means: a=1.5, b=15, c=-1.5
  y <- reorder2(x, X)
  expect_equal(levels(y), c("b", "a", "c"))
})

test_that("reorder2 reorders with character auxiliary vector", {
  x <- factor(rep(letters[1:4], 1:4))
  X <- c("d", "b", "a", "c")
  y <- reorder2(x, X, threshold.na.strings = "Other")
  expect_equal(levels(y)[1:3], c("d", "b", "a"))
 # expect_true("Other" %in% levels(y))
})

test_that("reorder2 collapses with named character vectors", {
  x <- factor(c("a", "b", "c", "d", "a", "d"))
  y <- reorder2(x,
                   group1 = c("a", "b"),
                   group2 = c("c", "d"))
  expect_true(all(levels(y) %in% c("group1", "group2")))
  expect_equal(sum(y == "group1"), 3)
  expect_equal(sum(y == "group2"), 3)
})

test_that("reorder2 moves levels to the end", {
  x <- factor(rep(letters[1:3], c(1, 5, 3)))
  y <- reorder2(x, last = "b")
  expect_equal(tail(levels(y), 1), "b")
})
