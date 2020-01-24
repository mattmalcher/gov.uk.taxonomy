context("test-tree_lists")


test_that(
  "list up tree works", {
    data("acme", package = "data.tree")
    
    list_up_tree(acme, "name")

    testthat::expect_equal(
      acme$name_agg,
      c(
        "Acme Inc.",
        "Accounting",
        "New Software",
        "New Accounting Standards",
        "Research",
        "New Product Line",
        "New Labs",
        "IT",
        "Outsource",
        "Go agile",
        "Switch to R"
      )
    )

    testthat::expect_equal(
      acme$Accounting$name_agg,
      c(
        "Accounting",
        "New Software",
        "New Accounting Standards"
      )
    )
  }
)

test_that(
  "remove agg works", {
    
    data("acme", package = "data.tree")

    list_up_tree(acme, "name")
    
    acme$Accounting$RemoveChild(name = "New Software")

    remove_agg_if_in_child(acme, "name")

    # root node only has its own name - none of its children were deleted 
    testthat::expect_equal(
      acme$name_agg,
      "Acme Inc."
    )
    
    #New Accounting Standards is not present, because that node exists
    testthat::expect_equal(
    acme$Accounting$name_agg,
    c("Accounting", "New Software")
    )
    
    # "New Software" is present, despite the node being deleted :)
    testthat::expect_equal(
      acme$Accounting$name_agg,
      c("Accounting", "New Software")
    )
    
    testthat::expect_equal(
      acme$Accounting$`New Accounting Standards`$name_agg,
      c("New Accounting Standards")
    )
    
  }
)
