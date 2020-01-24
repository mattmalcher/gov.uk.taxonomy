context("test-get_taxon_df")

test_that("get taxon fetches pages using content api correctly", {

  root_df <- get_taxon_df(
    #path = "https://www.gov.uk/api/content/"
    path = "govuk_root.json"
    )

  world_taxon <- get_taxon_df(
    #path = "https://www.gov.uk/api/content/world/all"
    path = "govuk_world_all.json"
    )


  # check we are getting a tibble back
  expect_true(tibble::is_tibble(root_df))

  # check there are somewhere between 15 & 30 root taxons (more for world)
  expect_true(nrow(root_df) > 15 & nrow(root_df) < 30)
  expect_true(nrow(world_taxon) > 200 & nrow(world_taxon) < 300)

  # check for a warning AND an error:
  expect_warning( 
    object = {
      expect_error(
        object = get_taxon_df(
          path = "https://www.gov.uk/api/content/mattsmadeuptaxon"),
        regexp = "cannot open the connection")
    }, 
    regexp = "404")
  

  # check that key columns are all present
  expect_true(
    object = all(
      c("links", "api_url", "parent_taxons", "content_id") %in% names(root_df)
    )
  )

})
