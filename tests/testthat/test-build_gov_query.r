context("test-build_gov_query")

test_that("build gov query assembles URLS as expected", {

  # defaults
  expect_equal(
    object = build_gov_query(),
    expected = "https://www.gov.uk/api/search.json?q=&count=10&start=0&order=-popularity"
  )

  # override defaults - mix of numeric & char
  expect_equal(
    object = build_gov_query(q = "test", count = "5", start = 10, order = "popularity"),
    expected = "https://www.gov.uk/api/search.json?q=test&count=5&start=10&order=popularity"
  )

  # any org filter
  expect_equal(
    object = build_gov_query(org_any = c("hm-revenue-customs", "department-for-work-pensions")),
    expected = "https://www.gov.uk/api/search.json?q=&count=10&start=0&order=-popularity&filter_any_organisations=hm-revenue-customs&filter_any_organisations=department-for-work-pensions"
  )

  # all org filter
  expect_equal(
    object = build_gov_query(org_all = c("hm-revenue-customs", "department-for-work-pensions")),
    expected = "https://www.gov.uk/api/search.json?q=&count=10&start=0&order=-popularity&filter_all_organisations=hm-revenue-customs&filter_all_organisations=department-for-work-pensions"
  )
})


test_that(
  "queries are executed as expected", {
    query_url <-
      build_gov_query(
        count = "3",
        org_any = c("hm-revenue-customs")
      )

    query_result <- execute_gov_query(query_url)

    expect_true(is.data.frame(query_result))

    expect_true(nrow(query_result) == 3)

    expect_true(
      all(names(query_result) %in% c(
        "description",
        "format",
        "link",
        "organisations",
        "public_timestamp",
        "specialist_sectors",
        "title",
        "topic_content_ids",
        "expanded_topics",
        "organisation_content_ids",
        "expanded_organisations",
        "index",
        "_id",
        "elasticsearch_type",
        "document_type",
        "slug"
      ))
    )
  }
)

test_that(
  desc = "pagination gives same result as a direct query",
  code = {
    query_url <- build_gov_query()
    direct_query <- execute_gov_query(query_url = query_url)
    paged_query <- paginate_query(query_url = query_url, chunksize = 2, limit = 10)

    # check the paged and direct version of the query are the same
    # note - test is limited to non list columns -
    expect_true(
      dplyr::all_equal(
        dplyr::select_if(direct_query, ~ !is.list(.)),
        dplyr::select_if(paged_query, ~ !is.list(.))
    ))
  }
)

test_that(
  desc = "pagination limits work",
  code = {
    query_url <- build_gov_query()

    paged_query_3_10 <-
      paginate_query(query_url = query_url, chunksize = 3, limit = 10)

    paged_query_3_12 <-
      paginate_query(query_url = query_url, chunksize = 3, limit = 12)
    
    expect_true(nrow(paged_query_3_10) == 12)
    expect_true(nrow(paged_query_3_12) == 12)
  }
)
