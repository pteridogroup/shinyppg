test_that("Loading data works", {
  skip("Skip long running test")
  testServer(
    load_data_server,
    {
      data <- head(load_data(data_source = "local"))
      # Check which cols_select columns are missing
      missing_cols <- setdiff(cols_select, colnames(data))
      if (length(missing_cols) > 0) {
        message("Missing columns: ", paste(missing_cols, collapse = ", "))
        message("Available columns: ", paste(colnames(data), collapse = ", "))
      }
      # Check that all cols_select columns are present in the data
      expect_true(
        all(cols_select %in% colnames(data))
      )
      expect_s3_class(data, "data.frame")
    }
  )
})
