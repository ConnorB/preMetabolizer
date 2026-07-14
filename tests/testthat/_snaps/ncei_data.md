# ncei_data validates inputs

    Code
      ncei_data(dataset = 1, stations = "x", start_date = "2023-01-01", end_date = "2023-12-31")
    Condition
      Error in `ncei_data()`:
      ! `dataset` must be a single string.

---

    Code
      ncei_data(dataset = "daily-summaries", stations = character(), start_date = "2023-01-01",
      end_date = "2023-12-31")
    Condition
      Error in `ncei_data()`:
      ! `stations` must be a character vector.

---

    Code
      ncei_data(dataset = "daily-summaries", stations = "x", start_date = "not-a-date",
        end_date = "2023-12-31")
    Condition
      Error in `ncei_data()`:
      ! `start_date` must be a valid date in `YYYY-MM-DD` format.

---

    Code
      ncei_data(dataset = "daily-summaries", stations = "x", start_date = "2023-01-01",
        end_date = "2023-12-31", units = "imperial")
    Condition
      Error in `ncei_data()`:
      ! `units` must be one of "metric" or "standard", not "imperial".

# ncei_datasets validates inputs

    Code
      ncei_datasets(123)
    Condition
      Error in `ncei_datasets()`:
      ! `dataset` must be a single string.
