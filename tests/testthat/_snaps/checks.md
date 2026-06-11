# check_bool errors on non-logical, length != 1, or NA

    Code
      check_bool("yes")
    Condition
      Error:
      ! `"yes"` must be `TRUE` or `FALSE`.

---

    Code
      check_bool(c(TRUE, FALSE))
    Condition
      Error:
      ! `c(TRUE, FALSE)` must be `TRUE` or `FALSE`.

---

    Code
      check_bool(NA)
    Condition
      Error:
      ! `NA` must be `TRUE` or `FALSE`.

# check_string allow_null returns NULL when permitted

    Code
      check_string(NULL)
    Condition
      Error:
      ! `NULL` must be a single string.

# check_string allow_empty = FALSE rejects empty string

    Code
      check_string("", allow_empty = FALSE)
    Condition
      Error:
      ! `""` must be a non-empty string.

# check_string errors on character vector or NA

    Code
      check_string(c("a", "b"))
    Condition
      Error:
      ! `c("a", "b")` must be a single string.

---

    Code
      check_string(NA_character_)
    Condition
      Error:
      ! `NA_character_` must be a single string.

---

    Code
      check_string(1)
    Condition
      Error:
      ! `1` must be a single string.

# check_character allow_null returns NULL when permitted

    Code
      check_character(NULL)
    Condition
      Error:
      ! `NULL` must be a character vector.

# check_character allow_na = FALSE rejects NA elements

    Code
      check_character(c("a", NA), allow_na = FALSE)
    Condition
      Error:
      ! `c("a", NA)` must not contain missing values.

# check_character allow_empty = FALSE rejects empty elements

    Code
      check_character(c("a", ""), allow_empty = FALSE)
    Condition
      Error:
      ! `c("a", "")` must not contain empty strings.

# check_character rejects non-character and zero-length input

    Code
      check_character(1:3)
    Condition
      Error:
      ! `1:3` must be a character vector.

---

    Code
      check_character(character())
    Condition
      Error:
      ! `character()` must be a character vector.

# check_numeric allow_null returns NULL when permitted

    Code
      check_numeric(NULL)
    Condition
      Error:
      ! `NULL` must be numeric.

# check_numeric allow_na = FALSE rejects NA elements

    Code
      check_numeric(c(1, NA), allow_na = FALSE)
    Condition
      Error:
      ! `c(1, NA)` must not contain missing values.

# check_numeric default rejects infinite values

    Code
      check_numeric(c(1, Inf))
    Condition
      Error:
      ! `c(1, Inf)` must contain only finite values.

# check_numeric rejects non-numeric input

    Code
      check_numeric("1")
    Condition
      Error:
      ! `"1"` must be numeric.

# detect_datetime_col errors without a candidate column

    Code
      detect_datetime_col(df)
    Condition
      Error:
      ! `data` must contain a date-time column (<POSIXct>).
      i Alternatively, name the column with `datetime_col`.

# detect_datetime_col errors with multiple candidate columns

    Code
      detect_datetime_col(df)
    Condition
      Error:
      ! `data` contains multiple date-time columns: a and b.
      i Specify the column to use with `datetime_col`.

# detect_datetime_col errors on invalid explicit column

    Code
      detect_datetime_col(df, "missing")
    Condition
      Error:
      ! `data` must contain a missing column.

---

    Code
      detect_datetime_col(df, 1)
    Condition
      Error:
      ! `datetime_col` must be a single string.

