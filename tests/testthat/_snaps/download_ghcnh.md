# download_ghcnh validates inputs

    Code
      download_ghcnh(station_id = c("a", "b"), years = 2024)
    Condition
      Error in `download_ghcnh()`:
      ! `station_id` must be a single non-empty string.

---

    Code
      download_ghcnh(station_id = "USW00000001", years = 2024.5)
    Condition
      Error in `download_ghcnh()`:
      ! `years` must be one or more whole numeric years.

# read_ghcnh validates inputs

    Code
      read_ghcnh(files = path, directory = dir)
    Condition
      Error in `read_ghcnh()`:
      ! Use only one of `files` and `directory`.

---

    Code
      read_ghcnh(files = "missing-ghcnh.csv")
    Condition
      Error in `read_ghcnh()`:
      ! All paths in `files` must exist.
      x Missing 'missing-ghcnh.csv'.

