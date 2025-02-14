# Comments for CRAN

## Resubmission

This is a resubmission asked by CRAN reviewer Uwe Ligges
(<ligges@statistik.tu-dortmund.de>). Many thanks to him for the fast review.

This version introduces the following changes (the official version was left
as is in file `DESCRIPTION`).

* Custom field `Roadmap` was removed from file `DESCRIPTION`.

  I would humbly like to point out that the current version of section 1.1.1
  (The `DESCRIPTION` file) of Writing R Extensions is confusing, as it suggests
  such fields can be added.

  > There is no restriction on the use of other fields not mentioned here (but
  > using other capitalizations of these field names would cause confusion).
  > Fields Note, Contact (for contacting the authors/developers) and MailingList
  > are in common use. Some repositories (including CRAN and R-forge) add their
  > own fields.

  This should be more specific.

## Previous Comments

* This is the first submission of version `0.1.0` of package `transltr`.
* There are no reverse dependency. I still performed required checks using
  package `revdepcheck`.

## Results of R CMD check

### Local

Below is relevant information extracted from `sessionInfo()`.

```
R version 4.4.2 (2024-10-31)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.1 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0

locale:
 [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8
 [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8
 [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C
[10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C

time zone: America/Toronto
tzcode source: system (glibc)
```

```r
devtools::check(remote = TRUE, manual = TRUE)
```

**Status: OK (0 errors, 0 warnings, 0 notes)**

### R win-builder

```r
devtools::check_win_devel()
```

**Status: OK (0 errors, 0 warnings, 0 notes)**

### R mac-builder

```
devtools::check_mac_release()
```

**Status: OK (0 errors, 0 warnings, 0 notes)**
