# Comments for CRAN

## Resubmission

This is a resubmission asked by CRAN reviewer Benjamin Altmann
(<benjamin.altmann@wu.ac.at>). Many thanks to him for reviewing the
first release of the package.

This version introduces the following changes (the official version was left
as is in file `DESCRIPTION`).

* Examples for unexported functions were removed, along with calls to `:::`
  in other examples.

* All `\dontrun` and `\donttest` directives were removed from `man` pages.

* Examples were benchmarked (locally) to ensure they can be executed in less
  than 5 seconds (per individual `man` page).

* This file was updated accordingly with the latest results stemming from
  `R CMD check`.

## Results of R CMD check

### Local

```r
devtools::check(remote = TRUE, manual = TRUE)
```

```
checking CRAN incoming feasibility ... [3s/18s] NOTE
  Maintainer: ‘Jean-Mathieu Potvin <jeanmathieupotvin@ununoctium.dev>’

  New submission
```

0 errors | 0 warnings | 1 note

### R win-builder

```r
devtools::check_win_devel()
```

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Jean-Mathieu Potvin <jeanmathieupotvin@ununoctium.dev>'

New submission
```

### R mac-builder

```
devtools::check_mac_release()
```

Status: OK

### Comments

* This is a first release.
* PDF manual contains some necessary non-ASCII characters such as `ç`, `ñ`,
  and `¡`. They did not generate an error when building the manual.
