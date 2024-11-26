# Results of R CMD check

## Local

```r
devtools::check(remote = TRUE, manual = TRUE)
```

```
checking CRAN incoming feasibility ... [3s/18s] NOTE
  Maintainer: ‘Jean-Mathieu Potvin <jeanmathieupotvin@ununoctium.dev>’

  New submission
```

0 errors | 0 warnings | 1 note

## R win-builder

```r
devtools::check_win_devel()
```

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Jean-Mathieu Potvin <jeanmathieupotvin@ununoctium.dev>'

New submission
```

## R mac-builder

```
devtools::check_mac_release()
```

Status: OK

# Comments

* This is a new release.
* PDF manual contains some necessary non-ASCII characters such as `ç`, `ñ`,
  and `¡`. They did not generate error when building the manual.
