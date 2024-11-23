## R CMD check results

```r
devtools::check(remote = FALSE, manual = TRUE)
```

0 errors | 0 warnings | 0 note

```r
devtools::check(remote = TRUE, manual = TRUE)
```

0 errors | 0 warnings | 1 note

```
checking CRAN incoming feasibility ... [3s/18s] NOTE
  Maintainer: ‘Jean-Mathieu Potvin <jeanmathieupotvin@ununoctium.dev>’

  New submission
```

## Comments

* This is a new release.
* PDF manual contains some necessary non-ASCII characters such as `ç`, `ñ`,
  and `¡`. They did not generate error when building the manual.
