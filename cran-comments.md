# Comments for CRAN

* This is the first submission of version `0.1.0` of package `transltr`.
* There are no reverse dependency. I still performed required checks using
  package `revdepcheck`.

## Results of R CMD check

### Local

```r
devtools::check(remote = TRUE, manual = TRUE)
```

**0 errors | 0 warnings | 1 note**

```
❯ checking CRAN incoming feasibility ... [3s/17s] NOTE
  Maintainer: ‘Jean-Mathieu Potvin <jeanmathieupotvin@ununoctium.dev>’

  Unknown, possibly misspelled, fields in DESCRIPTION:
    ‘Roadmap’
```

### R win-builder

```r
devtools::check_win_devel()
```

**0 errors | 0 warnings | 1 note**

```
* checking CRAN incoming feasibility ... [56s] NOTE
Maintainer: 'Jean-Mathieu Potvin <jeanmathieupotvin@ununoctium.dev>'

Unknown, possibly misspelled, fields in DESCRIPTION:
  'Roadmap'
```

### R mac-builder

```
devtools::check_mac_release()
```

**0 errors | 0 warnings | 0 note**

**Status: OK**
