
## `keyword_processor` (Old) vs `KeywordProcessor` (New)

``` r
library(rflashtext)
library(microbenchmark)
```

``` r
set.seed(123)
CHARS <- c(letters, LETTERS, rep(" ", 5))
```

``` r
RunBenchmark <- function(no_sample = 1e1, word_size = 25, times = 100) {
  word_corpus <- vector("character", 2 * no_sample)
  for (i in seq_along(word_corpus)) 
    word_corpus[[i]] <- sample(CHARS, sample.int(word_size, 1), replace = TRUE) |> paste0(collapse = "")
  
  keys <- sample(word_corpus, no_sample) |> unique()
  words <- tolower(keys)
  sentence <- sample(word_corpus, no_sample, replace = TRUE) |> paste0(collapse = " ")
  
  microbenchmark(old_build = {
    old_processor <- keyword_processor$new(ignore_case = FALSE)
    old_processor$add_keys_words(keys, words)
    },
    new_build = new_processor <- KeywordProcessor$new(keys, words),
    times = times) |> suppressMessages() |> print()
  
  microbenchmark(old_find = old_processor$find_keys(sentence),
               new_find = new_processor$find_keys(sentence), 
               times = times) |> print()
  
  microbenchmark(old_replace = old_processor$replace_keys(sentence), 
               new_replace = new_processor$replace_keys(sentence),
               times = times) |> print()
  
  identical(old_processor$find_keys(sentence, span_info = FALSE) |> unlist(use.names = FALSE),
            new_processor$find_keys(sentence, span_info = FALSE) |> unlist(use.names = FALSE)) |> 
    sprintf(fmt = "Identical find_keys: %s") |> message()

  identical(old_processor$replace_keys(sentence), new_processor$replace_keys(sentence)) |> 
    sprintf(fmt = "Identical replace_keys: %s") |> message()
}
```

### No sample: 10

``` r
RunBenchmark(no_sample = 1e1, times = 20)
#> Unit: microseconds
#>       expr     min      lq     mean   median       uq       max neval
#>  old_build 422.543 439.563 980.1634 444.6420 500.8050 10821.171    20
#>  new_build  93.157  98.115 320.6116 111.4645 129.5835  4074.378    20
#> Unit: microseconds
#>      expr     min       lq       mean median       uq       max neval
#>  old_find 186.864 190.2230 1567.25935 193.17 209.0335 27147.243    20
#>  new_find  71.931  72.5475   85.00465  73.54  77.4245   256.116    20
#> Unit: microseconds
#>         expr     min       lq      mean   median       uq       max neval
#>  old_replace 190.117 191.7090 1510.3458 197.0315 209.6185 26130.047    20
#>  new_replace  70.898  71.8255   84.5227  72.3710  76.5695   256.067    20
#> Identical find_keys: TRUE
#> Identical replace_keys: TRUE
```

### No sample: 100

``` r
RunBenchmark(no_sample = 1e2, times = 20)
#> Unit: microseconds
#>       expr      min       lq      mean   median       uq      max neval
#>  old_build 2789.739 2834.180 3649.1775 2898.417 3863.396 7590.613    20
#>  new_build  340.097  364.705  454.9326  404.890  530.850  740.977    20
#> Unit: microseconds
#>      expr      min       lq      mean   median        uq      max neval
#>  old_find 1614.464 1773.254 2403.2353 1962.120 2857.4345 4530.876    20
#>  new_find  693.445  702.351  782.0984  734.646  787.9995 1360.639    20
#> Unit: microseconds
#>         expr      min        lq     mean   median       uq      max neval
#>  old_replace 1924.377 2111.6615 2818.572 2551.477 3024.005 5427.631    20
#>  new_replace  707.755  778.5125 1012.216  858.183 1076.282 2054.518    20
#> Identical find_keys: TRUE
#> Identical replace_keys: TRUE
```

### No sample: 1,000

``` r
RunBenchmark(no_sample = 1e3, times = 20)
#> Unit: milliseconds
#>       expr       min        lq      mean    median        uq      max neval
#>  old_build 38.442530 44.084830 48.324731 47.988672 50.299964 65.53939    20
#>  new_build  3.193777  3.390086  4.572598  4.324871  5.400149 10.32014    20
#> Unit: milliseconds
#>      expr       min        lq     mean   median       uq      max neval
#>  old_find 19.900758 21.877715 24.96172 23.39823 27.04512 42.34596    20
#>  new_find  7.401243  8.165731 10.25702 10.11235 12.06821 14.20217    20
#> Unit: milliseconds
#>         expr       min        lq     mean   median       uq      max neval
#>  old_replace 22.431640 30.317706 37.67097 34.35298 45.63918 58.29065    20
#>  new_replace  7.289222  9.328996 11.53827 10.42158 14.38882 20.89609    20
#> Identical find_keys: TRUE
#> Identical replace_keys: TRUE
```

### No sample: 10,000

``` r
RunBenchmark(no_sample = 1e4, times = 20)
#> Unit: milliseconds
#>       expr       min        lq      mean    median        uq       max neval
#>  old_build 481.28194 511.66368 615.56425 639.49008 698.65296 798.70352    20
#>  new_build  37.49975  43.57581  51.35543  48.33079  53.21909  98.13814    20
#> Unit: milliseconds
#>      expr       min       lq     mean   median       uq      max neval
#>  old_find 270.35999 306.0492 335.3225 318.7489 352.1663 542.4752    20
#>  new_find  99.79255 106.2760 112.5304 112.5971 116.5510 131.7441    20
#> Unit: milliseconds
#>         expr       min       lq     mean   median       uq      max neval
#>  old_replace 266.40394 277.0761 309.4580 283.3959 300.9507 579.0725    20
#>  new_replace  99.06444 101.4031 111.3732 105.4608 115.7890 155.3825    20
#> Identical find_keys: TRUE
#> Identical replace_keys: TRUE
```

### No sample: 100,000

``` r
RunBenchmark(no_sample = 1e5, times = 20)
#> Unit: milliseconds
#>       expr       min        lq      mean    median        uq       max neval
#>  old_build 6730.8515 7237.2647 7526.2443 7442.3321 7780.0394 8507.2325    20
#>  new_build  501.2218  553.1217  587.9295  580.2733  617.8672  753.3779    20
#> Unit: seconds
#>      expr      min       lq     mean   median       uq      max neval
#>  old_find 3.204985 3.516988 3.855967 3.840606 3.926642 6.196141    20
#>  new_find 1.154295 1.193105 1.245187 1.258444 1.303141 1.326085    20
#> Unit: seconds
#>         expr      min       lq     mean   median       uq      max neval
#>  old_replace 3.345822 3.454719 3.774512 3.642931 3.876683 5.885249    20
#>  new_replace 1.191829 1.232034 1.291575 1.299751 1.311840 1.563881    20
#> Identical find_keys: TRUE
#> Identical replace_keys: TRUE
```
