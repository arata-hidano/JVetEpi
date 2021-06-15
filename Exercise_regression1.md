回帰モデルの選択
================
Arata Hidano
4/4/2021

## Import data データを取り込み

``` r
# Set directory
setwd("D://tem")
BH_dat = read.csv("BH_dat_adjusted.csv",header=T)
library(dplyr); library(skimr); library(lmtest)

# Check data データが存在するか確認
head(BH_dat)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["animalid"],"name":[1],"type":["int"],"align":["right"]},{"label":["status"],"name":[2],"type":["int"],"align":["right"]},{"label":["exotic"],"name":[3],"type":["chr"],"align":["left"]},{"label":["sexp"],"name":[4],"type":["chr"],"align":["left"]},{"label":["cage"],"name":[5],"type":["chr"],"align":["left"]},{"label":["fsummer2"],"name":[6],"type":["chr"],"align":["left"]},{"label":[".fgspr"],"name":[7],"type":["int"],"align":["right"]},{"label":["dzkid"],"name":[8],"type":["int"],"align":["right"]}],"data":[{"1":"1","2":"1","3":"Exotic","4":"Female","5":"3-5 years","6":"Fresh fern","7":"1","8":"18","_rn_":"1"},{"1":"2","2":"0","3":"Exotic","4":"Female","5":"3-5 years","6":"Fresh fern","7":"3","8":"18","_rn_":"2"},{"1":"3","2":"0","3":"Exotic","4":"Male","5":"3-5 years","6":"Fresh fern","7":"2","8":"18","_rn_":"3"},{"1":"4","2":"0","3":"Exotic","4":"Male","5":"3-5 years","6":"Fresh fern","7":"3","8":"18","_rn_":"4"},{"1":"5","2":"0","3":"Local","4":"Male","5":"3-5 years","6":"Fresh fern","7":"3","8":"18","_rn_":"5"},{"1":"6","2":"1","3":"Exotic","4":"Female","5":"6 years","6":"Fresh fern","7":"2","8":"18","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

## Missing data and Descriptive statistics 欠損値および各変数の特徴をつかむ

``` r
summary(BH_dat)
```

    ##     animalid         status          exotic              sexp          
    ##  Min.   :  1.0   Min.   :0.0000   Length:611         Length:611        
    ##  1st Qu.:141.0   1st Qu.:0.0000   Class :character   Class :character  
    ##  Median :276.0   Median :0.0000   Mode  :character   Mode  :character  
    ##  Mean   :275.2   Mean   :0.3333                                        
    ##  3rd Qu.:412.0   3rd Qu.:1.0000                                        
    ##  Max.   :542.0   Max.   :1.0000                                        
    ##  NA's   :2       NA's   :2                                             
    ##      cage             fsummer2             .fgspr          dzkid       
    ##  Length:611         Length:611         Min.   :0.000   Min.   : 2.000  
    ##  Class :character   Class :character   1st Qu.:0.000   1st Qu.: 5.000  
    ##  Mode  :character   Mode  :character   Median :3.000   Median :10.000  
    ##                                        Mean   :1.857   Mean   : 9.317  
    ##                                        3rd Qu.:3.000   3rd Qu.:12.000  
    ##                                        Max.   :3.000   Max.   :19.000  
    ##                                        NA's   :2       NA's   :2

``` r
skim(BH_dat)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | BH\_dat |
| Number of rows                                   | 611     |
| Number of columns                                | 8       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 4       |
| numeric                                          | 4       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| exotic         |         12 |           0.98 |   5 |   6 |     0 |         2 |          0 |
| sexp           |          2 |           1.00 |   4 |   6 |     0 |         2 |          0 |
| cage           |          2 |           1.00 |   7 |   9 |     0 |         4 |          0 |
| fsummer2       |          1 |           1.00 |   7 |  10 |     0 |         4 |          0 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |   mean |     sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:---------------|-----------:|---------------:|-------:|-------:|----:|----:|----:|----:|-----:|:------|
| animalid       |          2 |              1 | 275.20 | 154.59 |   1 | 141 | 276 | 412 |  542 | ▇▇▇▇▇ |
| status         |          2 |              1 |   0.33 |   0.47 |   0 |   0 |   0 |   1 |    1 | ▇▁▁▁▃ |
| .fgspr         |          2 |              1 |   1.86 |   1.40 |   0 |   0 |   3 |   3 |    3 | ▅▁▁▁▇ |
| dzkid          |          2 |              1 |   9.32 |   5.58 |   2 |   5 |  10 |  12 |   19 | ▇▆▇▂▅ |

``` r
    ### skim()がエラーを返す時は最新のdplyrとskimrパッケージがインストールされているか確認。
    ### ここでn_missingに注目。全ての変数に欠損データがあることを確認しておこう
```

``` r
# check the level of categorical variables カテゴリカル変数の取る値を確認

  # 牛の種類
  table(BH_dat$exotic)
```

    ## 
    ## Exotic  Local 
    ##    205    394

``` r
  # 性別
  table(BH_dat$sexp)
```

    ## 
    ## Female   Male 
    ##    479    130

``` r
  # 年齢
  table(BH_dat$cage)
```

    ## 
    ## 3-5 years   6 years 7-8 years   9+years 
    ##       238       105       136       130

``` r
      # 年齢は連続変数でなく既にカテゴリ化されている。ただし現在はデータが文字列型なので因子型に変換。
      # 参照とする値を決める。
       BH_dat$cage = factor(BH_dat$cage ) 
       str(BH_dat$cage) # '3-5 years'が参照値となっていることに留意
```

    ##  Factor w/ 4 levels "3-5 years","6 years",..: 1 1 1 1 1 2 2 3 1 3 ...

``` r
  # 夏場にワラビを敷き草として使うか
  table(BH_dat$fsummer2)
```

    ## 
    ##   Dry fern Fresh fern    No fern Not housed 
    ##         72        204        213        121

``` r
    # 因子型に変換。今回は参照値を"No fern"に変更する
    BH_dat$fsummer2 = factor(BH_dat$fsummer2,levels=c("No fern","Dry fern","Fresh fern","Not housed"))
    str(BH_dat$fsummer2) #参照値が"No fern"に変更されたことを確認
```

    ##  Factor w/ 4 levels "No fern","Dry fern",..: 3 3 3 3 3 3 1 1 1 3 ...

``` r
# 連続変数の取る値を確認
    table(BH_dat$.fgspr)
```

    ## 
    ##   0   1   2   3 
    ## 208  24  24 353

## Associations between explanatory variables 説明変数同士の関係を把握する

``` r
# 説明変数同士の関係を把握する
  #  exoticと他の変数
    table(BH_dat$exotic,BH_dat$sexp)
```

    ##         
    ##          Female Male
    ##   Exotic    170   35
    ##   Local     307   87

``` r
    chisq.test(BH_dat$exotic,BH_dat$sexp, correct=FALSE)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  BH_dat$exotic and BH_dat$sexp
    ## X-squared = 2.0851, df = 1, p-value = 0.1487

``` r
    table(BH_dat$exotic,BH_dat$cage)
```

    ##         
    ##          3-5 years 6 years 7-8 years 9+years
    ##   Exotic        76      30        49      50
    ##   Local        162      75        86      71

``` r
    chisq.test(BH_dat$exotic,BH_dat$cage, correct=FALSE)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  BH_dat$exotic and BH_dat$cage
    ## X-squared = 5.0112, df = 3, p-value = 0.171

``` r
    table(BH_dat$exotic,BH_dat$fsummer2)
```

    ##         
    ##          No fern Dry fern Fresh fern Not housed
    ##   Exotic      88       17         76         24
    ##   Local      121       55        123         94

``` r
    chisq.test(BH_dat$exotic,BH_dat$fsummer2, correct=FALSE) #p-value = 0.0001132
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  BH_dat$exotic and BH_dat$fsummer2
    ## X-squared = 20.849, df = 3, p-value = 0.0001132

``` r
    table(BH_dat$exotic,BH_dat$.fgspr)
```

    ##         
    ##            0   1   2   3
    ##   Exotic 111   5   8  80
    ##   Local   94  18  16 266

``` r
    chisq.test(BH_dat$exotic,BH_dat$.fgspr, correct=FALSE) #p-value = 2.868e-12
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  BH_dat$exotic and BH_dat$.fgspr
    ## X-squared = 56.776, df = 3, p-value = 2.868e-12

``` r
  # gmodels packageのCrossTable functionが便利
    library(gmodels)
  # sexpと他の変数
    CrossTable(BH_dat$sexp, BH_dat$cage,chisq = T) # p =  1.286262e-09 
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  609 
    ## 
    ##  
    ##              | BH_dat$cage 
    ##  BH_dat$sexp | 3-5 years |   6 years | 7-8 years |   9+years | Row Total | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##       Female |       157 |        87 |       128 |       107 |       479 | 
    ##              |     4.871 |     0.236 |     4.135 |     0.221 |           | 
    ##              |     0.328 |     0.182 |     0.267 |     0.223 |     0.787 | 
    ##              |     0.660 |     0.829 |     0.941 |     0.823 |           | 
    ##              |     0.258 |     0.143 |     0.210 |     0.176 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##         Male |        81 |        18 |         8 |        23 |       130 | 
    ##              |    17.946 |     0.869 |    15.236 |     0.813 |           | 
    ##              |     0.623 |     0.138 |     0.062 |     0.177 |     0.213 | 
    ##              |     0.340 |     0.171 |     0.059 |     0.177 |           | 
    ##              |     0.133 |     0.030 |     0.013 |     0.038 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ## Column Total |       238 |       105 |       136 |       130 |       609 | 
    ##              |     0.391 |     0.172 |     0.223 |     0.213 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ##  
    ## Statistics for All Table Factors
    ## 
    ## 
    ## Pearson's Chi-squared test 
    ## ------------------------------------------------------------
    ## Chi^2 =  44.32674     d.f. =  3     p =  1.286262e-09 
    ## 
    ## 
    ## 

``` r
    CrossTable(BH_dat$sexp, BH_dat$fsummer2,chisq = T) #  p =  0.2327552
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  608 
    ## 
    ##  
    ##              | BH_dat$fsummer2 
    ##  BH_dat$sexp |    No fern |   Dry fern | Fresh fern | Not housed |  Row Total | 
    ## -------------|------------|------------|------------|------------|------------|
    ##       Female |        163 |         51 |        167 |         97 |        478 | 
    ##              |      0.050 |      0.555 |      0.273 |      0.037 |            | 
    ##              |      0.341 |      0.107 |      0.349 |      0.203 |      0.786 | 
    ##              |      0.773 |      0.708 |      0.819 |      0.802 |            | 
    ##              |      0.268 |      0.084 |      0.275 |      0.160 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ##         Male |         48 |         21 |         37 |         24 |        130 | 
    ##              |      0.184 |      2.041 |      1.004 |      0.135 |            | 
    ##              |      0.369 |      0.162 |      0.285 |      0.185 |      0.214 | 
    ##              |      0.227 |      0.292 |      0.181 |      0.198 |            | 
    ##              |      0.079 |      0.035 |      0.061 |      0.039 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ## Column Total |        211 |         72 |        204 |        121 |        608 | 
    ##              |      0.347 |      0.118 |      0.336 |      0.199 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ## 
    ##  
    ## Statistics for All Table Factors
    ## 
    ## 
    ## Pearson's Chi-squared test 
    ## ------------------------------------------------------------
    ## Chi^2 =  4.280188     d.f. =  3     p =  0.2327552 
    ## 
    ## 
    ## 

``` r
    CrossTable(BH_dat$sexp, BH_dat$.fgspr,chisq = T) #  p =  0.6588869
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  608 
    ## 
    ##  
    ##              | BH_dat$.fgspr 
    ##  BH_dat$sexp |         0 |         1 |         2 |         3 | Row Total | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##       Female |       160 |        20 |        17 |       281 |       478 | 
    ##              |     0.046 |     0.068 |     0.185 |     0.044 |           | 
    ##              |     0.335 |     0.042 |     0.036 |     0.588 |     0.786 | 
    ##              |     0.773 |     0.833 |     0.708 |     0.796 |           | 
    ##              |     0.263 |     0.033 |     0.028 |     0.462 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##         Male |        47 |         4 |         7 |        72 |       130 | 
    ##              |     0.170 |     0.250 |     0.680 |     0.160 |           | 
    ##              |     0.362 |     0.031 |     0.054 |     0.554 |     0.214 | 
    ##              |     0.227 |     0.167 |     0.292 |     0.204 |           | 
    ##              |     0.077 |     0.007 |     0.012 |     0.118 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ## Column Total |       207 |        24 |        24 |       353 |       608 | 
    ##              |     0.340 |     0.039 |     0.039 |     0.581 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ##  
    ## Statistics for All Table Factors
    ## 
    ## 
    ## Pearson's Chi-squared test 
    ## ------------------------------------------------------------
    ## Chi^2 =  1.602219     d.f. =  3     p =  0.6588869 
    ## 
    ## 
    ## 

``` r
  # cageと他の変数
    CrossTable(BH_dat$cage, BH_dat$fsummer2,chisq = T) # p =  0.003441523 
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  608 
    ## 
    ##  
    ##              | BH_dat$fsummer2 
    ##  BH_dat$cage |    No fern |   Dry fern | Fresh fern | Not housed |  Row Total | 
    ## -------------|------------|------------|------------|------------|------------|
    ##    3-5 years |         81 |         41 |         62 |         53 |        237 | 
    ##              |      0.019 |      5.961 |      3.860 |      0.722 |            | 
    ##              |      0.342 |      0.173 |      0.262 |      0.224 |      0.390 | 
    ##              |      0.384 |      0.569 |      0.304 |      0.438 |            | 
    ##              |      0.133 |      0.067 |      0.102 |      0.087 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ##      6 years |         37 |         12 |         44 |         12 |        105 | 
    ##              |      0.009 |      0.015 |      2.183 |      3.788 |            | 
    ##              |      0.352 |      0.114 |      0.419 |      0.114 |      0.173 | 
    ##              |      0.175 |      0.167 |      0.216 |      0.099 |            | 
    ##              |      0.061 |      0.020 |      0.072 |      0.020 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ##    7-8 years |         52 |         10 |         45 |         29 |        136 | 
    ##              |      0.489 |      2.314 |      0.009 |      0.138 |            | 
    ##              |      0.382 |      0.074 |      0.331 |      0.213 |      0.224 | 
    ##              |      0.246 |      0.139 |      0.221 |      0.240 |            | 
    ##              |      0.086 |      0.016 |      0.074 |      0.048 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ##      9+years |         41 |          9 |         53 |         27 |        130 | 
    ##              |      0.375 |      2.656 |      2.018 |      0.049 |            | 
    ##              |      0.315 |      0.069 |      0.408 |      0.208 |      0.214 | 
    ##              |      0.194 |      0.125 |      0.260 |      0.223 |            | 
    ##              |      0.067 |      0.015 |      0.087 |      0.044 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ## Column Total |        211 |         72 |        204 |        121 |        608 | 
    ##              |      0.347 |      0.118 |      0.336 |      0.199 |            | 
    ## -------------|------------|------------|------------|------------|------------|
    ## 
    ##  
    ## Statistics for All Table Factors
    ## 
    ## 
    ## Pearson's Chi-squared test 
    ## ------------------------------------------------------------
    ## Chi^2 =  24.60431     d.f. =  9     p =  0.003441523 
    ## 
    ## 
    ## 

``` r
          # 6 yearsはNot housedでない(屋内に入れられている)確率が高い
    
    CrossTable(BH_dat$cage, BH_dat$.fgspr,chisq = T) #  p =  0.4856489 
```

    ## Warning in chisq.test(t, correct = FALSE, ...): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  608 
    ## 
    ##  
    ##              | BH_dat$.fgspr 
    ##  BH_dat$cage |         0 |         1 |         2 |         3 | Row Total | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##    3-5 years |        87 |         7 |        11 |       133 |       238 | 
    ##              |     0.440 |     0.610 |     0.274 |     0.194 |           | 
    ##              |     0.366 |     0.029 |     0.046 |     0.559 |     0.391 | 
    ##              |     0.420 |     0.292 |     0.458 |     0.377 |           | 
    ##              |     0.143 |     0.012 |     0.018 |     0.219 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##      6 years |        41 |         5 |         5 |        54 |       105 | 
    ##              |     0.771 |     0.176 |     0.176 |     0.795 |           | 
    ##              |     0.390 |     0.048 |     0.048 |     0.514 |     0.173 | 
    ##              |     0.198 |     0.208 |     0.208 |     0.153 |           | 
    ##              |     0.067 |     0.008 |     0.008 |     0.089 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##    7-8 years |        38 |         8 |         5 |        85 |       136 | 
    ##              |     1.489 |     1.290 |     0.025 |     0.462 |           | 
    ##              |     0.279 |     0.059 |     0.037 |     0.625 |     0.224 | 
    ##              |     0.184 |     0.333 |     0.208 |     0.241 |           | 
    ##              |     0.062 |     0.013 |     0.008 |     0.140 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ##      9+years |        41 |         4 |         3 |        81 |       129 | 
    ##              |     0.194 |     0.234 |     0.860 |     0.497 |           | 
    ##              |     0.318 |     0.031 |     0.023 |     0.628 |     0.212 | 
    ##              |     0.198 |     0.167 |     0.125 |     0.229 |           | 
    ##              |     0.067 |     0.007 |     0.005 |     0.133 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ## Column Total |       207 |        24 |        24 |       353 |       608 | 
    ##              |     0.340 |     0.039 |     0.039 |     0.581 |           | 
    ## -------------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ##  
    ## Statistics for All Table Factors
    ## 
    ## 
    ## Pearson's Chi-squared test 
    ## ------------------------------------------------------------
    ## Chi^2 =  8.489669     d.f. =  9     p =  0.4856489 
    ## 
    ## 
    ## 

``` r
  # fsummer2と.fgspr
    
    CrossTable(BH_dat$fsummer2, BH_dat$.fgspr,chisq = T) #  p =  5.674029e-15
```

    ## Warning in chisq.test(t, correct = FALSE, ...): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## | Chi-square contribution |
    ## |           N / Row Total |
    ## |           N / Col Total |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  608 
    ## 
    ##  
    ##                 | BH_dat$.fgspr 
    ## BH_dat$fsummer2 |         0 |         1 |         2 |         3 | Row Total | 
    ## ----------------|-----------|-----------|-----------|-----------|-----------|
    ##         No fern |       114 |         2 |         7 |        88 |       211 | 
    ##                 |    24.224 |     4.809 |     0.212 |     9.551 |           | 
    ##                 |     0.540 |     0.009 |     0.033 |     0.417 |     0.347 | 
    ##                 |     0.548 |     0.083 |     0.292 |     0.250 |           | 
    ##                 |     0.188 |     0.003 |     0.012 |     0.145 |           | 
    ## ----------------|-----------|-----------|-----------|-----------|-----------|
    ##        Dry fern |        21 |         4 |         6 |        41 |        72 | 
    ##                 |     0.535 |     0.472 |     3.509 |     0.011 |           | 
    ##                 |     0.292 |     0.056 |     0.083 |     0.569 |     0.118 | 
    ##                 |     0.101 |     0.167 |     0.250 |     0.116 |           | 
    ##                 |     0.035 |     0.007 |     0.010 |     0.067 |           | 
    ## ----------------|-----------|-----------|-----------|-----------|-----------|
    ##      Fresh fern |        63 |        12 |         9 |       120 |       204 | 
    ##                 |     0.661 |     1.935 |     0.111 |     0.030 |           | 
    ##                 |     0.309 |     0.059 |     0.044 |     0.588 |     0.336 | 
    ##                 |     0.303 |     0.500 |     0.375 |     0.341 |           | 
    ##                 |     0.104 |     0.020 |     0.015 |     0.197 |           | 
    ## ----------------|-----------|-----------|-----------|-----------|-----------|
    ##      Not housed |        10 |         6 |         2 |       103 |       121 | 
    ##                 |    23.811 |     0.314 |     1.614 |    15.496 |           | 
    ##                 |     0.083 |     0.050 |     0.017 |     0.851 |     0.199 | 
    ##                 |     0.048 |     0.250 |     0.083 |     0.293 |           | 
    ##                 |     0.016 |     0.010 |     0.003 |     0.169 |           | 
    ## ----------------|-----------|-----------|-----------|-----------|-----------|
    ##    Column Total |       208 |        24 |        24 |       352 |       608 | 
    ##                 |     0.342 |     0.039 |     0.039 |     0.579 |           | 
    ## ----------------|-----------|-----------|-----------|-----------|-----------|
    ## 
    ##  
    ## Statistics for All Table Factors
    ## 
    ## 
    ## Pearson's Chi-squared test 
    ## ------------------------------------------------------------
    ## Chi^2 =  87.2943     d.f. =  9     p =  5.674029e-15 
    ## 
    ## 
    ## 

``` r
          # Not housedは放牧される確率及び期間が長い
```

## Associations between outcome and each explanatory variable 目的変数と説明変数の関係性を把握する

``` r
# Univariable analysis 目的変数と説明変数の関係性を把握する

  # 1 .fgspr この変数を連続変数として扱うべきかカテゴリカル変数として扱うべきか決定する
    # まずカテゴリカル変数として係数を推定
  model1 = glm(status~factor(.fgspr),data=BH_dat,family="binomial")
  summary(model1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ factor(.fgspr), family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3232  -0.9678  -0.7090   1.4026   1.7344  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -1.2528     0.1672  -7.493 6.71e-14 ***
    ## factor(.fgspr)1   1.0857     0.4425   2.454 0.014138 *  
    ## factor(.fgspr)2   1.5892     0.4465   3.559 0.000372 ***
    ## factor(.fgspr)3   0.7374     0.2001   3.685 0.000229 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 751.68  on 604  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 759.68
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
        # 月数と係数には単調な増減関係は認められない
          # 参照値である(.fgspr)0と比較して(.fgspr)3の係数は0.43で(.fgspr)2の1.02より減少している
          # さらにp値を確認しても0.02/0.03と、目的変数と強い関連があることが示唆される
  
      # この時点で.fgsprを連続変数として使ってはいけないことが明らかであるが、連続変数として扱った場合の係数を見てみる
      model1_1 = glm(status~.fgspr,data=BH_dat,family="binomial")
      summary(model1_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ .fgspr, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9925  -0.9925  -0.7494   1.3743   1.6776  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.1263     0.1562  -7.212 5.53e-13 ***
    ## .fgspr        0.2248     0.0643   3.497 0.000471 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 761.76  on 606  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 765.76
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
            # 月数が1増えるごとに係数は0.13増えるという推定結果
            # カテゴリカル変数として扱った場合に確認された(.fgspr)3の係数が(.fgspr)2より低い、という結果と矛盾している
            # よって.fgsprは連続変数として扱ってはいけない
  
  # 2 exotic
  model2 = glm(status~exotic,data=BH_dat,family="binomial")
  summary(model2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ exotic, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9286  -0.9286  -0.8322   1.4485   1.5676  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -0.8824     0.1535  -5.748 9.01e-09 ***
    ## exoticLocal   0.2645     0.1863   1.419    0.156    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 760.21  on 598  degrees of freedom
    ## Residual deviance: 758.17  on 597  degrees of freedom
    ##   (12 observations deleted due to missingness)
    ## AIC: 762.17
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
        # ここで注目すべきは係数とP値の両方
        # 係数は0.26で、この変数が目的変数の値に与える影響は小さく、かつ
        # P値は0.156で、「推定された係数が0ではない」という帰無仮説を却下できない、ことから
        # この変数の係数は0である可能性がある、つまり目的変数に影響を与えない、可能性が高い。
        # 今回はスクリーニングの基準をp=0.15としたので、この変数は多変量モデルでは考慮しない
  
  # 3 sexp
  model3 = glm(status~sexp,data=BH_dat,family="binomial")
  summary(model3)
```

    ## 
    ## Call:
    ## glm(formula = status ~ sexp, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9329  -0.9329  -0.9329   1.4435   1.6378  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.60667    0.09562  -6.345 2.23e-10 ***
    ## sexpMale    -0.43131    0.22129  -1.949   0.0513 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 775.27  on 608  degrees of freedom
    ## Residual deviance: 771.32  on 607  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 775.32
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
        # 同様に結果を解釈すると、オスはメスよりも係数が0.43低い
        # P値は0.051で帰無仮説を棄却するエビデンスがある
        # つまり性別は目的変数に影響を与える可能性が現段階ではある
  
  # 4 cage
  model4 = glm(status~cage,data=BH_dat,family="binomial")
  summary(model4)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1384  -0.8842  -0.8060   1.2169   1.6016  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -0.95784    0.14479  -6.615 3.71e-11 ***
    ## cage6 years    0.08776    0.25833   0.340 0.734067    
    ## cage7-8 years  0.22024    0.23359   0.943 0.345748    
    ## cage9+years    0.86547    0.22760   3.803 0.000143 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 775.27  on 608  degrees of freedom
    ## Residual deviance: 759.62  on 605  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 767.62
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
        # この変数は4つの値を取り、cage6 yearsの係数は0.08と0に近いうえ、P値も0.73と帰無仮説は却下できない
        # しかしながらcage9+の係数は0.86と0と比較してかなり大きいうえ、P値も小さい
        # このように変数の取る値によって目的変数との関係性が明確でない場合は次のアプローチをとる
  
  model_null = glm(status~NULL,data=BH_dat,family="binomial")
        # NULLモデル、つまり説明変数が全く入っていないモデル（切片の推定だけされる）、を作る
        # cageを説明変数として加えたmodel4はmodel_nullに比べてどれだけ目的変数の推定をより良くできているか？調べる
        # model_nullとmodel4を尤度比検定して比べる
        # lrtest function(lmtestパッケージ)を使う
    lrtest(model4,model_null)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"-379.8084","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"1","2":"-387.6371","3":"-3","4":"15.65752","5":"0.001332849","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
        # 帰無仮説は「比較した2つのモデルのうち、より複雑なモデルは複雑でないモデルと比較してデータへのあてはまりが同じかそれ以下」
        # P値が非常に小さい(p=0.001333)ということは、帰無仮説が却下される、つまり複雑なモデルのほうがデータへの当てはまりが良い
        # つまりcageを入れたモデルの方が良いモデル、ということになる
        # よってcageは目的変数と関連があり（因果関係とは限らない）多変量モデルに入れる候補となる
    
  # 5 fsummer2
  model5 = glm(status~fsummer2,data=BH_dat,family="binomial")
  summary(model5)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3232  -0.9412  -0.6663   1.3908   1.7968  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.3922     0.1724  -8.075 6.75e-16 ***
    ## fsummer2Dry fern     1.7287     0.2947   5.865 4.48e-09 ***
    ## fsummer2Fresh fern   0.8075     0.2260   3.574 0.000352 ***
    ## fsummer2Not housed   0.9034     0.2546   3.549 0.000387 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 735.23  on 604  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 743.23
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
        # cageと同様に尤度比検定してfsummer2と目的変数との関連を調べる
  
  lrtest(model5,model_null)
```

    ## Error in lrtest.default(model5, model_null): models were not all fitted to the same size of dataset

``` r
        #　ここで"models were not all fitted to the same size of dataset"というエラーが出る。
        #  尤度比検定を行うために2つのルールを満たす必要がある。
            #  ルール1: 尤度比検定を2つのモデルを比較するときに用いるためには、片方のモデルが他方の入れ子構造(nested)となっている必要がある
              #  例えるならば青リンゴと赤リンゴはリンゴという果物(モデル)内で入れ子になっているので比較できる
              #  一方、赤リンゴとオレンジは果物の種類が異なるので比較できない
              #  ２つのモデルが入れ子であるという意味は、モデルBに含まれるパラメーターは全てモデルAに含まれているということである
              #  つまりモデルAとB両方が、もう片方のモデルにないパラメーターを含む場合尤度比検定は行えない
            #  ルール2: さらに尤度を計算する際、一つ一つの観察データーを(データでいう各行)使うためモデルAに含まれる行とモデルBに含まれる行が異なる場合も尤度比検定は行えない
              #  今回model5とmodel_nullに含まれるデータは異なることに気が付いただろうか？
              #  上で欠損データを確認した際に、fsummer2変数には欠損データがあった。
              #　欠損データを含む変数をモデルに投入した場合、欠損値を含む行はパラメータ推定には使えないので自動的に除去される
              #　どれだけデータがモデルに含まれているか以下のように確認できる
              model_null
```

    ## 
    ## Call:  glm(formula = status ~ NULL, family = "binomial", data = BH_dat)
    ## 
    ## Coefficients:
    ## (Intercept)  
    ##     -0.6931  
    ## 
    ## Degrees of Freedom: 608 Total (i.e. Null);  608 Residual
    ##   (2 observations deleted due to missingness)
    ## Null Deviance:       775.3 
    ## Residual Deviance: 775.3     AIC: 777.3

``` r
                  # Degrees of Freedom: 608 Total NULLモデルのパラメータ数は1なのでデータの数は608+1=609
              model5
```

    ## 
    ## Call:  glm(formula = status ~ fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Coefficients:
    ##        (Intercept)    fsummer2Dry fern  fsummer2Fresh fern  fsummer2Not housed  
    ##            -1.3922              1.7287              0.8075              0.9034  
    ## 
    ## Degrees of Freedom: 607 Total (i.e. Null);  604 Residual
    ##   (3 observations deleted due to missingness)
    ## Null Deviance:       774.5 
    ## Residual Deviance: 735.2     AIC: 743.2

``` r
                  # Degrees of Freedom: 607 Total 同様にデータの数は607+1=608
                  # model5は含まれるデータの数が一つ少ないことが分かる。これによって尤度比検定が行えない
        # よって比較する2つのモデルが同じデータを含むようにすればよい
        # fsummer2に欠損値を持つ行を除いてmodel_nullを作り直す。model_null2と命名する
        model_null2 = glm(status~NULL,data=subset(BH_dat,!is.na(BH_dat$fsummer2)),family="binomial")
                # subset関数を使いfsummer2に欠損値を持つデータを除外
        lrtest(model5,model_null2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"-367.6126","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"1","2":"-387.2313","3":"-3","4":"39.23729","5":"1.545944e-08","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
          # 無事尤度比検定を行えた
          # ここでp値は1.546e-08と、fsummer2と目的変数の関連は強い
         
    # 最後に.fgsprのp値を求めておく
        model_null3 = glm(status~NULL,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
        lrtest(model1,model_null3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"-375.8413","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"1","2":"-387.2313","3":"-3","4":"22.77983","5":"4.488297e-05","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
          # p = 4.488e-05
        
# 単変量解析の段階で目的変数との関連の強さはp値を基準とすると
  # fsummer2 < .fgspr < cage < sexp < exotic
  # これらのうちexoticは多変量モデルに投入する候補とならない
```

## Construct multivariable models 多変量モデル

``` r
# Multivariable analysis 多変量モデル

# 多変量モデルを作る手順はいくつかパターンがある
# 代表的なものはforward-selection, backward-selection, backward-forward-selection
# 以下それぞれの手法を見ていく

# Method 1: Forward selection
# 始めにNULLモデル、つまり切片だけのモデルからスタートし各変数を重要度の順に従ってモデルに加えていく
# 重要度は研究目的に沿って自分で決める。多く使われる指標は単変量解析時のp値の小さな順など

multi_model_null = glm(status~NULL
                          ,data=BH_dat,family="binomial")

# Forward selection process
# 最もp値が低かったfsummer2から順に入れていく

# 変数一つ目
  # fsummer2
  
  multi_model_F1 = glm(status~fsummer2
                            ,data=BH_dat,family="binomial")
    # このモデルは上のmodel5と同じモデルなので係数等確認する必要はない

# 変数二つ目  
  #　続いて各変数を一つづつ加え最もモデルのあてはまりに大きな影響を与える変数を決定する
  # 交絡作用を評価するために変数を加えた際の係数の変化に注意する
      # cage
          multi_model_F2_1 = glm(status~fsummer2 + cage
                                    ,data=BH_dat,family="binomial")
          lrtest(multi_model_F1,multi_model_F2_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"-367.6126","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-357.7075","3":"3","4":"19.81028","5":"0.0001858234","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
            # p =0.0001858
          summary(multi_model_F2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6998  -0.8877  -0.6139   1.1224   1.9645  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7728     0.2206  -8.035 9.33e-16 ***
    ## fsummer2Dry fern     1.9036     0.3047   6.247 4.17e-10 ***
    ## fsummer2Fresh fern   0.7645     0.2301   3.322 0.000893 ***
    ## fsummer2Not housed   0.9190     0.2599   3.536 0.000406 ***
    ## cage6 years          0.1996     0.2724   0.733 0.463856    
    ## cage7-8 years        0.4023     0.2462   1.635 0.102140    
    ## cage9+years          1.0449     0.2419   4.320 1.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 715.41  on 601  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 729.41
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
          summary(multi_model_F1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3232  -0.9412  -0.6663   1.3908   1.7968  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.3922     0.1724  -8.075 6.75e-16 ***
    ## fsummer2Dry fern     1.7287     0.2947   5.865 4.48e-09 ***
    ## fsummer2Fresh fern   0.8075     0.2260   3.574 0.000352 ***
    ## fsummer2Not housed   0.9034     0.2546   3.549 0.000387 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 735.23  on 604  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 743.23
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
          # fsummer2の係数に大きな変化はない
      # .fgspr
          multi_model_F2_2 = glm(status~fsummer2 + factor(.fgspr)
                                    ,data=BH_dat,family="binomial")
          lrtest(multi_model_F1,multi_model_F2_2)
```

    ## Error in lrtest.default(multi_model_F1, multi_model_F2_2): models were not all fitted to the same size of dataset

``` r
            # Error 以前と同様にモデルに含まれるデータが欠損値の存在によって異なっている
            # .fgsprに欠損値を持つデータを除く
            multi_model_F1_fgspr = glm(status~fsummer2
                                    ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
             lrtest(multi_model_F1_fgspr,multi_model_F2_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"-367.3901","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-360.8163","3":"3","4":"13.14743","5":"0.004328387","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
             # p =0.004328
          summary(multi_model_F2_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + factor(.fgspr), family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3686  -0.9853  -0.7445   1.2919   1.9399  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7162     0.2105  -8.153 3.56e-16 ***
    ## fsummer2Dry fern     1.5805     0.3008   5.254 1.49e-07 ***
    ## fsummer2Fresh fern   0.6713     0.2327   2.885  0.00391 ** 
    ## fsummer2Not housed   0.6719     0.2701   2.487  0.01287 *  
    ## factor(.fgspr)1      0.7797     0.4588   1.699  0.08924 .  
    ## factor(.fgspr)2      1.3822     0.4634   2.982  0.00286 ** 
    ## factor(.fgspr)3      0.5747     0.2157   2.665  0.00771 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 721.63  on 600  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 735.63
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
          summary(multi_model_F1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3232  -0.9412  -0.6663   1.3908   1.7968  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.3922     0.1724  -8.075 6.75e-16 ***
    ## fsummer2Dry fern     1.7287     0.2947   5.865 4.48e-09 ***
    ## fsummer2Fresh fern   0.8075     0.2260   3.574 0.000352 ***
    ## fsummer2Not housed   0.9034     0.2546   3.549 0.000387 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 735.23  on 604  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 743.23
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
          # fsummer2 Not housedの係数が若干変化している(0.9034-0.6719)/0.9034 = 0.25 つまり25%の変化
          # 交絡作用の可能性があるので後できちんと解析するために覚えておく
          
      # sexp
           multi_model_F2_3 = glm(status~fsummer2 + sexp
                                    ,data=BH_dat,family="binomial")
          lrtest(multi_model_F1,multi_model_F2_3)    
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"4","2":"-367.6126","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"5","2":"-365.0687","3":"1","4":"5.087729","5":"0.02409573","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
            # p = 0.0241 
          summary(multi_model_F2_3)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + sexp, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3908  -0.9744  -0.6975   1.3484   1.9764  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.2897     0.1776  -7.262 3.83e-13 ***
    ## fsummer2Dry fern     1.7787     0.2979   5.972 2.35e-09 ***
    ## fsummer2Fresh fern   0.7913     0.2268   3.489 0.000484 ***
    ## fsummer2Not housed   0.8963     0.2555   3.508 0.000452 ***
    ## sexpMale            -0.5105     0.2315  -2.205 0.027462 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 730.14  on 603  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 740.14
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
          summary(multi_model_F1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3232  -0.9412  -0.6663   1.3908   1.7968  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.3922     0.1724  -8.075 6.75e-16 ***
    ## fsummer2Dry fern     1.7287     0.2947   5.865 4.48e-09 ***
    ## fsummer2Fresh fern   0.8075     0.2260   3.574 0.000352 ***
    ## fsummer2Not housed   0.9034     0.2546   3.549 0.000387 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 735.23  on 604  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 743.23
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
           # 特に係数に変化はない
  #  これらの中でcageのp値が最も低いので、次にcageをモデルに加える
      
# 変数三つ目
  # 同様に残った変数を一つづつ加えていく
    # .fgspr
         multi_model_F3_1 = glm(status~fsummer2 + cage + factor(.fgspr)
                                    ,data=BH_dat,family="binomial")
              multi_model_F2_1_fgspr = glm(status~fsummer2 + cage
                                        ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
          lrtest(multi_model_F2_1_fgspr,multi_model_F3_1) 
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"7","2":"-357.3106","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"10","2":"-350.5028","3":"3","4":"13.61554","5":"0.003478051","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
          # p =0.003478
          summary(multi_model_F3_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage + factor(.fgspr), family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7508  -0.8952  -0.6232   1.0891   2.1041  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.0979     0.2555  -8.213  < 2e-16 ***
    ## fsummer2Dry fern     1.7514     0.3104   5.642 1.69e-08 ***
    ## fsummer2Fresh fern   0.6326     0.2372   2.667  0.00765 ** 
    ## fsummer2Not housed   0.7037     0.2756   2.553  0.01067 *  
    ## cage6 years          0.2000     0.2761   0.724  0.46882    
    ## cage7-8 years        0.3622     0.2496   1.451  0.14668    
    ## cage9+years          1.0782     0.2451   4.398 1.09e-05 ***
    ## factor(.fgspr)1      0.7965     0.4653   1.712  0.08693 .  
    ## factor(.fgspr)2      1.4809     0.4676   3.167  0.00154 ** 
    ## factor(.fgspr)3      0.5578     0.2199   2.537  0.01119 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 701.01  on 597  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 721.01
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
          summary(multi_model_F2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6998  -0.8877  -0.6139   1.1224   1.9645  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7728     0.2206  -8.035 9.33e-16 ***
    ## fsummer2Dry fern     1.9036     0.3047   6.247 4.17e-10 ***
    ## fsummer2Fresh fern   0.7645     0.2301   3.322 0.000893 ***
    ## fsummer2Not housed   0.9190     0.2599   3.536 0.000406 ***
    ## cage6 years          0.1996     0.2724   0.733 0.463856    
    ## cage7-8 years        0.4023     0.2462   1.635 0.102140    
    ## cage9+years          1.0449     0.2419   4.320 1.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 715.41  on 601  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 729.41
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
            # やはりfsummer2 Not housedの係数に変化がみられる
    # sexp
           multi_model_F3_2 = glm(status~fsummer2 + cage + sexp
                                    ,data=BH_dat,family="binomial")
              multi_model_F2_1_sexp = glm(status~fsummer2 + cage
                                        ,data=subset(BH_dat,!is.na(BH_dat$sexp)),family="binomial")
          lrtest(multi_model_F2_1_sexp,multi_model_F3_2) 
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"7","2":"-357.7075","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"8","2":"-356.1477","3":"1","4":"3.119502","5":"0.07736043","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
          # p =0.07736
          summary(multi_model_F3_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage + sexp, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7454  -0.8883  -0.6336   1.1339   2.0298  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.6350     0.2325  -7.032 2.04e-12 ***
    ## fsummer2Dry fern     1.9260     0.3067   6.279 3.41e-10 ***
    ## fsummer2Fresh fern   0.7519     0.2307   3.259 0.001117 ** 
    ## fsummer2Not housed   0.9087     0.2602   3.493 0.000478 ***
    ## cage6 years          0.1314     0.2756   0.477 0.633691    
    ## cage7-8 years        0.2928     0.2536   1.154 0.248332    
    ## cage9+years          0.9864     0.2444   4.036 5.44e-05 ***
    ## sexpMale            -0.4201     0.2414  -1.740 0.081834 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 712.30  on 600  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 728.3
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
          summary(multi_model_F2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6998  -0.8877  -0.6139   1.1224   1.9645  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7728     0.2206  -8.035 9.33e-16 ***
    ## fsummer2Dry fern     1.9036     0.3047   6.247 4.17e-10 ***
    ## fsummer2Fresh fern   0.7645     0.2301   3.322 0.000893 ***
    ## fsummer2Not housed   0.9190     0.2599   3.536 0.000406 ***
    ## cage6 years          0.1996     0.2724   0.733 0.463856    
    ## cage7-8 years        0.4023     0.2462   1.635 0.102140    
    ## cage9+years          1.0449     0.2419   4.320 1.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 715.41  on 601  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 729.41
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
           # 特に係数に変化なし 
  # 同様に.fgsprをモデルに加える

# 変数四つ目
  # 残りの変数はsexpのみ
           multi_model_F4_1 = glm(status~fsummer2 + cage + factor(.fgspr) + sexp
                                    ,data=BH_dat,family="binomial")
           lrtest(multi_model_F3_1,multi_model_F4_1) 
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"11","2":"-348.7685","3":"1","4":"3.468507","5":"0.06254774","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
    # p = 0.06255
           summary(multi_model_F4_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage + factor(.fgspr) + sexp, 
    ##     family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7972  -0.8967  -0.5781   1.0762   2.0428  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.9540     0.2655  -7.359 1.85e-13 ***
    ## fsummer2Dry fern     1.7730     0.3126   5.673 1.41e-08 ***
    ## fsummer2Fresh fern   0.6226     0.2376   2.620  0.00878 ** 
    ## fsummer2Not housed   0.6946     0.2763   2.514  0.01195 *  
    ## cage6 years          0.1308     0.2790   0.469  0.63936    
    ## cage7-8 years        0.2496     0.2564   0.973  0.33039    
    ## cage9+years          1.0182     0.2479   4.107 4.00e-05 ***
    ## factor(.fgspr)1      0.7939     0.4669   1.700  0.08910 .  
    ## factor(.fgspr)2      1.5108     0.4680   3.228  0.00125 ** 
    ## factor(.fgspr)3      0.5559     0.2203   2.524  0.01162 *  
    ## sexpMale            -0.4470     0.2439  -1.833  0.06680 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 697.54  on 596  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 719.54
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
           summary(multi_model_F3_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage + factor(.fgspr), family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7508  -0.8952  -0.6232   1.0891   2.1041  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.0979     0.2555  -8.213  < 2e-16 ***
    ## fsummer2Dry fern     1.7514     0.3104   5.642 1.69e-08 ***
    ## fsummer2Fresh fern   0.6326     0.2372   2.667  0.00765 ** 
    ## fsummer2Not housed   0.7037     0.2756   2.553  0.01067 *  
    ## cage6 years          0.2000     0.2761   0.724  0.46882    
    ## cage7-8 years        0.3622     0.2496   1.451  0.14668    
    ## cage9+years          1.0782     0.2451   4.398 1.09e-05 ***
    ## factor(.fgspr)1      0.7965     0.4653   1.712  0.08693 .  
    ## factor(.fgspr)2      1.4809     0.4676   3.167  0.00154 ** 
    ## factor(.fgspr)3      0.5578     0.2199   2.537  0.01119 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 701.01  on 597  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 721.01
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
            # cage6 yearsおよびcage7-8 yearsの係数に変化がみられる
            # 例えばcage6 yearsの係数は(0.2000-0.1308)/0.2000 = 0.346と35%変化しており交絡作用とみなすことができる
            # しかしながらこの係数のp値を見ると0.63936となっており、有意でない。
            # つまり「cage6 yearsの係数は0でない」という帰無仮説を棄却できないので、この係数は0である可能性がある
            # 0かもしれない係数が35%変化し、変化後も帰無仮説が棄却できていないので、この変化にはあまり意味がない
            # つまりsexpの交絡作用は殆どない、と考える
           
    # 基準とするp=0.05に満たない、かつ交絡作用も認められないのでsexpはモデルに含めない
   
                   
# モデルに加える変数が決まったので、これからそれらの交互作用をモデルに加えるか検討する
# 交互作用を加えるうえでいくつか考慮する点がある
# まず解釈可能な交互作用に絞ることが重要である。例えばfsummer2と.fgsprの交互作用が仮に統計的に優位だったとしても、その解釈は難しい
# 今回はfsummer2cage .fgsprcage
# 続いて交互作用が統計的に有意であるかは今までと同様に尤度比検定が使えるが、変数の有意性を確認するときと比べて検定力が落ちる
# そのため変数を含める基準より緩い基準を使うことが多い(例えばp<0.1)

  # fsummer2*cage
     multi_model_F4_1 = glm(status~fsummer2 + cage + factor(.fgspr) + fsummer2*cage
                                    ,data=BH_dat,family="binomial")      
     lrtest(multi_model_F3_1,multi_model_F4_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"19","2":"-346.9001","3":"9","4":"7.205415","5":"0.6157422","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
     # p =0.6157
     
  # .fgspr*cage
      multi_model_F4_2 = glm(status~fsummer2 + cage + factor(.fgspr) + factor(.fgspr)*cage
                                    ,data=BH_dat,family="binomial")      
     lrtest(multi_model_F3_1,multi_model_F4_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"19","2":"-344.9429","3":"9","4":"11.11978","5":"0.2675877","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
     # p = 0.2676

  # ちなみに.fgspr*fsummer2を見てみる
     multi_model_F4_3 = glm(status~fsummer2 + cage + factor(.fgspr) + factor(.fgspr)*fsummer2
                                    ,data=BH_dat,family="binomial")      
     lrtest(multi_model_F3_1,multi_model_F4_3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"19","2":"-340.3301","3":"9","4":"20.34533","5":"0.01589681","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
     summary(multi_model_F4_3)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage + factor(.fgspr) + factor(.fgspr) * 
    ##     fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8264  -0.8457  -0.5923   1.0687   2.1355  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                          -2.1723     0.3033  -7.162 7.96e-13 ***
    ## fsummer2Dry fern                      0.8913     0.5643   1.580   0.1142    
    ## fsummer2Fresh fern                    0.8676     0.3924   2.211   0.0270 *  
    ## fsummer2Not housed                    1.6737     0.7125   2.349   0.0188 *  
    ## cage6 years                           0.2480     0.2838   0.874   0.3823    
    ## cage7-8 years                         0.4605     0.2548   1.807   0.0707 .  
    ## cage9+years                           1.1401     0.2491   4.578 4.70e-06 ***
    ## factor(.fgspr)1                       1.6023     1.4993   1.069   0.2852    
    ## factor(.fgspr)2                       2.0393     0.8224   2.480   0.0131 *  
    ## factor(.fgspr)3                       0.5207     0.3747   1.390   0.1646    
    ## fsummer2Dry fern:factor(.fgspr)1     16.1247  1197.4862   0.013   0.9893    
    ## fsummer2Fresh fern:factor(.fgspr)1   -1.4338     1.6501  -0.869   0.3849    
    ## fsummer2Not housed:factor(.fgspr)1   -2.2759     1.8647  -1.221   0.2223    
    ## fsummer2Dry fern:factor(.fgspr)2     15.8078   979.6105   0.016   0.9871    
    ## fsummer2Fresh fern:factor(.fgspr)2   -1.2193     1.1051  -1.103   0.2699    
    ## fsummer2Not housed:factor(.fgspr)2  -18.9215  1661.4427  -0.011   0.9909    
    ## fsummer2Dry fern:factor(.fgspr)3      1.0790     0.7038   1.533   0.1253    
    ## fsummer2Fresh fern:factor(.fgspr)3   -0.1913     0.5057  -0.378   0.7052    
    ## fsummer2Not housed:factor(.fgspr)3   -0.9009     0.7845  -1.148   0.2508    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 680.66  on 588  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 718.66
    ## 
    ## Number of Fisher Scoring iterations: 15

``` r
     # p = 0.0159
     # 強い交互作用が見られる。しかしながら結果の解釈が非常に難しい
     # 研究目的のために、この交互作用が重要な場合はもちろんモデルに含める
     
# 最終モデル
summary(multi_model_F3_1)     
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage + factor(.fgspr), family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7508  -0.8952  -0.6232   1.0891   2.1041  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.0979     0.2555  -8.213  < 2e-16 ***
    ## fsummer2Dry fern     1.7514     0.3104   5.642 1.69e-08 ***
    ## fsummer2Fresh fern   0.6326     0.2372   2.667  0.00765 ** 
    ## fsummer2Not housed   0.7037     0.2756   2.553  0.01067 *  
    ## cage6 years          0.2000     0.2761   0.724  0.46882    
    ## cage7-8 years        0.3622     0.2496   1.451  0.14668    
    ## cage9+years          1.0782     0.2451   4.398 1.09e-05 ***
    ## factor(.fgspr)1      0.7965     0.4653   1.712  0.08693 .  
    ## factor(.fgspr)2      1.4809     0.4676   3.167  0.00154 ** 
    ## factor(.fgspr)3      0.5578     0.2199   2.537  0.01119 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 701.01  on 597  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 721.01
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
  # fsummer2と.fgsprの交絡作用の考察
  # fsummer2Not housedの係数が.fgsprをモデルに投入することで減少
  # つまりfsummer2Not housedの目的変数に対する効果のいくつかは放牧期間によって説明される
  # その理由として屋外に代われている牛は放牧される可能性が高い
      table(BH_dat$fsummer2,BH_dat$.fgspr)
```

    ##             
    ##                0   1   2   3
    ##   No fern    114   2   7  88
    ##   Dry fern    21   4   6  41
    ##   Fresh fern  63  12   9 120
    ##   Not housed  10   6   2 103

``` r
# ここで考察が終わりがちだが、cageの係数がfsummer2によって変化するか確認していない
# fsummer2の係数についてはcage投入後に確認したが
# cageの係数について確認するのを忘れやすいのはForward selectionを行う欠点である
       summary(model4)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1384  -0.8842  -0.8060   1.2169   1.6016  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -0.95784    0.14479  -6.615 3.71e-11 ***
    ## cage6 years    0.08776    0.25833   0.340 0.734067    
    ## cage7-8 years  0.22024    0.23359   0.943 0.345748    
    ## cage9+years    0.86547    0.22760   3.803 0.000143 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 775.27  on 608  degrees of freedom
    ## Residual deviance: 759.62  on 605  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 767.62
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
       summary(multi_model_F2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ fsummer2 + cage, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6998  -0.8877  -0.6139   1.1224   1.9645  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7728     0.2206  -8.035 9.33e-16 ***
    ## fsummer2Dry fern     1.9036     0.3047   6.247 4.17e-10 ***
    ## fsummer2Fresh fern   0.7645     0.2301   3.322 0.000893 ***
    ## fsummer2Not housed   0.9190     0.2599   3.536 0.000406 ***
    ## cage6 years          0.1996     0.2724   0.733 0.463856    
    ## cage7-8 years        0.4023     0.2462   1.635 0.102140    
    ## cage9+years          1.0449     0.2419   4.320 1.56e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 774.46  on 607  degrees of freedom
    ## Residual deviance: 715.41  on 601  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 729.41
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    # cage9+yearsの係数はfsummer2の投入によって (1.0449- 0.86547)/1.0449=0.17つまり17%変化している
    # 交絡要因とみなす基準はまちまちだが25%-35%の増減を基準とする場合が多い。今回はこれを交絡とはみなさない
```

``` r
# Method 2: Backward elimination
# 始めに単変量解析でスクリーニングした変数、およびそれらの交互作用を含んだモデルを作成（フルモデル）
# 交互作用について：2変数間もしくはそれ以上の変数間の交互作用を作れるが通常3変数間以上の交互作用は解釈が難しい
# よって2変数間の交互作用のみを今回は含める
# またどの2変数間で交互作用を含めるかは研究の目的により異なる
# 今回はfsummer2とcageの交互作用のみに着目する

multi_model_full_B = glm(status~cage + factor(.fgspr) + fsummer2 + sexp +
                         cage*fsummer2 
                          ,data=BH_dat,family="binomial")
summary(multi_model_full_B)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2 + sexp + 
    ##     cage * fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8033  -0.8766  -0.6041   1.0468   2.1433  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -2.19095    0.37822  -5.793 6.92e-09 ***
    ## cage6 years                       0.31141    0.57848   0.538  0.59035    
    ## cage7-8 years                     0.93111    0.47695   1.952  0.05091 .  
    ## cage9+years                       1.07538    0.50173   2.143  0.03209 *  
    ## factor(.fgspr)1                   0.80222    0.48028   1.670  0.09486 .  
    ## factor(.fgspr)2                   1.40938    0.47395   2.974  0.00294 ** 
    ## factor(.fgspr)3                   0.55957    0.22183   2.522  0.01165 *  
    ## fsummer2Dry fern                  2.21244    0.49022   4.513 6.39e-06 ***
    ## fsummer2Fresh fern                0.97965    0.45956   2.132  0.03303 *  
    ## fsummer2Not housed                0.87295    0.48106   1.815  0.06958 .  
    ## sexpMale                         -0.49292    0.24993  -1.972  0.04858 *  
    ## cage6 years:fsummer2Dry fern      0.04637    0.91318   0.051  0.95951    
    ## cage7-8 years:fsummer2Dry fern   -1.75738    0.87340  -2.012  0.04421 *  
    ## cage9+years:fsummer2Dry fern     -0.63958    0.94118  -0.680  0.49679    
    ## cage6 years:fsummer2Fresh fern   -0.22390    0.72356  -0.309  0.75699    
    ## cage7-8 years:fsummer2Fresh fern -1.07573    0.64680  -1.663  0.09628 .  
    ## cage9+years:fsummer2Fresh fern   -0.10848    0.64167  -0.169  0.86575    
    ## cage6 years:fsummer2Not housed   -0.67291    0.93572  -0.719  0.47206    
    ## cage7-8 years:fsummer2Not housed -0.52632    0.68191  -0.772  0.44022    
    ## cage9+years:fsummer2Not housed    0.24001    0.71251   0.337  0.73623    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 689.77  on 587  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 729.77
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# 一つ目の変数の除去
# Backward elimination process
    # cage*fsummer2
    multi_model_full_B1_1 = glm(status~cage + factor(.fgspr) + fsummer2 + sexp
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_B1_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2 + sexp, 
    ##     family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7972  -0.8967  -0.5781   1.0762   2.0428  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.9540     0.2655  -7.359 1.85e-13 ***
    ## cage6 years          0.1308     0.2790   0.469  0.63936    
    ## cage7-8 years        0.2496     0.2564   0.973  0.33039    
    ## cage9+years          1.0182     0.2479   4.107 4.00e-05 ***
    ## factor(.fgspr)1      0.7939     0.4669   1.700  0.08910 .  
    ## factor(.fgspr)2      1.5108     0.4680   3.228  0.00125 ** 
    ## factor(.fgspr)3      0.5559     0.2203   2.524  0.01162 *  
    ## fsummer2Dry fern     1.7730     0.3126   5.673 1.41e-08 ***
    ## fsummer2Fresh fern   0.6226     0.2376   2.620  0.00878 ** 
    ## fsummer2Not housed   0.6946     0.2763   2.514  0.01195 *  
    ## sexpMale            -0.4470     0.2439  -1.833  0.06680 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 697.54  on 596  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 719.54
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_B,multi_model_full_B1_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"20","2":"-344.8848","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"11","2":"-348.7685","3":"-9","4":"7.767376","5":"0.5577485","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.5577
    
    # sexp
    multi_model_full_B1_2 = glm(status~cage + factor(.fgspr) + fsummer2 + cage*fsummer2
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_B1_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2 + cage * 
    ##     fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7332  -0.8630  -0.6635   1.0692   2.2043  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -2.33715    0.37072  -6.304 2.89e-10 ***
    ## cage6 years                       0.37537    0.57580   0.652  0.51446    
    ## cage7-8 years                     1.04789    0.47369   2.212  0.02695 *  
    ## cage9+years                       1.12076    0.49936   2.244  0.02481 *  
    ## factor(.fgspr)1                   0.80555    0.47768   1.686  0.09172 .  
    ## factor(.fgspr)2                   1.38569    0.47411   2.923  0.00347 ** 
    ## factor(.fgspr)3                   0.56017    0.22165   2.527  0.01150 *  
    ## fsummer2Dry fern                  2.14340    0.48491   4.420 9.86e-06 ***
    ## fsummer2Fresh fern                0.98108    0.45854   2.140  0.03239 *  
    ## fsummer2Not housed                0.88920    0.47969   1.854  0.06378 .  
    ## cage6 years:fsummer2Dry fern      0.11204    0.90979   0.123  0.90199    
    ## cage7-8 years:fsummer2Dry fern   -1.65937    0.86967  -1.908  0.05639 .  
    ## cage9+years:fsummer2Dry fern     -0.53237    0.93300  -0.571  0.56827    
    ## cage6 years:fsummer2Fresh fern   -0.24118    0.72132  -0.334  0.73811    
    ## cage7-8 years:fsummer2Fresh fern -1.08017    0.64603  -1.672  0.09452 .  
    ## cage9+years:fsummer2Fresh fern   -0.06492    0.63919  -0.102  0.91910    
    ## cage6 years:fsummer2Not housed   -0.60778    0.93347  -0.651  0.51498    
    ## cage7-8 years:fsummer2Not housed -0.54537    0.68035  -0.802  0.42278    
    ## cage9+years:fsummer2Not housed    0.19167    0.70757   0.271  0.78648    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 693.80  on 588  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 731.8
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_B,multi_model_full_B1_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"20","2":"-344.8848","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"19","2":"-346.9001","3":"-1","4":"4.030468","5":"0.04468555","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.044
    
    # factor(.fgspr)
    multi_model_full_B1_3 = glm(status~cage + sexp + fsummer2 + cage*fsummer2
                              ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
    lrtest(multi_model_full_B,multi_model_full_B1_3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"20","2":"-344.8848","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"17","2":"-351.1676","3":"-3","4":"12.56546","5":"0.005677091","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.005677

  # cageとfsummer2は現在は除かない。なぜならこれらのうち1変数を除くと自動的にcage*fsummer2の交互作用を除くことになるため
  # 除いた3変数のうち最もp値が高かったcage*fsummer2を除く
    
# 二つ目の変数の除去
    # ここからは、変数除去によって変数の係数に変化があるか（交絡作用の有無）慎重に観察する
# Backward elimination process
    # sexp
    multi_model_full_B2_1 = glm(status~cage + factor(.fgspr) + fsummer2
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_B2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7508  -0.8952  -0.6232   1.0891   2.1041  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.0979     0.2555  -8.213  < 2e-16 ***
    ## cage6 years          0.2000     0.2761   0.724  0.46882    
    ## cage7-8 years        0.3622     0.2496   1.451  0.14668    
    ## cage9+years          1.0782     0.2451   4.398 1.09e-05 ***
    ## factor(.fgspr)1      0.7965     0.4653   1.712  0.08693 .  
    ## factor(.fgspr)2      1.4809     0.4676   3.167  0.00154 ** 
    ## factor(.fgspr)3      0.5578     0.2199   2.537  0.01119 *  
    ## fsummer2Dry fern     1.7514     0.3104   5.642 1.69e-08 ***
    ## fsummer2Fresh fern   0.6326     0.2372   2.667  0.00765 ** 
    ## fsummer2Not housed   0.7037     0.2756   2.553  0.01067 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 701.01  on 597  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 721.01
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_B1_1,multi_model_full_B2_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"10","2":"-350.5028","3":"-1","4":"3.468507","5":"0.06254774","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.06255
    
    # fsummer2
    multi_model_full_B2_2 = glm(status~cage + factor(.fgspr) + sexp
                              ,data=subset(BH_dat,!is.na(BH_dat$fsummer2)),family="binomial")
    summary(multi_model_full_B2_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + sexp, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$fsummer2)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7049  -0.9263  -0.6793   1.2353   1.9708  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.38049    0.21785  -6.337 2.34e-10 ***
    ## cage6 years      0.03151    0.26699   0.118 0.906053    
    ## cage7-8 years    0.05714    0.24561   0.233 0.816054    
    ## cage9+years      0.84098    0.23605   3.563 0.000367 ***
    ## factor(.fgspr)1  1.11440    0.44964   2.478 0.013197 *  
    ## factor(.fgspr)2  1.72663    0.45516   3.793 0.000149 ***
    ## factor(.fgspr)3  0.72479    0.20376   3.557 0.000375 ***
    ## sexpMale        -0.40662    0.23497  -1.731 0.083537 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 731.30  on 599  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 747.3
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
      ### cageの係数がfsummer2を除く前と比較して(multi_model_full_B1_1)大幅に変化している
      ### cageとfsummer2の間には交絡が起こっている可能性あり
    lrtest(multi_model_full_B1_1,multi_model_full_B2_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"8","2":"-365.6510","3":"-3","4":"33.76486","5":"2.221057e-07","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 2.221e-07
    
    # factor(.fgspr)
    multi_model_full_B2_3 = glm(status~cage  + fsummer2 + sexp
                              ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
    summary(multi_model_full_B2_3)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + fsummer2 + sexp, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$.fgspr)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7504  -0.8877  -0.6360   1.1303   2.0279  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.6263     0.2324  -6.997 2.62e-12 ***
    ## cage6 years          0.1308     0.2756   0.474 0.635145    
    ## cage7-8 years        0.2914     0.2535   1.150 0.250299    
    ## cage9+years          0.9972     0.2447   4.074 4.61e-05 ***
    ## fsummer2Dry fern     1.9176     0.3068   6.251 4.08e-10 ***
    ## fsummer2Fresh fern   0.7410     0.2309   3.209 0.001332 ** 
    ## fsummer2Not housed   0.8983     0.2603   3.451 0.000558 ***
    ## sexpMale            -0.4237     0.2414  -1.755 0.079272 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 711.45  on 599  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 727.45
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_B1_1,multi_model_full_B2_3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"8","2":"-355.7238","3":"-3","4":"13.9106","5":"0.003029406","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.003029    
    
    # cage
     multi_model_full_B2_4 = glm(status~sexp + factor(.fgspr) + fsummer2
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_B2_4)
```

    ## 
    ## Call:
    ## glm(formula = status ~ sexp + factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4362  -0.9954  -0.6145   1.2613   1.8938  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.6111     0.2151  -7.490 6.87e-14 ***
    ## sexpMale            -0.5303     0.2349  -2.257  0.02399 *  
    ## factor(.fgspr)1      0.7604     0.4623   1.645  0.09999 .  
    ## factor(.fgspr)2      1.4160     0.4623   3.063  0.00219 ** 
    ## factor(.fgspr)3      0.5702     0.2166   2.633  0.00846 ** 
    ## fsummer2Dry fern     1.6313     0.3040   5.365 8.09e-08 ***
    ## fsummer2Fresh fern   0.6612     0.2333   2.835  0.00459 ** 
    ## fsummer2Not housed   0.6678     0.2716   2.458  0.01396 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 716.29  on 599  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 732.29
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_B1_1,multi_model_full_B2_4)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"8","2":"-358.1463","3":"-3","4":"18.75546","5":"0.0003071456","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p =  0.0003071
    
    # sexpを除いた際のp値のみが基準とするp<0.05より高いので(0.06)sexpを除く
    
    
# 三つ目の変数の除去
    # 引き続き変数除去によって変数の係数に変化があるか（交絡作用の有無）慎重に観察する
# Backward elimination process
    
    # fsummer2
    multi_model_full_B3_1 = glm(status~cage + factor(.fgspr) 
                              ,data=subset(BH_dat,!is.na(BH_dat$fsummer2)),family="binomial")
    summary(multi_model_full_B3_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr), family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$fsummer2)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6565  -0.9015  -0.6587   1.1287   1.8488  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.50937    0.20599  -7.327 2.35e-13 ***
    ## cage6 years      0.09185    0.26428   0.348 0.728186    
    ## cage7-8 years    0.16069    0.23887   0.673 0.501146    
    ## cage9+years      0.89802    0.23327   3.850 0.000118 ***
    ## factor(.fgspr)1  1.11992    0.44936   2.492 0.012693 *  
    ## factor(.fgspr)2  1.69084    0.45192   3.741 0.000183 ***
    ## factor(.fgspr)3  0.72709    0.20340   3.575 0.000351 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 734.38  on 600  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 748.38
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    # 比較対象はfsummer2を除く前のmulti_model_full_B2_1
    summary(multi_model_full_B2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7508  -0.8952  -0.6232   1.0891   2.1041  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.0979     0.2555  -8.213  < 2e-16 ***
    ## cage6 years          0.2000     0.2761   0.724  0.46882    
    ## cage7-8 years        0.3622     0.2496   1.451  0.14668    
    ## cage9+years          1.0782     0.2451   4.398 1.09e-05 ***
    ## factor(.fgspr)1      0.7965     0.4653   1.712  0.08693 .  
    ## factor(.fgspr)2      1.4809     0.4676   3.167  0.00154 ** 
    ## factor(.fgspr)3      0.5578     0.2199   2.537  0.01119 *  
    ## fsummer2Dry fern     1.7514     0.3104   5.642 1.69e-08 ***
    ## fsummer2Fresh fern   0.6326     0.2372   2.667  0.00765 ** 
    ## fsummer2Not housed   0.7037     0.2756   2.553  0.01067 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 701.01  on 597  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 721.01
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
      ### cageおよび.fgspr両方の係数に変化が認められる
    lrtest(multi_model_full_B2_1,multi_model_full_B3_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-367.1924","3":"-3","4":"33.37924","5":"2.678792e-07","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 2.679e-07
    
    # factor(.fgspr)
    multi_model_full_B3_2 = glm(status~cage  + fsummer2
                              ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
    summary(multi_model_full_B3_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + fsummer2, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$.fgspr)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7041  -0.8758  -0.6159   1.1228   1.9614  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7655     0.2205  -8.007 1.17e-15 ***
    ## cage6 years          0.1995     0.2723   0.733 0.463812    
    ## cage7-8 years        0.4019     0.2461   1.633 0.102367    
    ## cage9+years          1.0554     0.2422   4.357 1.32e-05 ***
    ## fsummer2Dry fern     1.8954     0.3047   6.220 4.97e-10 ***
    ## fsummer2Fresh fern   0.7542     0.2303   3.275 0.001056 ** 
    ## fsummer2Not housed   0.9093     0.2600   3.497 0.000471 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 714.62  on 600  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 728.62
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
      ### cageの係数の変化はほぼなし
    lrtest(multi_model_full_B2_1,multi_model_full_B3_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-357.3106","3":"-3","4":"13.61554","5":"0.003478051","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.003478    
    
    # cage
     multi_model_full_B3_3 = glm(status~factor(.fgspr)  + fsummer2
                              ,data=subset(BH_dat,!is.na(BH_dat$cage)),family="binomial")
    summary(multi_model_full_B3_3)
```

    ## 
    ## Call:
    ## glm(formula = status ~ factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$cage)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3686  -0.9853  -0.7445   1.2919   1.9399  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7162     0.2105  -8.153 3.56e-16 ***
    ## factor(.fgspr)1      0.7797     0.4588   1.699  0.08924 .  
    ## factor(.fgspr)2      1.3822     0.4634   2.982  0.00286 ** 
    ## factor(.fgspr)3      0.5747     0.2157   2.665  0.00771 ** 
    ## fsummer2Dry fern     1.5805     0.3008   5.254 1.49e-07 ***
    ## fsummer2Fresh fern   0.6713     0.2327   2.885  0.00391 ** 
    ## fsummer2Not housed   0.6719     0.2701   2.487  0.01287 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 721.63  on 600  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 735.63
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_B2_1,multi_model_full_B3_3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-360.8163","3":"-3","4":"20.62711","5":"0.0001258176","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p =0.0001258
   # 全ての変数において、それらの除去が有意なP値を取るのでモデルから除去できない
# 最終モデルは　multi_model_full_B2_1
```

``` r
# Method 3: Backward elimination + forward selection
# Backward eliminationのみのプロセスと同様、始めに単変量解析でスクリーニングした変数、およびそれらの交互作用を含んだモデルを作成（フルモデル）
# Method 2との違いはBackward eliminationで変数を除去するたびに、既に除去された変数(もしくは交互作用)をモデルに戻す
# この作業によって特定の変数の存在によって隠れていた関係を(その変数を除くことで)明らかにできる可能性がある

multi_model_full_BF = glm(status~cage + factor(.fgspr) + fsummer2 + sexp +
                         cage*fsummer2 
                          ,data=BH_dat,family="binomial")
summary(multi_model_full_BF)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2 + sexp + 
    ##     cage * fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8033  -0.8766  -0.6041   1.0468   2.1433  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -2.19095    0.37822  -5.793 6.92e-09 ***
    ## cage6 years                       0.31141    0.57848   0.538  0.59035    
    ## cage7-8 years                     0.93111    0.47695   1.952  0.05091 .  
    ## cage9+years                       1.07538    0.50173   2.143  0.03209 *  
    ## factor(.fgspr)1                   0.80222    0.48028   1.670  0.09486 .  
    ## factor(.fgspr)2                   1.40938    0.47395   2.974  0.00294 ** 
    ## factor(.fgspr)3                   0.55957    0.22183   2.522  0.01165 *  
    ## fsummer2Dry fern                  2.21244    0.49022   4.513 6.39e-06 ***
    ## fsummer2Fresh fern                0.97965    0.45956   2.132  0.03303 *  
    ## fsummer2Not housed                0.87295    0.48106   1.815  0.06958 .  
    ## sexpMale                         -0.49292    0.24993  -1.972  0.04858 *  
    ## cage6 years:fsummer2Dry fern      0.04637    0.91318   0.051  0.95951    
    ## cage7-8 years:fsummer2Dry fern   -1.75738    0.87340  -2.012  0.04421 *  
    ## cage9+years:fsummer2Dry fern     -0.63958    0.94118  -0.680  0.49679    
    ## cage6 years:fsummer2Fresh fern   -0.22390    0.72356  -0.309  0.75699    
    ## cage7-8 years:fsummer2Fresh fern -1.07573    0.64680  -1.663  0.09628 .  
    ## cage9+years:fsummer2Fresh fern   -0.10848    0.64167  -0.169  0.86575    
    ## cage6 years:fsummer2Not housed   -0.67291    0.93572  -0.719  0.47206    
    ## cage7-8 years:fsummer2Not housed -0.52632    0.68191  -0.772  0.44022    
    ## cage9+years:fsummer2Not housed    0.24001    0.71251   0.337  0.73623    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 689.77  on 587  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 729.77
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# 一つ目の変数の除去
# Backward elimination process
    # cage*fsummer2
    multi_model_full_BF1_1 = glm(status~cage + factor(.fgspr) + fsummer2 + sexp
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_BF1_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2 + sexp, 
    ##     family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7972  -0.8967  -0.5781   1.0762   2.0428  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.9540     0.2655  -7.359 1.85e-13 ***
    ## cage6 years          0.1308     0.2790   0.469  0.63936    
    ## cage7-8 years        0.2496     0.2564   0.973  0.33039    
    ## cage9+years          1.0182     0.2479   4.107 4.00e-05 ***
    ## factor(.fgspr)1      0.7939     0.4669   1.700  0.08910 .  
    ## factor(.fgspr)2      1.5108     0.4680   3.228  0.00125 ** 
    ## factor(.fgspr)3      0.5559     0.2203   2.524  0.01162 *  
    ## fsummer2Dry fern     1.7730     0.3126   5.673 1.41e-08 ***
    ## fsummer2Fresh fern   0.6226     0.2376   2.620  0.00878 ** 
    ## fsummer2Not housed   0.6946     0.2763   2.514  0.01195 *  
    ## sexpMale            -0.4470     0.2439  -1.833  0.06680 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 697.54  on 596  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 719.54
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_BF,multi_model_full_BF1_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"20","2":"-344.8848","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"11","2":"-348.7685","3":"-9","4":"7.767376","5":"0.5577485","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.5577
    
    # sexp
    multi_model_full_BF1_2 = glm(status~cage + factor(.fgspr) + fsummer2 + cage*fsummer2
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_BF1_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2 + cage * 
    ##     fsummer2, family = "binomial", data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7332  -0.8630  -0.6635   1.0692   2.2043  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                      -2.33715    0.37072  -6.304 2.89e-10 ***
    ## cage6 years                       0.37537    0.57580   0.652  0.51446    
    ## cage7-8 years                     1.04789    0.47369   2.212  0.02695 *  
    ## cage9+years                       1.12076    0.49936   2.244  0.02481 *  
    ## factor(.fgspr)1                   0.80555    0.47768   1.686  0.09172 .  
    ## factor(.fgspr)2                   1.38569    0.47411   2.923  0.00347 ** 
    ## factor(.fgspr)3                   0.56017    0.22165   2.527  0.01150 *  
    ## fsummer2Dry fern                  2.14340    0.48491   4.420 9.86e-06 ***
    ## fsummer2Fresh fern                0.98108    0.45854   2.140  0.03239 *  
    ## fsummer2Not housed                0.88920    0.47969   1.854  0.06378 .  
    ## cage6 years:fsummer2Dry fern      0.11204    0.90979   0.123  0.90199    
    ## cage7-8 years:fsummer2Dry fern   -1.65937    0.86967  -1.908  0.05639 .  
    ## cage9+years:fsummer2Dry fern     -0.53237    0.93300  -0.571  0.56827    
    ## cage6 years:fsummer2Fresh fern   -0.24118    0.72132  -0.334  0.73811    
    ## cage7-8 years:fsummer2Fresh fern -1.08017    0.64603  -1.672  0.09452 .  
    ## cage9+years:fsummer2Fresh fern   -0.06492    0.63919  -0.102  0.91910    
    ## cage6 years:fsummer2Not housed   -0.60778    0.93347  -0.651  0.51498    
    ## cage7-8 years:fsummer2Not housed -0.54537    0.68035  -0.802  0.42278    
    ## cage9+years:fsummer2Not housed    0.19167    0.70757   0.271  0.78648    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 693.80  on 588  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 731.8
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_BF,multi_model_full_BF1_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"20","2":"-344.8848","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"19","2":"-346.9001","3":"-1","4":"4.030468","5":"0.04468555","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.044
    
    # factor(.fgspr)
    multi_model_full_BF1_3 = glm(status~cage + sexp + fsummer2 + cage*fsummer2
                              ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
    lrtest(multi_model_full_BF,multi_model_full_BF1_3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"20","2":"-344.8848","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"17","2":"-351.1676","3":"-3","4":"12.56546","5":"0.005677091","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.005677

  # cageとfsummer2は現在は除かない。なぜならこれらのうち1変数を除くと自動的にcage*fsummer2の交互作用を除くことになるため
  # 除いた3変数のうち最もp値が高かったcage*fsummer2を除く
    
# 二つ目の変数の除去
    # ここからは、変数除去によって変数の係数に変化があるか（交絡作用の有無）慎重に観察する
# Backward elimination process
    # sexp
    multi_model_full_BF2_1 = glm(status~cage + factor(.fgspr) + fsummer2
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_BF2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7508  -0.8952  -0.6232   1.0891   2.1041  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.0979     0.2555  -8.213  < 2e-16 ***
    ## cage6 years          0.2000     0.2761   0.724  0.46882    
    ## cage7-8 years        0.3622     0.2496   1.451  0.14668    
    ## cage9+years          1.0782     0.2451   4.398 1.09e-05 ***
    ## factor(.fgspr)1      0.7965     0.4653   1.712  0.08693 .  
    ## factor(.fgspr)2      1.4809     0.4676   3.167  0.00154 ** 
    ## factor(.fgspr)3      0.5578     0.2199   2.537  0.01119 *  
    ## fsummer2Dry fern     1.7514     0.3104   5.642 1.69e-08 ***
    ## fsummer2Fresh fern   0.6326     0.2372   2.667  0.00765 ** 
    ## fsummer2Not housed   0.7037     0.2756   2.553  0.01067 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 701.01  on 597  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 721.01
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_BF1_1,multi_model_full_BF2_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"10","2":"-350.5028","3":"-1","4":"3.468507","5":"0.06254774","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.06255
    
    # fsummer2
    multi_model_full_BF2_2 = glm(status~cage + factor(.fgspr) + sexp
                              ,data=subset(BH_dat,!is.na(BH_dat$fsummer2)),family="binomial")
    summary(multi_model_full_BF2_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + sexp, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$fsummer2)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7049  -0.9263  -0.6793   1.2353   1.9708  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.38049    0.21785  -6.337 2.34e-10 ***
    ## cage6 years      0.03151    0.26699   0.118 0.906053    
    ## cage7-8 years    0.05714    0.24561   0.233 0.816054    
    ## cage9+years      0.84098    0.23605   3.563 0.000367 ***
    ## factor(.fgspr)1  1.11440    0.44964   2.478 0.013197 *  
    ## factor(.fgspr)2  1.72663    0.45516   3.793 0.000149 ***
    ## factor(.fgspr)3  0.72479    0.20376   3.557 0.000375 ***
    ## sexpMale        -0.40662    0.23497  -1.731 0.083537 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 731.30  on 599  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 747.3
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
      ### cageの係数がfsummer2を除く前と比較して(multi_model_full_BF1_1)大幅に変化している
      ### cageとfsummer2の間には交絡が起こっている可能性あり
    lrtest(multi_model_full_BF1_1,multi_model_full_BF2_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"8","2":"-365.6510","3":"-3","4":"33.76486","5":"2.221057e-07","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 2.221e-07
    
    # factor(.fgspr)
    multi_model_full_BF2_3 = glm(status~cage  + fsummer2 + sexp
                              ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
    summary(multi_model_full_BF2_3)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + fsummer2 + sexp, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$.fgspr)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7504  -0.8877  -0.6360   1.1303   2.0279  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.6263     0.2324  -6.997 2.62e-12 ***
    ## cage6 years          0.1308     0.2756   0.474 0.635145    
    ## cage7-8 years        0.2914     0.2535   1.150 0.250299    
    ## cage9+years          0.9972     0.2447   4.074 4.61e-05 ***
    ## fsummer2Dry fern     1.9176     0.3068   6.251 4.08e-10 ***
    ## fsummer2Fresh fern   0.7410     0.2309   3.209 0.001332 ** 
    ## fsummer2Not housed   0.8983     0.2603   3.451 0.000558 ***
    ## sexpMale            -0.4237     0.2414  -1.755 0.079272 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 711.45  on 599  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 727.45
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_BF1_1,multi_model_full_BF2_3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"8","2":"-355.7238","3":"-3","4":"13.9106","5":"0.003029406","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.003029    
    
    # cage
     multi_model_full_BF2_4 = glm(status~sexp + factor(.fgspr) + fsummer2
                              ,data=BH_dat,family="binomial")
    summary(multi_model_full_BF2_4)
```

    ## 
    ## Call:
    ## glm(formula = status ~ sexp + factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4362  -0.9954  -0.6145   1.2613   1.8938  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.6111     0.2151  -7.490 6.87e-14 ***
    ## sexpMale            -0.5303     0.2349  -2.257  0.02399 *  
    ## factor(.fgspr)1      0.7604     0.4623   1.645  0.09999 .  
    ## factor(.fgspr)2      1.4160     0.4623   3.063  0.00219 ** 
    ## factor(.fgspr)3      0.5702     0.2166   2.633  0.00846 ** 
    ## fsummer2Dry fern     1.6313     0.3040   5.365 8.09e-08 ***
    ## fsummer2Fresh fern   0.6612     0.2333   2.835  0.00459 ** 
    ## fsummer2Not housed   0.6678     0.2716   2.458  0.01396 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 716.29  on 599  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 732.29
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_BF1_1,multi_model_full_BF2_4)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"11","2":"-348.7685","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"8","2":"-358.1463","3":"-3","4":"18.75546","5":"0.0003071456","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p =  0.0003071
    
    # sexpを除いた際のp値のみが基準とするp<0.05より高いので(0.06)sexpを除く
    
# Forward selection process
    # ここで既に除いた変数をモデルに戻し(forward selection)変数が本当に目的変数と関連がないか調べる
    # 既に除いた変数はcage*fsummer2とsexpだが、後者は今除いたばかりなので前者のみ戻す
    # (sexpを戻すとmulti_model_full_BF2_1になるだけ)
    
    multi_model_full_BF2_1_1 = glm(status~cage + factor(.fgspr) + fsummer2 + cage*fsummer2
                              ,data=BH_dat,family="binomial")
    lrtest(multi_model_full_BF2_1,multi_model_full_BF2_1_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"19","2":"-346.9001","3":"9","4":"7.205415","5":"0.6157422","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.6157 なのでモデルには加えない
    
    
    
# 三つ目の変数の除去
    # 引き続き変数除去によって変数の係数に変化があるか（交絡作用の有無）慎重に観察する
# Backward elimination process
    
    # fsummer2
    multi_model_full_BF3_1 = glm(status~cage + factor(.fgspr) 
                              ,data=subset(BH_dat,!is.na(BH_dat$fsummer2)),family="binomial")
    summary(multi_model_full_BF3_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr), family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$fsummer2)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6565  -0.9015  -0.6587   1.1287   1.8488  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.50937    0.20599  -7.327 2.35e-13 ***
    ## cage6 years      0.09185    0.26428   0.348 0.728186    
    ## cage7-8 years    0.16069    0.23887   0.673 0.501146    
    ## cage9+years      0.89802    0.23327   3.850 0.000118 ***
    ## factor(.fgspr)1  1.11992    0.44936   2.492 0.012693 *  
    ## factor(.fgspr)2  1.69084    0.45192   3.741 0.000183 ***
    ## factor(.fgspr)3  0.72709    0.20340   3.575 0.000351 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 734.38  on 600  degrees of freedom
    ##   (3 observations deleted due to missingness)
    ## AIC: 748.38
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    # 比較対象はfsummer2を除く前のmulti_model_full_BF2_1
    summary(multi_model_full_BF2_1)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = BH_dat)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7508  -0.8952  -0.6232   1.0891   2.1041  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -2.0979     0.2555  -8.213  < 2e-16 ***
    ## cage6 years          0.2000     0.2761   0.724  0.46882    
    ## cage7-8 years        0.3622     0.2496   1.451  0.14668    
    ## cage9+years          1.0782     0.2451   4.398 1.09e-05 ***
    ## factor(.fgspr)1      0.7965     0.4653   1.712  0.08693 .  
    ## factor(.fgspr)2      1.4809     0.4676   3.167  0.00154 ** 
    ## factor(.fgspr)3      0.5578     0.2199   2.537  0.01119 *  
    ## fsummer2Dry fern     1.7514     0.3104   5.642 1.69e-08 ***
    ## fsummer2Fresh fern   0.6326     0.2372   2.667  0.00765 ** 
    ## fsummer2Not housed   0.7037     0.2756   2.553  0.01067 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 701.01  on 597  degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## AIC: 721.01
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
      ### cageおよび.fgspr両方の係数に変化が認められる
    lrtest(multi_model_full_BF2_1,multi_model_full_BF3_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-367.1924","3":"-3","4":"33.37924","5":"2.678792e-07","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 2.679e-07
    
    # factor(.fgspr)
    multi_model_full_BF3_2 = glm(status~cage  + fsummer2
                              ,data=subset(BH_dat,!is.na(BH_dat$.fgspr)),family="binomial")
    summary(multi_model_full_BF3_2)
```

    ## 
    ## Call:
    ## glm(formula = status ~ cage + fsummer2, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$.fgspr)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7041  -0.8758  -0.6159   1.1228   1.9614  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7655     0.2205  -8.007 1.17e-15 ***
    ## cage6 years          0.1995     0.2723   0.733 0.463812    
    ## cage7-8 years        0.4019     0.2461   1.633 0.102367    
    ## cage9+years          1.0554     0.2422   4.357 1.32e-05 ***
    ## fsummer2Dry fern     1.8954     0.3047   6.220 4.97e-10 ***
    ## fsummer2Fresh fern   0.7542     0.2303   3.275 0.001056 ** 
    ## fsummer2Not housed   0.9093     0.2600   3.497 0.000471 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 714.62  on 600  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 728.62
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
      ### cageの係数の変化はほぼなし
    lrtest(multi_model_full_BF2_1,multi_model_full_BF3_2)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-357.3106","3":"-3","4":"13.61554","5":"0.003478051","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.003478    
    
    # cage
     multi_model_full_BF3_3 = glm(status~factor(.fgspr)  + fsummer2
                              ,data=subset(BH_dat,!is.na(BH_dat$cage)),family="binomial")
    summary(multi_model_full_BF3_3)
```

    ## 
    ## Call:
    ## glm(formula = status ~ factor(.fgspr) + fsummer2, family = "binomial", 
    ##     data = subset(BH_dat, !is.na(BH_dat$cage)))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3686  -0.9853  -0.7445   1.2919   1.9399  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.7162     0.2105  -8.153 3.56e-16 ***
    ## factor(.fgspr)1      0.7797     0.4588   1.699  0.08924 .  
    ## factor(.fgspr)2      1.3822     0.4634   2.982  0.00286 ** 
    ## factor(.fgspr)3      0.5747     0.2157   2.665  0.00771 ** 
    ## fsummer2Dry fern     1.5805     0.3008   5.254 1.49e-07 ***
    ## fsummer2Fresh fern   0.6713     0.2327   2.885  0.00391 ** 
    ## fsummer2Not housed   0.6719     0.2701   2.487  0.01287 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 773.65  on 606  degrees of freedom
    ## Residual deviance: 721.63  on 600  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: 735.63
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
    lrtest(multi_model_full_BF2_1,multi_model_full_BF3_3)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"7","2":"-360.8163","3":"-3","4":"20.62711","5":"0.0001258176","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p =0.0001258
   # 全ての変数において、それらの除去が有意なP値を取るのでモデルから除去できない
    
# Forward selection process
    #　同様に既に除いた変数をモデルに戻し(forward selection)変数が本当に目的変数と関連がないか調べる
    # しかし今回のElimination processでは変数を除去しなかったので現在選択しているモデルはmulti_model_full_BF2_1のまま
    # よって上のmulti_model_full_BF2_1_1のステップと同一であるので変数を加える必要はない
    
    multi_model_full_BF3_1_1 = glm(status~cage + factor(.fgspr) + fsummer2 + cage*fsummer2
                              ,data=BH_dat,family="binomial")
    lrtest(multi_model_full_BF2_1,multi_model_full_BF2_1_1)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["#Df"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LogLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"10","2":"-350.5028","3":"NA","4":"NA","5":"NA","_rn_":"1"},{"1":"19","2":"-346.9001","3":"9","4":"7.205415","5":"0.6157422","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
      # p = 0.6157 なのでモデルには加えない
    
### 結論
###  選択したモデルはmulti_model_full_BF2_1
### 交絡作用について考察するため再びモデルの係数を観察する
    # 最終モデル
      multi_model_full_BF2_1 = glm(status~cage + factor(.fgspr) + fsummer2
                              ,data=BH_dat,family="binomial")
    
      
    # 最終モデルからfsummer2を除いたモデル
      multi_model_full_BF2_1_unadjusted = glm(status~cage + factor(.fgspr) 
                              ,data=BH_dat,family="binomial")
      
    # 比較
      summary(multi_model_full_BF2_1)$coefficients
```

    ##                      Estimate Std. Error    z value     Pr(>|z|)
    ## (Intercept)        -2.0978893  0.2554503 -8.2125129 2.166068e-16
    ## cage6 years         0.2000198  0.2761197  0.7243951 4.688232e-01
    ## cage7-8 years       0.3622204  0.2495749  1.4513497 1.466825e-01
    ## cage9+years         1.0781505  0.2451418  4.3980683 1.092186e-05
    ## factor(.fgspr)1     0.7964538  0.4652732  1.7117980 8.693391e-02
    ## factor(.fgspr)2     1.4809296  0.4675693  3.1672947 1.538643e-03
    ## factor(.fgspr)3     0.5577774  0.2198943  2.5365710 1.119440e-02
    ## fsummer2Dry fern    1.7514052  0.3104482  5.6415382 1.685377e-08
    ## fsummer2Fresh fern  0.6325829  0.2371599  2.6673263 7.645740e-03
    ## fsummer2Not housed  0.7037492  0.2756314  2.5532261 1.067302e-02

``` r
      summary(multi_model_full_BF2_1_unadjusted)$coefficients
```

    ##                   Estimate Std. Error    z value     Pr(>|z|)
    ## (Intercept)     -1.5137248  0.2060664 -7.3458110 2.045151e-13
    ## cage6 years      0.0985045  0.2642026  0.3728369 7.092698e-01
    ## cage7-8 years    0.1677536  0.2387607  0.7026012 4.823043e-01
    ## cage9+years      0.9049907  0.2331564  3.8814748 1.038249e-04
    ## factor(.fgspr)1  1.1193535  0.4494057  2.4907415 1.274768e-02
    ## factor(.fgspr)2  1.6915901  0.4519793  3.7426276 1.821060e-04
    ## factor(.fgspr)3  0.7230719  0.2033694  3.5554600 3.773183e-04

``` r
    # cageとfsummer2の関係について
      # fsummer2を入れることでのcageの係数が上昇している。例えばcage6 yearsの係数は0.0985045から0.2000198になっている
      # (0.2000198-0.0985045)/0.0985045 = 1.03つまり係数は100％上昇しているが、係数のp値は有意でなくこの上昇が意味があるものか判断できない
      # これを交絡作用としては考えない
```
