Lab 11 - Grading the professor, Pt. 2
================
Olivia Zhang
05/14/2025

## Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

## Exercise 1

``` r
m_bty <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg, data = evals)
tidy(m_bty)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.88      0.0761     51.0  1.56e-191
    ## 2 bty_avg       0.0666    0.0163      4.09 5.08e-  5

``` r
glance(m_bty)$r.squared
```

    ## [1] 0.03502226

``` r
glance(m_bty)$adj.r.squared
```

    ## [1] 0.03292903

The linear model predicting average professor evaluation score from
average beauty rating is: score = 3.88 + 0.0666\*bty_avg

The R2 is 0.04, meaning that professors’ beauty rating explains 4% of
variance in the evaluation scores.

The adjusted R2 is 0.03.

## Exercise 2

``` r
m_bty_gen <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg*gender, data = evals)
tidy(m_bty_gen)
```

    ## # A tibble: 4 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          3.95      0.118      33.5  2.92e-125
    ## 2 bty_avg              0.0306    0.0240      1.28 2.02e-  1
    ## 3 gendermale          -0.184     0.153      -1.20 2.32e-  1
    ## 4 bty_avg:gendermale   0.0796    0.0325      2.45 1.46e-  2

``` r
glance(m_bty_gen)$r.squared
```

    ## [1] 0.07128874

``` r
glance(m_bty_gen)$adj.r.squared
```

    ## [1] 0.06521873

The linear model predicting average professor evaluation score from
average beauty rating and gender is: score = 3.95 + 0.0306*bty_avg -
0.184*male + 0.0796*bty_avg*male

## Exercise 3

The intercept of m_bty_gen model is 3.95, meaning female professors who
have an average beauty rating of 0 will have an evaluation score of 3.95
on average.

The coefficient of bty_avg is 0.0306, meaning that for female
professors, as the average beauty rating increases 1 unit, the average
evaluation score increases 0.0306

The coefficient of gender_male is 0.184, meaning that for professor
whose beauty rating is 0, the average evaluation score of male
professors is 0.184 lower than that of female professors.

The coefficient of the interaction term is 0.0796, meaning that the
effect of beauty ratings on evaluation scores depends on gender.

## Exercise 4

The R2 is 0.07, meaning that the model m_bty_gen explains 7% of variance
in the evaluation scores.

## Exercise 5

For male professors, the equation is: score = 3.766 + 0.1102\*bty_avg

## Exercise 6

For two professors who received the same beauty rating, female tends to
have the higher course evaluation score than female.

Note: the direction change if the interaction term is not included.

## Exercise 7

Beauty ratings affect male professors (0.1102) more than female
professors (0.0306).

## Exercise 8

``` r
glance(m_bty_gen)$adj.r.squared > glance(m_bty)$adj.r.squared
```

    ## [1] TRUE

The adjusted R2 is higher for m_bty_gen than for m_bty, meaning that
adding gender is effectively explaining more variance in the evaluation
score than beauty ratings alone.

## Exercise 9

Yes, the slope for bty_avg decreased from 0.0666 (m_bty model) to 0.0306
(m_bty_gen model).

## Exercise 10

``` r
m_bty_rank <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg*rank, data = evals)
tidy(m_bty_rank)
```

    ## # A tibble: 6 × 5
    ##   term                     estimate std.error statistic  p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)                4.10      0.150    27.4    1.80e-98
    ## 2 bty_avg                    0.0417    0.0314    1.33   1.84e- 1
    ## 3 ranktenure track          -0.0188    0.230    -0.0818 9.35e- 1
    ## 4 ranktenured               -0.409     0.182    -2.25   2.52e- 2
    ## 5 bty_avg:ranktenure track  -0.0264    0.0463   -0.570  5.69e- 1
    ## 6 bty_avg:ranktenured        0.0659    0.0392    1.68   9.38e- 2

``` r
glance(m_bty_rank)$r.squared
```

    ## [1] 0.05871159

``` r
glance(m_bty_rank)$adj.r.squared
```

    ## [1] 0.04841303

The linear model predicting average professor evaluation score from
average beauty rating and rank is: score = 4.10 + 0.0417*bty_avg -
0.0188*TenureTrack - 0.409*Tenured - 0.0264*bty_avg*TenureTrack +
0.0659*bty_avg\*Tenured

Intercept: For teaching professors whose beauty rating is 0, their
evaluation score is 4.10.

Coefficient for bty_avg: for teaching professors, as their beauty rating
increases 1 unit, their evaluation score increases 0.0417 unit.

Coefficient for TenureTrack: for professors whose beauty rating is 0,
TenureTrack professors is rated 0.0188 lower than teaching professors.

Coefficient for Tenured: for professors whose beauty rating is 0,
Tenured professors is rated 0.409 lower than teaching professors.

Coefficient for bty_avg\*TenureTrack interaction: the effect on beauty
rating on evaluation score depends on whether the professor is on a
teaching track or tenure track.

Coefficient for bty_avg\*Tenured interaction: the effect on beauty
rating on evaluation score depends on whether the professor is on a
teaching track or already tenured.

## Exercise 11

I would guess that cls_students on its own is probably not a good
predictor, because class size itself doesn’t tell us how many students
completed the eval.

## Exercise 12

``` r
m_cls_students <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ cls_students, data = evals)
tidy(m_cls_students)
```

    ## # A tibble: 2 × 5
    ##   term         estimate std.error statistic p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)  4.16      0.0314     133.      0    
    ## 2 cls_students 0.000188  0.000337     0.558   0.577

``` r
glance(m_cls_students)$r.squared
```

    ## [1] 0.0006744268

``` r
glance(m_cls_students)$adj.r.squared
```

    ## [1] -0.001493308

The p-value for cls_students is 0.577, indicating that cls_students by
iteslf is not a good predictor of evaluation score.

## Exercise 13

I would not include cls_did_eval, because it is redundant (you can
calculate this variable from cls_perc_eval and cls_students).

## Exercise 14

``` r
m_full <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
tidy(m_full)
```

    ## # A tibble: 13 × 5
    ##    term                   estimate std.error statistic  p.value
    ##    <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)            3.53      0.241       14.7   4.65e-40
    ##  2 ranktenure track      -0.107     0.0820      -1.30  1.93e- 1
    ##  3 ranktenured           -0.0450    0.0652      -0.691 4.90e- 1
    ##  4 ethnicitynot minority  0.187     0.0775       2.41  1.63e- 2
    ##  5 gendermale             0.179     0.0515       3.47  5.79e- 4
    ##  6 languagenon-english   -0.127     0.108       -1.17  2.41e- 1
    ##  7 age                   -0.00665   0.00308     -2.16  3.15e- 2
    ##  8 cls_perc_eval          0.00570   0.00155      3.67  2.68e- 4
    ##  9 cls_students           0.000445  0.000358     1.24  2.15e- 1
    ## 10 cls_levelupper         0.0187    0.0556       0.337 7.37e- 1
    ## 11 cls_profssingle       -0.00858   0.0514      -0.167 8.67e- 1
    ## 12 cls_creditsone credit  0.509     0.117        4.35  1.70e- 5
    ## 13 bty_avg                0.0613    0.0167       3.67  2.68e- 4

``` r
glance(m_full)$r.squared
```

    ## [1] 0.1635232

``` r
glance(m_full)$adj.r.squared
```

    ## [1] 0.1412172

## Exercise 15

``` r
m_final <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~  ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_credits + bty_avg, data = evals)
tidy(m_final)
```

    ## # A tibble: 9 × 5
    ##   term                   estimate std.error statistic  p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)            3.39      0.209        16.2  8.89e-47
    ## 2 ethnicitynot minority  0.204     0.0747        2.74 6.43e- 3
    ## 3 gendermale             0.177     0.0503        3.51 4.85e- 4
    ## 4 languagenon-english   -0.151     0.104        -1.46 1.45e- 1
    ## 5 age                   -0.00487   0.00261      -1.87 6.23e- 2
    ## 6 cls_perc_eval          0.00575   0.00154       3.74 2.12e- 4
    ## 7 cls_students           0.000407  0.000343      1.19 2.35e- 1
    ## 8 cls_creditsone credit  0.523     0.105         4.98 9.03e- 7
    ## 9 bty_avg                0.0619    0.0165        3.75 2.03e- 4

``` r
glance(m_final)$r.squared
```

    ## [1] 0.1601644

``` r
glance(m_final)$adj.r.squared
```

    ## [1] 0.1453655

Final linear model: score = 3.39 + 0.204*ethnicity + 0.177*gender -
0.151*language - 0.00487*age + 0.00575*cls_perc_eval +
0.000407*cls_students + 0.523*cls_credits + 0.0619*bty_avg

## Exercise 16

The coefficient of ethnicity is 0.204, meaning that professors who are
not minority are rated 0.204 higher than professors who are minority.

The coefficient of age is -0.00487, meaning as a professor’s age
increases 1 year, their eval rating decreases 0.00487 unit.

## Exercise 17

A professor who receive high evaluation at University of Texas at Austin
is probably not a minority, a male, received education in an English
institution, relatively young, and good looking. Also, they need to be
teaching a one credit course with a lot of students completing the
evaluations.

## Exercise 18

I think this conclusion can probably be applied to other institutions,
because the predictors are common in creating biases in students’
perception of professors and courses. However, since Texas is a deep red
state, some of the biases may be alleviated in blue states.
