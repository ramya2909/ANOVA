Golf Ball
================
Author
04/07/2019

\#OBJECTIVE

At the 0.05 level of significance, is there a evidence of a difference
in the mean distances travelled by the golf balls with different
designs?

``` r
mydata=read.csv("Golfball.csv")
```

``` r
attach(mydata)
dim(mydata)
```

    ## [1] 40  2

Inference: There are 40 rows and 2 columns in the given dataset

``` r
str(mydata)
```

    ## 'data.frame':    40 obs. of  2 variables:
    ##  $ Design  : Factor w/ 4 levels "Design1","Design2",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Distance: num  206 208 206 204 210 ...

Inference: Given that there are four levels of design namely,
design1,design2,design3 and design4

``` r
summary(mydata)
```

    ##      Design      Distance    
    ##  Design1:10   Min.   :203.8  
    ##  Design2:10   1st Qu.:211.6  
    ##  Design3:10   Median :221.5  
    ##  Design4:10   Mean   :220.1  
    ##               3rd Qu.:228.4  
    ##               Max.   :235.4

``` r
library(psych)
describeBy(Distance,group=Design)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: Design1
    ##    vars  n   mean   sd median trimmed  mad    min    max range skew
    ## X1    1 10 206.61 2.29 206.25  206.43 2.56 203.81 210.86  7.05 0.53
    ##    kurtosis   se
    ## X1    -1.12 0.72
    ## -------------------------------------------------------- 
    ## group: Design2
    ##    vars  n   mean   sd median trimmed  mad    min    max range skew
    ## X1    1 10 218.52 5.54 217.56  217.99 5.63 211.82 229.43 17.61 0.55
    ##    kurtosis   se
    ## X1    -1.02 1.75
    ## -------------------------------------------------------- 
    ## group: Design3
    ##    vars  n   mean   sd median trimmed  mad   min   max range skew kurtosis
    ## X1    1 10 226.59 4.81 225.78  226.51 4.97 219.5 234.3  14.8  0.2    -1.38
    ##      se
    ## X1 1.52
    ## -------------------------------------------------------- 
    ## group: Design4
    ##    vars  n   mean   sd median trimmed  mad    min    max range  skew
    ## X1    1 10 228.62 4.01 228.92  228.66 3.78 221.53 235.45 13.92 -0.12
    ##    kurtosis   se
    ## X1    -0.99 1.27

``` r
library(lattice)
histogram(~Distance|Design)
```

![](GolfBall_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
boxplot(Distance~Design,horizontal=TRUE,col=c("Red","Blue","Green","Yellow"))
```

![](GolfBall_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> Inference:
Boxplot indicates that there are no outliers

``` r
Model=aov(Distance~Design,data=mydata)
Model
```

    ## Call:
    ##    aov(formula = Distance ~ Design, data = mydata)
    ## 
    ## Terms:
    ##                    Design Residuals
    ## Sum of Squares  2990.9898  676.8244
    ## Deg. of Freedom         3        36
    ## 
    ## Residual standard error: 4.335975
    ## Estimated effects may be unbalanced

``` r
summary(Model)
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## Design       3 2991.0   997.0   53.03 2.73e-13 ***
    ## Residuals   36  676.8    18.8                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Inference: F value is 53.03 which is good, and p value is less than
0.05, indicating there are differences in the mean distance travelled by
the golf balls with different designs.

``` r
# To investigate further we need to do a Tukey test
tuk<-TukeyHSD(Model)
tuk
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Distance ~ Design, data = mydata)
    ## 
    ## $Design
    ##                   diff       lwr       upr     p adj
    ## Design2-Design1 11.902  6.679545 17.124455 0.0000027
    ## Design3-Design1 19.974 14.751545 25.196455 0.0000000
    ## Design4-Design1 22.008 16.785545 27.230455 0.0000000
    ## Design3-Design2  8.072  2.849545 13.294455 0.0010308
    ## Design4-Design2 10.106  4.883545 15.328455 0.0000451
    ## Design4-Design3  2.034 -3.188455  7.256455 0.7221072

``` r
plot(tuk)
```

![](GolfBall_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Inference: Design 4 and Design 3 shows significance with a p value of
0.72 and others does not show any form of significance.
