
# README

These are analysis operations and data for the study titled “Musical
expertise better predictor of tension in harmonic intervals than
psychoacoustics across North and South Indian listeners” by Imre
Lahdelma, [Tuomas Eerola](https://tuomaseerola.github.io/), Nashra
Ahmad, Martin Clayton, James Armitage, Budhaditya Bhattacharyya, and
Niveshan Munsamy.

## Load data

``` r
rm(list = ls())
library(ggplot2)
library(dplyr)
library(papaja)
options(dplyr.summarise.inform = FALSE)
df <- read.csv('data/indian_tension_ratings.csv')
knitr::kable(head(df),format = 'markdown')
```

| omsi1indian              | omsi1western             | gender | age | participant | Expertise | Interval | tensionrating | Group                  | musicianship | musicianshipW | musicianshipI | ExpertiseN |
|:-------------------------|:-------------------------|:-------|----:|:------------|----------:|:---------|--------------:|:-----------------------|:-------------|:--------------|:--------------|-----------:|
| Music-loving nonmusician | Music-loving nonmusician | Male   |  29 | I1          |         2 | m2       |             5 | Indian - non-musicians | Non-Musician | Non-Musician  | Non-Musician  |  16.666667 |
| Music-loving nonmusician | Music-loving nonmusician | Female |  61 | I2          |         2 | m2       |             2 | Indian - non-musicians | Non-Musician | Non-Musician  | Non-Musician  |  16.666667 |
| Nonmusician              | Nonmusician              | Male   |  25 | I3          |         2 | m2       |             4 | Indian - non-musicians | Non-Musician | Non-Musician  | Non-Musician  |  16.666667 |
| Music-loving nonmusician | Music-loving nonmusician | Male   |  23 | I4          |         2 | m2       |             2 | Indian - non-musicians | Non-Musician | Non-Musician  | Non-Musician  |  16.666667 |
| Music-loving nonmusician | Music-loving nonmusician | Male   |  18 | I5          |         0 | m2       |             5 | Indian - non-musicians | Non-Musician | Non-Musician  | Non-Musician  |   0.000000 |
| Nonmusician              | Nonmusician              | Male   |  22 | I6          |         1 | m2       |             1 | Indian - non-musicians | Non-Musician | Non-Musician  | Non-Musician  |   8.333333 |

**Brief explanation of columns**

- `omsi1indian` refers to participant’s own decision of how they
  identify themselves as indian musicians according to OMSI 1-item
  question.
- `omsi1western` refers to participant’s own decision of how they
  identify themselves as western musicians according to OMSI 1-item
  question.
- `gender` = Self-defined gender (Male/Female)
- `age` = Age in years
- `participant` = unique code for each participant
- `Expertise` = Number of questions correct in custom Indian classical
  music familiary questionnaire. Note that since Carnatic and Hindustani
  questionnaires have different number of questions, we use the
  proportion correct (`ExpertiseN`) as the index for the expertise.
- `tension_rating` = Rating of tension from 1 to 7 (7-point scale)
- `Group` = Carnatic, Hindustani or Indian non-musicians
- `musicianship` = variable inferred from `omsi1indian`
- `participant` = variable inferred from `omsi1western`
- `ExpertiseN` = Proportion of expertise questions correct (0-100)

## Describe participants

``` r
U<-as.character(unique(df$participant))
background <- NULL
for (k in 1:length(U)) {
  tmp <- dplyr::filter(df,participant==U[k])
  background<-rbind(background,tmp[1,])
}

SS <- summarise(group_by(background,Group),N=n(),ageM=mean(age,na.rm=TRUE),ageSE=sd(age,na.rm=TRUE)/sqrt(n()),expertiseM=mean(ExpertiseN,na.rm=TRUE),expertiseSE=sd(ExpertiseN,na.rm=TRUE)/sqrt(n()),female=sum(gender=='Female')/n()*100)

knitr::kable(SS,digits = 2,format = 'markdown')
```

| Group                  |   N |  ageM | ageSE | expertiseM | expertiseSE | female |
|:-----------------------|----:|------:|------:|-----------:|------------:|-------:|
| Carnatic               |  26 | 28.23 |  1.77 |      85.51 |        2.88 |  53.85 |
| Hindustani             |  19 | 31.16 |  2.64 |      82.71 |        4.17 |  36.84 |
| Indian - non-musicians |  26 | 27.12 |  2.12 |      12.18 |        1.15 |  46.15 |

``` r
m1 <- aov(ExpertiseN ~ Group, data=background)
a<-apa_print(summary(m1))
print(a$statistic)
```

    ## $Group
    ## [1] "$F(2, 65) = 234.50$, $p < .001$"

``` r
print(TukeyHSD(m1))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = ExpertiseN ~ Group, data = background)
    ## 
    ## $Group
    ##                                         diff       lwr        upr     p adj
    ## Hindustani-Carnatic                -2.800479 -12.72050   7.119538 0.7775883
    ## Indian - non-musicians-Carnatic   -73.327759 -82.48735 -64.168171 0.0000000
    ## Indian - non-musicians-Hindustani -70.527280 -80.18493 -60.869634 0.0000000

## LMM analysis

### Are the Hindustani and Carnatic musicians tension ratings different from each other?

``` r
library(lme4)
library(lmerTest)
library(emmeans)
library(effectsize)

# Put the intervals in the correct order in the factor
iv_labels <- c('m2','M2','m3','M3','P4','A4','P5','m6','M6','m7','M7','P8')
df$Interval<-factor(df$Interval,levels = iv_labels)
# Take only Carnatic and Hindustani Musicians
tmp<-dplyr::filter(df,Group=='Carnatic' | Group=='Hindustani')
tmp$Group<-factor(tmp$Group)
m1 <- lmer(tensionrating ~ Interval * Group + (1|participant), data=tmp)
print(anova(m1))
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)    
    ## Interval       375.89  34.172    11 471.19 18.4130 <2e-16 ***
    ## Group            1.49   1.487     1  43.11  0.8011 0.3757    
    ## Interval:Group  27.96   2.542    11 471.19  1.3695 0.1839    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# No difference between the two expert groups nor interaction
```

### Differences between and within musicians and non-musicians?

``` r
# Combine two musician groups and relabel everybody to musician/non-musician
df$Group2<-as.character(df$Group)
df$Group2[df$Group=='Carnatic']<-'Musicians'
df$Group2[df$Group=='Hindustani']<-'Musicians'
df$Group2[df$Group=='Indian - non-musicians']<-'Non-musicians'
df$Group2<-factor(df$Group2)

m1 <- lmer(tensionrating ~ Group2 * Interval + (1|participant),data = df)
print(anova(m1))
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                  Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## Group2            2.636  2.6361     1  69.03  1.5836    0.2125    
    ## Interval        160.651 14.6046    11 757.09  8.7738 8.973e-15 ***
    ## Group2:Interval 190.640 17.3309    11 757.09 10.4117 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
F_to_eta2(8.7738, 11, 757.09)  # effect size
```

    ## Eta2 (partial) |       95% CI
    ## -----------------------------
    ## 0.11           | [0.07, 1.00]
    ## 
    ## - One-sided CIs: upper bound fixed at [1.00].

``` r
F_to_eta2(10.4117, 11, 757.09) # effect size
```

    ## Eta2 (partial) |       95% CI
    ## -----------------------------
    ## 0.13           | [0.09, 1.00]
    ## 
    ## - One-sided CIs: upper bound fixed at [1.00].

``` r
print(pairs(emmeans(m1,~ Group2|Interval))) # this for each interval
```

    ## Interval = m2:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)    1.567 0.381 408   4.108  <.0001
    ## 
    ## Interval = M2:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)    0.507 0.381 408   1.329  0.1846
    ## 
    ## Interval = m3:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)   -0.190 0.381 408  -0.497  0.6191
    ## 
    ## Interval = M3:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)   -0.950 0.381 408  -2.492  0.0131
    ## 
    ## Interval = P4:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)   -1.572 0.381 408  -4.121  <.0001
    ## 
    ## Interval = A4:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)    0.489 0.381 408   1.282  0.2006
    ## 
    ## Interval = P5:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)   -1.202 0.383 411  -3.141  0.0018
    ## 
    ## Interval = m6:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)   -0.897 0.383 411  -2.346  0.0195
    ## 
    ## Interval = M6:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)   -1.279 0.381 408  -3.353  0.0009
    ## 
    ## Interval = m7:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)    0.219 0.381 408   0.574  0.5665
    ## 
    ## Interval = M7:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)    0.985 0.381 408   2.584  0.0101
    ## 
    ## Interval = P8:
    ##  contrast                    estimate    SE  df t.ratio p.value
    ##  Musicians - (Non-musicians)   -1.150 0.381 408  -3.014  0.0027
    ## 
    ## Degrees-of-freedom method: kenward-roger

The comparison of intervals across musicians/non-musicians is also
documented in the Figure 1.

#### Check discrimination of intervals within non-musicians

``` r
m1 <- lmer(tensionrating ~ Interval + (1|participant),data = dplyr::filter(df,Group2=='Non-musicians'))
anova(m1)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##          Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
    ## Interval 64.574  5.8703    11   275  4.5092 2.821e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
F_to_epsilon2(4.5092, 11, 275) # effect size
```

    ## Epsilon2 (partial) |       95% CI
    ## ---------------------------------
    ## 0.12               | [0.04, 1.00]
    ## 
    ## - One-sided CIs: upper bound fixed at [1.00].

``` r
# posthocs with multiple corrections
emm <- emmeans(m1,specs = pairwise ~ Interval,pbkrtest.limit = 4345, type = "response")
print(emm$contrasts)
```

    ##  contrast estimate    SE  df t.ratio p.value
    ##  m2 - M2   -0.6154 0.316 275  -1.945  0.7300
    ##  m2 - m3   -0.4231 0.316 275  -1.337  0.9735
    ##  m2 - M3   -0.9615 0.316 275  -3.038  0.1033
    ##  m2 - P4   -1.5385 0.316 275  -4.862  0.0001
    ##  m2 - A4   -1.5000 0.316 275  -4.740  0.0002
    ##  m2 - P5   -1.1154 0.316 275  -3.525  0.0245
    ##  m2 - m6   -1.0385 0.316 275  -3.282  0.0523
    ##  m2 - M6   -1.4231 0.316 275  -4.497  0.0006
    ##  m2 - m7   -1.1923 0.316 275  -3.768  0.0107
    ##  m2 - M7   -0.6923 0.316 275  -2.188  0.5600
    ##  m2 - P8   -0.5385 0.316 275  -1.702  0.8663
    ##  M2 - m3    0.1923 0.316 275   0.608  1.0000
    ##  M2 - M3   -0.3462 0.316 275  -1.094  0.9948
    ##  M2 - P4   -0.9231 0.316 275  -2.917  0.1408
    ##  M2 - A4   -0.8846 0.316 275  -2.795  0.1878
    ##  M2 - P5   -0.5000 0.316 275  -1.580  0.9150
    ##  M2 - m6   -0.4231 0.316 275  -1.337  0.9735
    ##  M2 - M6   -0.8077 0.316 275  -2.552  0.3125
    ##  M2 - m7   -0.5769 0.316 275  -1.823  0.8040
    ##  M2 - M7   -0.0769 0.316 275  -0.243  1.0000
    ##  M2 - P8    0.0769 0.316 275   0.243  1.0000
    ##  m3 - M3   -0.5385 0.316 275  -1.702  0.8663
    ##  m3 - P4   -1.1154 0.316 275  -3.525  0.0245
    ##  m3 - A4   -1.0769 0.316 275  -3.403  0.0361
    ##  m3 - P5   -0.6923 0.316 275  -2.188  0.5600
    ##  m3 - m6   -0.6154 0.316 275  -1.945  0.7300
    ##  m3 - M6   -1.0000 0.316 275  -3.160  0.0742
    ##  m3 - m7   -0.7692 0.316 275  -2.431  0.3890
    ##  m3 - M7   -0.2692 0.316 275  -0.851  0.9995
    ##  m3 - P8   -0.1154 0.316 275  -0.365  1.0000
    ##  M3 - P4   -0.5769 0.316 275  -1.823  0.8040
    ##  M3 - A4   -0.5385 0.316 275  -1.702  0.8663
    ##  M3 - P5   -0.1538 0.316 275  -0.486  1.0000
    ##  M3 - m6   -0.0769 0.316 275  -0.243  1.0000
    ##  M3 - M6   -0.4615 0.316 275  -1.458  0.9502
    ##  M3 - m7   -0.2308 0.316 275  -0.729  0.9999
    ##  M3 - M7    0.2692 0.316 275   0.851  0.9995
    ##  M3 - P8    0.4231 0.316 275   1.337  0.9735
    ##  P4 - A4    0.0385 0.316 275   0.122  1.0000
    ##  P4 - P5    0.4231 0.316 275   1.337  0.9735
    ##  P4 - m6    0.5000 0.316 275   1.580  0.9150
    ##  P4 - M6    0.1154 0.316 275   0.365  1.0000
    ##  P4 - m7    0.3462 0.316 275   1.094  0.9948
    ##  P4 - M7    0.8462 0.316 275   2.674  0.2451
    ##  P4 - P8    1.0000 0.316 275   3.160  0.0742
    ##  A4 - P5    0.3846 0.316 275   1.215  0.9874
    ##  A4 - m6    0.4615 0.316 275   1.458  0.9502
    ##  A4 - M6    0.0769 0.316 275   0.243  1.0000
    ##  A4 - m7    0.3077 0.316 275   0.972  0.9981
    ##  A4 - M7    0.8077 0.316 275   2.552  0.3125
    ##  A4 - P8    0.9615 0.316 275   3.038  0.1033
    ##  P5 - m6    0.0769 0.316 275   0.243  1.0000
    ##  P5 - M6   -0.3077 0.316 275  -0.972  0.9981
    ##  P5 - m7   -0.0769 0.316 275  -0.243  1.0000
    ##  P5 - M7    0.4231 0.316 275   1.337  0.9735
    ##  P5 - P8    0.5769 0.316 275   1.823  0.8040
    ##  m6 - M6   -0.3846 0.316 275  -1.215  0.9874
    ##  m6 - m7   -0.1538 0.316 275  -0.486  1.0000
    ##  m6 - M7    0.3462 0.316 275   1.094  0.9948
    ##  m6 - P8    0.5000 0.316 275   1.580  0.9150
    ##  M6 - m7    0.2308 0.316 275   0.729  0.9999
    ##  M6 - M7    0.7308 0.316 275   2.309  0.4726
    ##  M6 - P8    0.8846 0.316 275   2.795  0.1878
    ##  m7 - M7    0.5000 0.316 275   1.580  0.9150
    ##  m7 - P8    0.6538 0.316 275   2.066  0.6473
    ##  M7 - P8    0.1538 0.316 275   0.486  1.0000
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 12 estimates

``` r
# 7/66 comparison significant at p<.05 level
```

#### Check discrimination of intervals within musicians

``` r
m1 <- lmer(tensionrating ~ Interval + (1|participant),data = dplyr::filter(df,Group2=='Musicians'))
print(anova(m1))
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##          Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
    ## Interval 367.42  33.402    11 482.16  17.848 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
F_to_epsilon2(17.848, 11, 482.16) # effect size
```

    ## Epsilon2 (partial) |       95% CI
    ## ---------------------------------
    ## 0.27               | [0.21, 1.00]
    ## 
    ## - One-sided CIs: upper bound fixed at [1.00].

``` r
# posthocs with multiple corrections
emm <- emmeans(m1,specs = pairwise ~ Interval,pbkrtest.limit = 4345, type = "response")
emm$contrasts
```

    ##  contrast estimate    SE  df t.ratio p.value
    ##  m2 - M2   0.44444 0.288 482   1.541  0.9283
    ##  m2 - m3   1.33333 0.288 482   4.623  0.0003
    ##  m2 - M3   1.55556 0.288 482   5.394  <.0001
    ##  m2 - P4   1.60000 0.288 482   5.548  <.0001
    ##  m2 - A4  -0.42222 0.288 482  -1.464  0.9494
    ##  m2 - P5   1.65341 0.290 482   5.698  <.0001
    ##  m2 - m6   1.42614 0.290 482   4.915  0.0001
    ##  m2 - M6   1.42222 0.288 482   4.931  0.0001
    ##  m2 - m7   0.15556 0.288 482   0.539  1.0000
    ##  m2 - M7  -0.11111 0.288 482  -0.385  1.0000
    ##  m2 - P8   2.17778 0.288 482   7.551  <.0001
    ##  M2 - m3   0.88889 0.288 482   3.082  0.0896
    ##  M2 - M3   1.11111 0.288 482   3.853  0.0073
    ##  M2 - P4   1.15556 0.288 482   4.007  0.0041
    ##  M2 - A4  -0.86667 0.288 482  -3.005  0.1102
    ##  M2 - P5   1.20897 0.290 482   4.167  0.0021
    ##  M2 - m6   0.98169 0.290 482   3.383  0.0368
    ##  M2 - M6   0.97778 0.288 482   3.390  0.0360
    ##  M2 - m7  -0.28889 0.288 482  -1.002  0.9976
    ##  M2 - M7  -0.55556 0.288 482  -1.926  0.7422
    ##  M2 - P8   1.73333 0.288 482   6.010  <.0001
    ##  m3 - M3   0.22222 0.288 482   0.771  0.9998
    ##  m3 - P4   0.26667 0.288 482   0.925  0.9989
    ##  m3 - A4  -1.75556 0.288 482  -6.087  <.0001
    ##  m3 - P5   0.32008 0.290 482   1.103  0.9945
    ##  m3 - m6   0.09281 0.290 482   0.320  1.0000
    ##  m3 - M6   0.08889 0.288 482   0.308  1.0000
    ##  m3 - m7  -1.17778 0.288 482  -4.084  0.0030
    ##  m3 - M7  -1.44444 0.288 482  -5.008  <.0001
    ##  m3 - P8   0.84444 0.288 482   2.928  0.1344
    ##  M3 - P4   0.04444 0.288 482   0.154  1.0000
    ##  M3 - A4  -1.97778 0.288 482  -6.858  <.0001
    ##  M3 - P5   0.09786 0.290 482   0.337  1.0000
    ##  M3 - m6  -0.12942 0.290 482  -0.446  1.0000
    ##  M3 - M6  -0.13333 0.288 482  -0.462  1.0000
    ##  M3 - m7  -1.40000 0.288 482  -4.854  0.0001
    ##  M3 - M7  -1.66667 0.288 482  -5.779  <.0001
    ##  M3 - P8   0.62222 0.288 482   2.157  0.5814
    ##  P4 - A4  -2.02222 0.288 482  -7.012  <.0001
    ##  P4 - P5   0.05341 0.290 482   0.184  1.0000
    ##  P4 - m6  -0.17386 0.290 482  -0.599  1.0000
    ##  P4 - M6  -0.17778 0.288 482  -0.616  1.0000
    ##  P4 - m7  -1.44444 0.288 482  -5.008  <.0001
    ##  P4 - M7  -1.71111 0.288 482  -5.933  <.0001
    ##  P4 - P8   0.57778 0.288 482   2.003  0.6910
    ##  A4 - P5   2.07563 0.290 482   7.153  <.0001
    ##  A4 - m6   1.84836 0.290 482   6.370  <.0001
    ##  A4 - M6   1.84444 0.288 482   6.395  <.0001
    ##  A4 - m7   0.57778 0.288 482   2.003  0.6910
    ##  A4 - M7   0.31111 0.288 482   1.079  0.9954
    ##  A4 - P8   2.60000 0.288 482   9.015  <.0001
    ##  P5 - m6  -0.22727 0.292 482  -0.779  0.9998
    ##  P5 - M6  -0.23119 0.290 482  -0.797  0.9997
    ##  P5 - m7  -1.49786 0.290 482  -5.162  <.0001
    ##  P5 - M7  -1.76452 0.290 482  -6.081  <.0001
    ##  P5 - P8   0.52437 0.290 482   1.807  0.8136
    ##  m6 - M6  -0.00392 0.290 482  -0.013  1.0000
    ##  m6 - m7  -1.27058 0.290 482  -4.379  0.0009
    ##  m6 - M7  -1.53725 0.290 482  -5.298  <.0001
    ##  m6 - P8   0.75164 0.290 482   2.590  0.2880
    ##  M6 - m7  -1.26667 0.288 482  -4.392  0.0008
    ##  M6 - M7  -1.53333 0.288 482  -5.317  <.0001
    ##  M6 - P8   0.75556 0.288 482   2.620  0.2715
    ##  m7 - M7  -0.26667 0.288 482  -0.925  0.9989
    ##  m7 - P8   2.02222 0.288 482   7.012  <.0001
    ##  M7 - P8   2.28889 0.288 482   7.936  <.0001
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: tukey method for comparing a family of 12 estimates

``` r
# 34/66 comparison significant at p<.05 level
```

## Visualise tension ratings of both groups

``` r
# take the means and 95% CIs
S <- df %>%
  group_by(Interval,Group2) %>%
  summarize(n=n(),M=mean(tensionrating,na.rm = TRUE),sd=sd(tensionrating,na.rm = TRUE)) %>%
  mutate(se=sd/sqrt(n),LCI=M+qnorm(0.025)*se,UCI=M+qnorm(0.975)*se)

dval<-0.33
colnames(S)[2] <- 'Expertise'

fig_india <- ggplot(S,aes(Interval,M,color=Expertise,group=Expertise,linestyle=Expertise,shape=Expertise))+
  geom_point(position=position_dodge(dval),size=4)+
  geom_line(position=position_dodge(dval))+
  geom_errorbar(aes(min=LCI, max=UCI),width=.2,position=position_dodge(dval)) +
  scale_y_continuous(breaks = seq(1,7,by=1),limits = c(1,7),expand = c(0.005,0.005))+
  scale_color_brewer(type = "div",palette = 'Set2')+
  scale_shape_manual(values=c(19,17))+  
  ylab('Tension Rating (M ± 95% CI)')+
  theme_bw()+
  theme(legend.position="bottom")+
  # Annotations take the statistics from analyses above
  annotate("text",x = 1,y=5.87,label='italic(p)<.001',parse = TRUE,size=3)+
  annotate("text",x = 4,y=5.87,label='italic(p)<.05',parse = TRUE,size=3)+
  annotate("text",x = 5,y=5.87,label='italic(p)<.001',parse = TRUE,size=3)+
  annotate("text",x = 7,y=5.87,label='italic(p)<0.01',parse = TRUE,size=3)+
  annotate("text",x = 8,y=5.87,label='italic(p)<.05',parse = TRUE,size=3)+
  annotate("text",x = 9,y=5.87,label='italic(p)<.001',parse = TRUE,size=3)+
  annotate("text",x = 11,y=5.87,label='italic(p)<.05',parse = TRUE,size=3)+
  annotate("text",x = 12,y=5.87,label='italic(p)<.01',parse = TRUE,size=3)+
  annotate("text",x = 1,y=6.5,label='Between group comparisons',size=4,hjust=0)+
  annotate("text",x = 1,y=2.5,label='Within group comparisons',size=4,hjust=0)+
  annotate("text",x = 1.5,y=2.0,label='Musicians:~34/66~intervals~differ~at~italic(p)<.05',color=RColorBrewer::brewer.pal(3, 'Set2')[1],size=3,hjust=0,parse=TRUE)+
  annotate("text",x = 1.5,y=1.5,label='Non-musicians:~7/66~intervals~differ~at~italic(p)<.05',color=RColorBrewer::brewer.pal(3, 'Set2')[2],size=3,hjust=0,parse=TRUE)+
  ggtitle('Indian samples (N=71)')
print(fig_india)
```

![Mean tension ratings for all intervals across expertise. Statistical
significance testing based on LMM analysis and multiple comparison
adjusted posthoc comparisons for between groups (upper part of the plot)
and within groups (lower part of the plot), see text for statistical
details.](README_files/figure-gfm/figure-1.png)

#### Session information

R version and libraries.

``` r
print(sessionInfo())
```

    ## R version 4.3.1 (2023-06-16)
    ## Platform: x86_64-apple-darwin20 (64-bit)
    ## Running under: macOS Ventura 13.6.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] effectsize_0.8.6  emmeans_1.8.9     lmerTest_3.1-3    lme4_1.1-34      
    ## [5] Matrix_1.6-1.1    papaja_0.1.1.9001 tinylabels_0.2.4  dplyr_1.1.4      
    ## [9] ggplot2_3.4.4    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyr_1.3.0         utf8_1.2.4          generics_0.1.3     
    ##  [4] lattice_0.22-5      digest_0.6.33       magrittr_2.0.3     
    ##  [7] RColorBrewer_1.1-3  evaluate_0.22       grid_4.3.1         
    ## [10] estimability_1.4.1  mvtnorm_1.2-3       fastmap_1.1.1      
    ## [13] backports_1.4.1     purrr_1.0.2         fansi_1.0.5        
    ## [16] scales_1.2.1        numDeriv_2016.8-1.1 cli_3.6.1          
    ## [19] rlang_1.1.2         munsell_0.5.0       splines_4.3.1      
    ## [22] withr_2.5.2         yaml_2.3.7          parallel_4.3.1     
    ## [25] pbkrtest_0.5.2      tools_4.3.1         datawizard_0.9.0   
    ## [28] nloptr_2.0.3        coda_0.19-4         minqa_1.2.6        
    ## [31] colorspace_2.1-0    bayestestR_0.13.1   boot_1.3-28.1      
    ## [34] broom_1.0.5         vctrs_0.6.5         R6_2.5.1           
    ## [37] lifecycle_1.0.4     MASS_7.3-60         insight_0.19.6     
    ## [40] pkgconfig_2.0.3     pillar_1.9.0        gtable_0.3.4       
    ## [43] glue_1.6.2          Rcpp_1.0.11         xfun_0.40          
    ## [46] tibble_3.2.1        tidyselect_1.2.0    rstudioapi_0.15.0  
    ## [49] parameters_0.21.2   knitr_1.44          farver_2.1.1       
    ## [52] xtable_1.8-4        htmltools_0.5.7     nlme_3.1-163       
    ## [55] rmarkdown_2.25      compiler_4.3.1
