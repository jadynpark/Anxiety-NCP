NCP\_Analysis\_Prelim
================
Jadyn Park
1/20/2021

**Research Questions:**  
**1.** Do implicit learning strategies differ between Low NCP and High
NCP?  
**2.** Are clinically relevant traits/symptoms (i.e.,
anxiety/impulsivity) related to the difference in group performance?  
**3.** Do relevant traits contribute to perseveration errors in implicit
learning tasks?

**Research Aims:**  
**Aim 1.** To test group differences in cognitive performance between
Low vs. High NCP  
**Aim 2.** To test if group performance is related to anxious/impulsive
symptoms  
**Aim 3.** To test if anxiety/impulsivity is correlated with
perseveration errors in implicit learning tasks

**Hypotheses:**  
**1.** Low NCPs perform better than high NCPs  
**2.** Greater the BAI (anxiety) & BAS (impulsivity) scores, worse the
performance in tasks  
**3.** Low NCPs perform better (i.e., perseverate less) than high NCPs
and anxious symptoms (potentially) moderate this relationship  
**3.1.** Alternatively, greater anxiety/impulsivity may only affect high
NCP but not low NCP

### This document displays preliminary results from NCP data.

``` r
#load libraries 
rm(list = ls())
library(knitr); library(kableExtra); library(reshape2); library(tidyverse); library(dplyr); library(effsize); library(tables); library(ggplot2); library(ggpubr); library(RColorBrewer); library(ez); source("~/Desktop/Anxiety NCP/summarySEwithin2.R"); library(wesanderson)

#import data - demographics, self report, and PCET
data <- read.csv("~/Desktop/Anxiety NCP/Anxiety_NCP_master.csv", header = TRUE)
```

### Demographics

``` r
# Demographics
demo.high <- data %>% filter(Group == "high") %>%
  dplyr::summarise(N = sum(!is.na(Subject.ID)),
            meanAge = mean(Age, na.rm = TRUE),
            sdAge = sd(Age, na.rm = TRUE),
            female = sum(Sex == "F"),
            male = sum(Sex == "M"),
            eastAsian = sum(Race == "east asian"),
            white = sum(Race == "white"),
            interracial = sum(Race == "interracial"),
            black = sum(Race == "black"),
  )
demo.low <- data %>% filter(Group == "low") %>%
  dplyr::summarise(N = sum(!is.na(Subject.ID)),
            meanAge = mean(Age, na.rm = TRUE),
            sdAge = sd(Age, na.rm = TRUE),
            female = sum(Sex == "F"),
            male = sum(Sex == "M"),
            eastAsian = sum(Race == "east asian"),
            white = sum(Race == "white"),
            interracial = sum(Race == "interracial"),
            black = sum(Race == "black"),
  )

demo.data <- rbind(demo.high, demo.low)
demo.data <- as.data.frame(t(demo.data))

names(demo.data) <- c("High NCP", "Low NCP") # column names
rownames(demo.data) <- c("N", "Mean", "SD", "Female", "Male", 
                           "East Asian", "White", "Interracial", "Black") # row names

knitr::kable(format(demo.data, digits = 4, drop0trailing = TRUE), caption = "Participant Demographics") %>%
  kable_styling(c("striped", full_width = F)) %>%
  pack_rows("Count", 1, 1) %>%
  pack_rows("Age", 2, 3) %>%
  pack_rows("Sex", 4, 5) %>%
  pack_rows("Race", 6, 9) 
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<caption>

Participant Demographics

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

High NCP

</th>

<th style="text-align:left;">

Low NCP

</th>

</tr>

</thead>

<tbody>

<tr grouplength="1">

<td colspan="3" style="border-bottom: 1px solid;">

<strong>Count</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

N

</td>

<td style="text-align:left;">

29

</td>

<td style="text-align:left;">

37

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="border-bottom: 1px solid;">

<strong>Age</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Mean

</td>

<td style="text-align:left;">

20.79

</td>

<td style="text-align:left;">

20.06

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

SD

</td>

<td style="text-align:left;">

1.95

</td>

<td style="text-align:left;">

1.97

</td>

</tr>

<tr grouplength="2">

<td colspan="3" style="border-bottom: 1px solid;">

<strong>Sex</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Female

</td>

<td style="text-align:left;">

22

</td>

<td style="text-align:left;">

26

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Male

</td>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

10

</td>

</tr>

<tr grouplength="4">

<td colspan="3" style="border-bottom: 1px solid;">

<strong>Race</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

East Asian

</td>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

4

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

White

</td>

<td style="text-align:left;">

7

</td>

<td style="text-align:left;">

22

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Interracial

</td>

<td style="text-align:left;">

3

</td>

<td style="text-align:left;">

3

</td>

</tr>

<tr>

<td style="text-align:left; padding-left: 2em;" indentlevel="1">

Black

</td>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

2

</td>

</tr>

</tbody>

</table>

### Let’s talk about sex distribution baby

Conducting Chi-square test to test the independence between two
variables – (1) Group (high vs. low), (2) Sex (M vs. F)

``` r
# Chi-square to test whether group is independent from sex
test <- chisq.test(table(data$Group, data$Sex))
X_squared = test[["statistic"]]; df = test[["parameter"]]; p.value = test[["p.value"]]; method = test[["method"]]
test.frame <- data.frame(X_squared, df, p.value, method)
row.names(test.frame) <- NULL

knitr::kable(format(test.frame, digits = 5, drop0trailing = TRUE), 
             caption = "p-value > .05, Group (high vs. low) and Sex (M vs. F) are independent") %>% 
             kable_styling(c("striped", "bordered", full_width = F))
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">

<caption>

p-value \> .05, Group (high vs. low) and Sex (M vs. F) are independent

</caption>

<thead>

<tr>

<th style="text-align:left;">

X\_squared

</th>

<th style="text-align:left;">

df

</th>

<th style="text-align:left;">

p.value

</th>

<th style="text-align:left;">

method

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

0.36906

</td>

<td style="text-align:left;">

2

</td>

<td style="text-align:left;">

0.8315

</td>

<td style="text-align:left;">

Pearson’s Chi-squared test

</td>

</tr>

</tbody>

</table>

``` r
# Chi-square to test whether Group is independent from sex
  # CAPE Positive Frequency scores are used to define NCP
  # 1st Q: 0-5, 2nd Q: 6-8, 3rd Q: 9-11, 4th Q: 12 and above
  # 9(Median) used as a cutoff for median split
  # Low NCP = 1st & 2nd Q, High NCP = 3rd & 4th
data$Quartile <- ifelse(data$Pos_Freq < 6, "1st",
                         ifelse(5<data$Pos_Freq & data$Pos_Freq<9, "2nd",
                          ifelse(8<data$Pos_Freq & data$Pos_Freq<12, "3rd",
                           ifelse(11<data$Pos_Freq, "4th", NA))))

data <- data[, c(1:5, 52, 6:51)] # reordering columns so that Quartile column is next to CAPE score

test2 <- chisq.test(table(data$Sex, data$Quartile))
X_squared = test2[["statistic"]]; df = test2[["parameter"]]; p.value = test2[["p.value"]]; method = test2[["method"]]
test2.frame <- data.frame(X_squared, df, p.value, method)
row.names(test2.frame) <- NULL

knitr::kable(format(test2.frame, digits = 5, drop0trailing = TRUE), 
             caption = "p-value > .05, Quartiles (1, 2, 3, 4)) and Sex (M vs. F) are independent") %>% 
             kable_styling(c("striped", "bordered", full_width = F))
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">

<caption>

p-value \> .05, Quartiles (1, 2, 3, 4)) and Sex (M vs. F) are
independent

</caption>

<thead>

<tr>

<th style="text-align:left;">

X\_squared

</th>

<th style="text-align:left;">

df

</th>

<th style="text-align:left;">

p.value

</th>

<th style="text-align:left;">

method

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

2.9604

</td>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

0.8138

</td>

<td style="text-align:left;">

Pearson’s Chi-squared test

</td>

</tr>

</tbody>

</table>

### Self Report Data

Preliminary data suggests CAPE positive frequency scores are positively
correlated with BAI scores

``` r
#Self Report data (CAPE positive & total, LSHS, BAI, BIS, BAS)
SR <- data %>% select(Subject.ID, Group, Pos_Freq, CAPE.sum, LSHS.sum, HPS.sum, BAI.sum, BIS.sum, BAS.fun, BAS.reward, BAS.drive, BAS.sum) 

# BAI-CAPE regression graph 
ggplot(SR, aes(x=Pos_Freq, y=BAI.sum, col = Group)) + 
 geom_point() +
  geom_smooth(method = lm , se = TRUE) +
   labs(title = "Correlation between CAPE score (Positive Frequency Only) and BAI score",
       x = "CAPE Positive Frequency score", y = "BAI score") +
  theme_classic() +
  stat_cor(method = "pearson", label.x = c(12, 3), label.y = c(30, 30)) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1"))
```

![](NCP_Analysis_Prelim_files/figure-gfm/selfreport-1.png)<!-- -->

``` r
# Linear Regression Summary 
low.NCP <- SR %>% filter(Group == "low")
summary(lm(low.NCP$BAI.sum~low.NCP$Pos_Freq)) # no significant relationship between BAI and CAPE
```

    ## 
    ## Call:
    ## lm(formula = low.NCP$BAI.sum ~ low.NCP$Pos_Freq)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -7.367 -5.866 -3.866  2.384 21.134 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)        7.2233     2.6946   2.681    0.012 *
    ## low.NCP$Pos_Freq   0.6429     0.5699   1.128    0.269  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.159 on 29 degrees of freedom
    ##   (6 observations deleted due to missingness)
    ## Multiple R-squared:  0.04203,    Adjusted R-squared:  0.009002 
    ## F-statistic: 1.273 on 1 and 29 DF,  p-value: 0.2685

``` r
high.NCP <- SR %>% filter(Group == "high")
summary(lm(high.NCP$BAI.sum~high.NCP$Pos_Freq)) # no significant relationship between BAI and CAPE
```

    ## 
    ## Call:
    ## lm(formula = high.NCP$BAI.sum ~ high.NCP$Pos_Freq)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -23.859 -10.335  -0.585   5.017  39.369 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)         -1.327     14.241  -0.093    0.927
    ## high.NCP$Pos_Freq    1.409      1.023   1.377    0.182
    ## 
    ## Residual standard error: 14.24 on 22 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.07938,    Adjusted R-squared:  0.03753 
    ## F-statistic: 1.897 on 1 and 22 DF,  p-value: 0.1823

### PCET (Penn Conditional Exclusion Task) Performance

Participants are given 48 trials to learn which characteristic (size,
shape, or line thickness) is determining the “odd man out”. They must
get 10 consecutive correct answers. After those correct answers, the
characteristic is changed. The PCET is scored based on a composite of
total correct responses and the number of rules/principles the
participant learned. Specifically, a performance composite score is
calculated by multiplying the number of principles learned (plus 1 to
accommodate those who do not learn a single rule) by proportion of
correct responses (i.e., correct responses/total responses). (Moore et
al., 2019)

![Penn Conditional Exclusion Task
Cues](/Users/Jadyn/Documents/GitHub/Anxiety-NCP/pcet_cue.png)

``` r
pcet <- data %>% select(Subject.ID, Group, PCET_NUM, PCETCR, PCETER, PCETRTCR, PCETRTER, PCET_CAT, CAT1_TR, CAT2_TR, CAT3_TR, PCET_EFF)

# PCET_NUM = total number of responses (correct + incorrect)
# PCETCR = total number of correct responses 
  # keep in mind that the rule changes only when the participant has answered correctly on 10 consecutive trials
  # if a participant makes a mistake, their CR will exceed 30
# PCETER = total number of incorrect responses
# PCET_CAT = number of categories (i.e., rules) achieved
  # if participant never got the rule, CAT = 0
# CAT1_TR = total number of trials (correct + incorrect) took to achieve 1st category
  # this will always include 10 consecutive successful trials
# CAT2_TR = same as CAT1 but for the 2nd category
# CAT3_TR = same as above but for the last category
  # if participant never got the rule, default is to complete 48 trials

pcet.melt <- melt(pcet, id.vars = c("Group", "Subject.ID"), variable_name="metric") #as a list
pcet.melt$value <- as.numeric(pcet.melt$value, na.rm = T) #value as numeric

pcet.fx <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
 return(data_sum)
} #function for SummaryWithin2 package -- calculates mean and sd

pcet.summary <- pcet.fx(pcet.melt, varname="value", groupnames=c("Group", "variable")) #summarized by quartile & variable (total, correct, incorrect)

# 1. Number of Correct, Incorrect, and Total Trials in High vs. Low NCP
trials <- c("PCET_NUM", "PCETCR", "PCETER")
pcet.group1 <- pcet.summary %>% filter(variable %in% trials)
pcet.group1$variable <- c("PCET_NUM" = "Total", "PCETCR" = "Correct", "PCETER" = "Incorrect")

# Graph of Correct, Incorrect, Total Trials By Group 
ggplot(pcet.group1, aes(x = variable, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd), width = .2,
                    position = position_dodge(.9)) +
  facet_grid(.~Group) +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  ggtitle("Mean # Correct and Incorrect Trials in High vs. Low NCP") + 
  xlab("Mean Total, Correct, and Incorrect Responses") +
  ylab("# Trials") +
  labs(fill = "Responses (Total, Correct, Incorrect)") +
  theme(
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line.x = element_line(color = "black", size = 0.4),
    axis.line.y = element_line(color = "black", size = 0.4)
  )
```

![](NCP_Analysis_Prelim_files/figure-gfm/pcet-1.png)<!-- -->

``` r
# 1.1 Significance Test
##### Total Responses #####
total.high <- subset(pcet.melt, Group == "high" & variable == "PCET_NUM")
total.low <- subset(pcet.melt, Group == "low" & variable == "PCET_NUM")
#var.test(total.high$value, total.low$value) ##Fisher's F-test, p<0.05; heterogeneous sample
total.t <- t.test(total.high$value, total.low$value, var.equal = FALSE) #two-sample t test, p = 0.0117; significant difference in total number of responses between high vs. low NCP
total.d <- cohen.d(total.high$value, total.low$value, na.rm = T)


##### Correct Responses #####
correct.high <- subset(pcet.melt, Group == "high" & variable == "PCETCR")
correct.low <- subset(pcet.melt, Group == "low" & variable == "PCETCR")
#var.test(correct.high$value, correct.low$value) #Fisher's F-test, p>0.05; both samples are homogeneous
correct.t <- t.test(correct.high$value, correct.low$value, var.equal = TRUE) #two-sample t-test, p = 0.4882; no significant difference in total number of correct responses between low vs. high NCP
correct.d <- cohen.d(correct.high$value, correct.low$value, na.rm = T)

##### Incorrect Responses #####
incorr.high <- subset(pcet.melt, Group == "high" & variable == "PCETER")
incorr.low <- subset(pcet.melt, Group == "low" & variable == "PCETER")
#var.test(incorr.high$value, incorr.low$value) #Fisher's F-test, p>0.05; both samples are homogeneous
incorr.t <- t.test(incorr.high$value, incorr.low$value, var.equal = TRUE) #two-sample t-test, p = 0.0069; significant difference in total number of incorrect responses between low vs. high NCP
incorr.d <- cohen.d(incorr.high$value, incorr.low$value, na.rm = T)


# 2. RT of Correct and Incorrect Trials in High vs. Low NCP
reac <- c("PCETRTCR", "PCETRTER")
pcet.group2 <- pcet.summary %>% filter(variable %in% reac)
pcet.group2$variable <- c("PCETRTCR" = "RT Correct", "PCETRTER" = "RT Incorrect")

# Graph of Correct and Incorrect Trials by Group
ggplot(pcet.group2, aes(x = variable, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd), width = .2,
                    position = position_dodge(.9)) +
  facet_grid(.~Group) +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  ggtitle("RT of Correct and Incorrect Trials in High vs. Low NCP") + 
  xlab("Mean Correct, and Incorrect Responses") +
  ylab("RT (in ms)") +
  labs(fill = "Responses (Correct, Incorrect)") +
  theme(
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line.x = element_line(color = "black", size = 0.4),
    axis.line.y = element_line(color = "black", size = 0.4)
  )
```

![](NCP_Analysis_Prelim_files/figure-gfm/pcet-2.png)<!-- -->

``` r
# 2.1 Significance Test
##### Correct Responses #####
correct.high.rt <- subset(pcet.melt, Group == "high" & variable == "PCETRTCR")
correct.low.rt <- subset(pcet.melt, Group == "low" & variable == "PCETRTCR")
#var.test(correct.high.rt$value, correct.low.rt$value) #Fisher's F-test, p<0.05; heterogeneous sample
cor.rt.t <- t.test(correct.high.rt$value, correct.low.rt$value, var.equal = FALSE) #Welch t-statistic, p = 0.1104; no significant difference in RT of correct responses between low vs. high NCP 
cor.rt.d <- cohen.d(correct.high.rt$value, correct.low.rt$value, na.rm = T)

##### Incorrect Responses #####
incorr.high.rt <- subset(pcet.melt, Group == "high" & variable == "PCETRTER")
incorr.low.rt <- subset(pcet.melt, Group == "low" & variable == "PCETRTER")
#var.test(incorr.high.rt$value, incorr.low.rt$value) #Fisher's F-test, p<0.05; heterogeneous sample
incor.rt.t <- t.test(incorr.high.rt$value, incorr.low.rt$value, var.equal = FALSE) #Welch t-statistic, p = 0.0557; no significant difference in RT of incorrect responses between low vs. high NCP 
incor.rt.d <- cohen.d(incorr.high.rt$value, incorr.low.rt$value, na.rm = T)

# 3. Summary Table
high.perf <- c(pcet.group1[2,3], pcet.group1[3,3], pcet.group1[1,3], pcet.group2[1,3], pcet.group2[2,3])
low.perf <- c(pcet.group1[5,3], pcet.group1[4,3], pcet.group1[6,3], pcet.group2[3,3], pcet.group2[4,3])
p.perf <- c(correct.t[["p.value"]], incorr.t[["p.value"]], total.t[["p.value"]], 
            cor.rt.t[["p.value"]], incor.rt.t[["p.value"]])
d.perf <- c(correct.d[["estimate"]], incorr.d[["estimate"]], total.d[["estimate"]], 
            cor.rt.d[["estimate"]], incor.rt.d[["estimate"]])
pcet.sum <- data.frame(high.perf, low.perf, p.perf, d.perf)
pcet.sum <- as.data.frame(t(pcet.sum))
rownames(pcet.sum) <- c("High NCP", "Low NCP", "p-value", "Cohen's D") 
colnames(pcet.sum) <- c("Correct", "Incorrect", "Total", "Correct RT", "Incorrect RT")

knitr::kable(format(pcet.sum, digits = 3, drop0trailing = TRUE, scientific = F), caption = "PCET Performance Summary") %>%
  kable_styling(c("striped", "bordered", full_width = F))
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">

<caption>

PCET Performance Summary

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Correct

</th>

<th style="text-align:left;">

Incorrect

</th>

<th style="text-align:left;">

Total

</th>

<th style="text-align:left;">

Correct RT

</th>

<th style="text-align:left;">

Incorrect RT

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

High NCP

</td>

<td style="text-align:left;">

35.333

</td>

<td style="text-align:left;">

28.33333

</td>

<td style="text-align:left;">

63.6667

</td>

<td style="text-align:left;">

1834.708

</td>

<td style="text-align:left;">

2680.5417

</td>

</tr>

<tr>

<td style="text-align:left;">

Low NCP

</td>

<td style="text-align:left;">

34.242

</td>

<td style="text-align:left;">

50.93939

</td>

<td style="text-align:left;">

16.697

</td>

<td style="text-align:left;">

1576.591

</td>

<td style="text-align:left;">

2099.9697

</td>

</tr>

<tr>

<td style="text-align:left;">

p-value

</td>

<td style="text-align:left;">

0.488

</td>

<td style="text-align:left;">

0.00694

</td>

<td style="text-align:left;">

0.0117

</td>

<td style="text-align:left;">

0.11

</td>

<td style="text-align:left;">

0.0557

</td>

</tr>

<tr>

<td style="text-align:left;">

Cohen’s D

</td>

<td style="text-align:left;">

0.187

</td>

<td style="text-align:left;">

0.75248

</td>

<td style="text-align:left;">

0.758

</td>

<td style="text-align:left;">

0.487

</td>

<td style="text-align:left;">

0.575

</td>

</tr>

</tbody>

</table>

### Performance based on Accuracy, Efficiency, and Composite Score

``` r
# 1. Accuracy (% Correct)
pcet <- pcet %>% mutate(acc = PCETCR/PCET_NUM)
pcet.acc <- melt(pcet, id.vars = c("Group", "Subject.ID"), variable_name="metric")
pcet.acc <- pcet.acc %>% filter(variable == "acc")

# N, mean, and SD of high-NCP
h.acc<- pcet.acc %>% filter(Group == "high") %>% summarise(n = sum(variable=="acc"),
                                                            mean = mean(as.numeric(value), na.rm = T),
                                                            sd = sd(as.numeric(value), na.rm = T)) 

# N, mean, and SD of low-NCP
l.acc <- pcet.acc %>% filter(Group == "low") %>% summarise(n = sum(variable=="acc"),
                                                           mean = mean(as.numeric(value), na.rm = T),
                                                           sd = sd(as.numeric(value), na.rm = T)) 

# Density Plot of Accuracy by Group
# high.acc <- data.frame(hf = rnorm(h.acc$n, h.acc$mean, h.acc$sd)) # normalized
# low.acc <- data.frame(lf = rnorm(l.acc$n, l.acc$mean, l.acc$sd))
#ggplot() +
#  geom_density(data = high.acc, aes(x = hf), 
#               fill = "#145A32", color = "black", alpha = .5) + 
#  geom_density(data = low.acc, aes(x = lf), 
#               fill = "#F7DC6F", color = "black", alpha = .5) + 
#  theme_classic() +
#  ggtitle("Accuracy (% Correct) in high vs. low NCP") +
#  xlab("Accuracy (% Correct)") 
  
# Box Plot of Accuracy by Group
ggplot(pcet.acc, aes(x=variable, y=as.numeric(value), fill = Group), na.rm = TRUE) +
  geom_boxplot(position = position_dodge()) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, position = position_dodge()) +
  theme_classic() +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  labs(title = "Accuracy by Group", y = "Accuracy")
```

![](NCP_Analysis_Prelim_files/figure-gfm/perf-1.png)<!-- -->

``` r
# Significance Test of Accuracy by Group
acc.high <- subset(pcet.acc, Group == "high")
acc.low <- subset(pcet.acc, Group == "low")
#var.test(as.numeric(acc.high$value), as.numeric(acc.low$value)) #Fisher's F-test, p>0.05; homogeneous sample
acc.t <- t.test(as.numeric(acc.high$value), as.numeric(acc.low$value, var.equal = TRUE)) # p = 0.020
acc.d <- cohen.d(acc.high$value, acc.low$value, na.rm = T)


# 2. Efficiency
pcet.eff <- pcet.melt %>% filter(variable=="PCET_EFF")

# N, mean, and SD of high-NCP
h.eff <- pcet.eff %>% filter(Group == "high") %>% summarise(n = sum(variable=="PCET_EFF"),
                                                            mean = mean(as.numeric(value), na.rm = T),
                                                            sd = sd(as.numeric(value), na.rm = T)) 
# N, mean, and SD of low-NCP
l.eff <- pcet.eff %>% filter(Group == "low") %>% summarise(n = sum(variable=="PCET_EFF"),
                                                           mean = mean(as.numeric(value), na.rm = T),
                                                           sd = sd(as.numeric(value), na.rm = T)) 

# Density Plot of Efficiency By Group
# high.eff <- data.frame(he = rnorm(h.eff$n, h.eff$mean, h.eff$sd)) # normalized
# low.eff <- data.frame(le = rnorm(l.eff$n, l.eff$mean, l.eff$sd))
#ggplot() +
#  geom_density(data = high.eff, aes(x = he),
#               fill = "#145A32", color = "black", alpha = .5) +
#  geom_density(data = low.eff, aes(x = le),
#               fill = "#F7DC6F", color = "black", alpha = .5) +
#  theme_classic() +
#  ggtitle("Efficiency in high vs. low NCP") +
#  xlab("Efficiency") 

# Box Plot of Efficiency by Group 
ggplot(pcet.eff, aes(x=variable, y=as.numeric(value), fill = Group), na.rm = TRUE) +
  geom_boxplot(position = position_dodge()) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, position = position_dodge()) +
  theme_classic() +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  labs(title = "Efficiency by Group", y = "Efficiency")
```

![](NCP_Analysis_Prelim_files/figure-gfm/perf-2.png)<!-- -->

``` r
# Significance Test of Efficiency by Group 
eff.high <- subset(pcet.eff, Group == "high")
eff.low <- subset(pcet.eff, Group == "low")
#var.test(eff.high$value, eff.low$value) #Fisher's F-test, p>0.05; homogeneous sample
eff.t <- t.test(eff.high$value, eff.low$value, var.equal = TRUE) #p=0.0123
eff.d <- cohen.d(eff.high$value, eff.low$value, na.rm = T)

# 3. Composite score
pcet <- pcet %>% mutate(score = PCET_CAT * acc)
pcet.score <- melt(pcet, id.vars = c("Group", "Subject.ID"), variable_name="metric") 
pcet.score <- pcet.score %>% filter(variable == "score") 

# N, mean, and SD of high-NCP
h.sc <- pcet.score %>% filter(Group == "high") %>% summarise(n = sum(variable=="score"),
                                                            mean = mean(as.numeric(value), na.rm = T),
                                                            sd = sd(as.numeric(value), na.rm = T))

# N, mean, and SD of low-NCP
l.sc <- pcet.score %>% filter(Group == "low") %>% summarise(n = sum(variable=="score"),
                                                           mean = mean(as.numeric(value), na.rm = T),
                                                           sd = sd(as.numeric(value), na.rm = T)) 

# Density Plot of Composite Score by Group
# high.score <- data.frame(hs = rnorm(h.sc$n, h.sc$mean, h.sc$sd)) # normalized
# low.score <- data.frame(ls = rnorm(l.sc$n, l.sc$mean, l.sc$sd))
#ggplot() +
#  geom_density(data = high.score, aes(x = hs),
#               fill = "#145A32", color = "black", alpha = .5) +
#  geom_density(data = low.score, aes(x = ls),
#               fill = "#F7DC6F", color = "black", alpha = .5) +
#  theme_classic() +
#  ggtitle("Composite Score in high vs. low NCP") +
#  xlab("Score") 

# Box Plot of Composite Score by Group
ggplot(pcet.score, aes(x=variable, y=as.numeric(value), fill = Group), na.rm = TRUE) +
  geom_boxplot(position = position_dodge()) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, position = position_dodge()) +
  theme_classic() +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  labs(title = "Composite Score by Group", y = "Score")
```

![](NCP_Analysis_Prelim_files/figure-gfm/perf-3.png)<!-- -->

``` r
# Significance Test of Composite Score by Group
score.high <- subset(pcet.score, Group == "high")
score.low <- subset(pcet.score, Group == "low")
#var.test(as.numeric(score.high$value), as.numeric(score.low$value)) #homogeneous
score.t <- t.test(as.numeric(score.high$value), as.numeric(score.low$value), var.equal=T) #p=0.014
score.d <- cohen.d(score.high$value, score.low$value, na.rm = T)

# Summary Table
h.acc.sum = paste(round(h.acc['mean'], 2), "(" , round(h.acc['sd'], 2), ")", sep = "")
h.eff.sum = paste(round(h.eff['mean'], 2), "(" , round(h.eff['sd'], 2), ")", sep = "")
h.sc.sum = paste(round(h.sc['mean'], 2), "(" , round(h.sc['sd'], 2), ")", sep = "")
l.acc.sum = paste(round(l.acc['mean'], 2), "(" , round(l.acc['sd'], 2), ")", sep = "")
l.eff.sum = paste(round(l.eff['mean'], 2), "(" , round(l.eff['sd'], 2), ")", sep = "")
l.sc.sum = paste(round(l.sc['mean'], 2), "(" , round(l.sc['sd'], 2), ")", sep = "")

high.summary <- cbind(h.acc.sum, h.eff.sum, h.sc.sum)
rownames(high.summary) <- "High NCP"
pval <- round(cbind(acc.t[["p.value"]], eff.t[["p.value"]], score.t[["p.value"]]),3)
rownames(pval) <- "p-value"
low.summary <- cbind(l.acc.sum, l.eff.sum, l.sc.sum)
rownames(low.summary) <- "Low NCP"
cohensd <- abs(round(cbind(acc.d[["estimate"]], eff.d[["estimate"]], score.d[["estimate"]]), 3))
rownames(cohensd) <- "Cohen's D"
performance.table <- data.frame(rbind(high.summary, low.summary, pval, cohensd))
colnames(performance.table) <- c("Accuracy", "Efficiency", "Score")

knitr::kable(performance.table,caption = "Mean (SD) of Accuracy, Efficiency, and Composite Score Summary") %>%
  kable_styling(c("striped", "bordered", full_width = F)) 
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">

<caption>

Mean (SD) of Accuracy, Efficiency, and Composite Score Summary

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Accuracy

</th>

<th style="text-align:left;">

Efficiency

</th>

<th style="text-align:left;">

Score

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

High NCP

</td>

<td style="text-align:left;">

0.6(0.17)

</td>

<td style="text-align:left;">

0.21(0.1)

</td>

<td style="text-align:left;">

1.58(0.75)

</td>

</tr>

<tr>

<td style="text-align:left;">

Low NCP

</td>

<td style="text-align:left;">

0.71(0.16)

</td>

<td style="text-align:left;">

0.28(0.08)

</td>

<td style="text-align:left;">

2.04(0.61)

</td>

</tr>

<tr>

<td style="text-align:left;">

p-value

</td>

<td style="text-align:left;">

0.02

</td>

<td style="text-align:left;">

0.012

</td>

<td style="text-align:left;">

0.013

</td>

</tr>

<tr>

<td style="text-align:left;">

Cohen’s D

</td>

<td style="text-align:left;">

0.658

</td>

<td style="text-align:left;">

0.694

</td>

<td style="text-align:left;">

0.686

</td>

</tr>

</tbody>

</table>

### PCET Performance by Block

``` r
pcet <- pcet %>% filter(PCET_CAT == 3) # excluding incomplete subjects (ie category achieved <3)
pcet.trials <- melt(pcet, id.vars = c("Group", "Subject.ID"), variable_name = "metric") # individual trials data
blocks <- c("CAT1_TR", "CAT2_TR", "CAT3_TR")
pcet.trials <- pcet.trials %>% filter(variable %in% blocks) # filter out irrelevant variables
pcet.trials$value <- as.numeric(pcet.trials$value, na.rm = T)
pcet.trials$Subject.ID <- as.factor(pcet.trials$Subject.ID) 

pcet.blocks <- pcet.trials %>% dplyr::group_by(Group, variable) %>%
  dplyr::summarise(value = mean(value, na.rm = T)) # group means data
```

#### Number of trials to achieve category as a proxy for rule learning. Individuals must choose the correct object on 10 consecutive trials to “achieve category”. Once it is achieved, new rule is in effect

``` r
# Individual and Group Trajectory
ggplot(pcet.trials, aes(x = variable, y = value, color = Group)) +
  geom_line(aes(group = Subject.ID), alpha = .3) +
  geom_line(data = pcet.blocks, aes(group = Group), alpha = .9, size = 2) +
  theme_classic() +
  scale_color_manual(values = wes_palette(n=2, name="Chevalier1")) +
  labs(
    title = "Individual and Group Trajectories in Rule Learning",
    x = NULL,
    y = "# of Trials",
    color = NULL
  )
```

![](NCP_Analysis_Prelim_files/figure-gfm/trajectory-1.png)<!-- -->

### PCET & Clinical Traits

``` r
data <- data %>% mutate(acc = as.numeric(PCETCR/PCET_NUM), score = as.numeric(PCET_CAT * acc))

# Group x Accuracy Correlation 
ggplot(data, aes(x = Pos_Freq, y = acc, col = Group)) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "Correlation between CAPE positive score and Accuracy by Group",
       x = "CAPE Positive Score", y = "Accuracy") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/penntraits-1.png)<!-- -->

``` r
# Group x Efficiency Correlation 
ggplot(data, aes(x = Pos_Freq, y = PCET_EFF, col = Group)) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "Correlation between CAPE positive score and Efficiency by Group",
       x = "CAPE Positive Score", y = "Efficiency") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/penntraits-2.png)<!-- -->

``` r
# Group x Composite Score Correlation 
ggplot(data, aes(x = Pos_Freq, y = score, col = Group)) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "Correlation between CAPE positive score and Composite Score by Group",
       x = "CAPE Positive Score", y = "PCET Score") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/penntraits-3.png)<!-- -->

``` r
# Anxiety x Accuracy Correlation by Group
ggplot(data, aes(x = BAI.sum, y = acc, col = Group), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "Correlation between BAI score and Accuracy", x = "BAI score", y = "Accuracy") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/penntraits-4.png)<!-- -->

``` r
# Does Group x Anxiety Interaction predict Accuracy?
  # X = Anxiety Score, # D = Group (1 = high NCP, 0 = low NCP) # Y = Accuracy
  # Yi = B0 + B1Xi + B2Xi + ui
interac <- data %>% select(acc, BAI.sum, Group)
summary(lm(data = interac, acc ~ BAI.sum*Group))
```

    ## 
    ## Call:
    ## lm(formula = acc ~ BAI.sum * Group, data = interac)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.36202 -0.13048  0.02811  0.09604  0.27318 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.500528   0.057719   8.672 4.47e-11 ***
    ## BAI.sum           0.005300   0.002379   2.228   0.0311 *  
    ## Grouplow          0.263472   0.074746   3.525   0.0010 ** 
    ## BAI.sum:Grouplow -0.010363   0.004467  -2.320   0.0250 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1585 on 44 degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## Multiple R-squared:  0.2233, Adjusted R-squared:  0.1703 
    ## F-statistic: 4.216 on 3 and 44 DF,  p-value: 0.0105

``` r
# anxiety score significantly predicted performance
# NCP status even more significantly predicted performance
# interaction signiicantly predicted performacne

# Anxiety x Efficiency Correlation by Group
ggplot(data, aes(x = BAI.sum, y = PCET_EFF, col = Group), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "Correlation between BAI score and Efficiency", x = "BAI score", y = "Efficiency") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/penntraits-5.png)<!-- -->

``` r
# Does Group x Anxiety Interaction predict Efficiency?
interac1 <- data %>% select(PCET_EFF, BAI.sum, Group)
summary(lm(data = interac1, PCET_EFF ~ BAI.sum*Group))
```

    ## 
    ## Call:
    ## lm(formula = PCET_EFF ~ BAI.sum * Group, data = interac1)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.18753 -0.06787  0.02841  0.06573  0.15829 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.153017   0.032776   4.669 2.86e-05 ***
    ## BAI.sum           0.003056   0.001351   2.262 0.028699 *  
    ## Grouplow          0.155203   0.042444   3.657 0.000679 ***
    ## BAI.sum:Grouplow -0.005845   0.002536  -2.304 0.025978 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08999 on 44 degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## Multiple R-squared:  0.2382, Adjusted R-squared:  0.1863 
    ## F-statistic: 4.587 on 3 and 44 DF,  p-value: 0.007031

``` r
# Anxiey x Score Correlation by Group 
ggplot(data, aes(x = BAI.sum, y = score, col = Group), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "Correlation between BAI score and Composite Score", x = "BAI score", y = "Composite Score") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/penntraits-6.png)<!-- -->

``` r
# Does Group x Anxiety Interaction predict Composite Score?
interac2 <- data %>% select(score, BAI.sum, Group)
summary(lm(data = interac2, score ~ BAI.sum*Group))
```

    ## 
    ## Call:
    ## lm(formula = score ~ BAI.sum * Group, data = interac2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3532 -0.5146  0.1394  0.4118  1.1244 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       1.160502   0.237541   4.885 1.41e-05 ***
    ## BAI.sum           0.021076   0.009791   2.153 0.036881 *  
    ## Grouplow          1.091805   0.307614   3.549 0.000933 ***
    ## BAI.sum:Grouplow -0.040721   0.018383  -2.215 0.031970 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6522 on 44 degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## Multiple R-squared:  0.228,  Adjusted R-squared:  0.1753 
    ## F-statistic: 4.331 on 3 and 44 DF,  p-value: 0.009264

### PCET & Clinical Traits on Box Plot

``` r
# Mean Anxiety Scores for Each Group
anx.means <- data %>% select(Group, BAI.sum) %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(mean.anx = mean(BAI.sum, na.rm = T), sd.anx = sd(BAI.sum, na.rm = T))

# BAI score by Group (above or below average)
anx.data <- data %>% select(Subject.ID, Group, BAI.sum)

anx.data$anxiety <- ifelse(anx.data$Group == "high" & anx.data$BAI.sum > 17.9, "above",
 ifelse(anx.data$Group == "low" & anx.data$BAI.sum > 9.77, "above",
  ifelse(anx.data$Group == "high" & anx.data$BAI.sum < 17.9, "below", "below"))) # 'above' or 'below' mean anxiety score of each group, mean value is hard-coded

##### 1. Accuracy #####
pcet.acc <- merge(pcet.acc, anx.data, by = "Subject.ID")

pcet.acc %>%
  filter(!is.na(anxiety)) %>%
  mutate(Group.x = ifelse(Group.x == "high", "High NCP", "Low NCP"),
         anxiety = ifelse(anxiety == "above", "High Anx", "Low Anx"),
         Interaction = factor(str_replace(interaction(Group.x, anxiety),
                                          '\\.', '&'),
                              ordered=TRUE)) %>%
  ggplot(aes(x=Interaction, y=value)) +
  geom_boxplot(aes(fill=Group.x)) +
  geom_point(aes(shape=anxiety), size = 2) +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  labs(title="Integrative Effects of Anxiety and Psychosis on Accuracy", y="Accuracy") +
  theme_classic(base_size=10) +
  theme(axis.title.x=element_blank())
```

![](NCP_Analysis_Prelim_files/figure-gfm/interacbox-1.png)<!-- -->

``` r
##### 2. Efficiency #####
pcet.eff <- merge(pcet.eff, anx.data, by = "Subject.ID")

pcet.eff %>%
  filter(!is.na(anxiety)) %>%
  mutate(Group.x = ifelse(Group.x == "high", "High NCP", "Low NCP"),
         anxiety = ifelse(anxiety == "above", "High Anx", "Low Anx"),
         Interaction = factor(str_replace(interaction(Group.x, anxiety),
                                          '\\.', '&'),
                              ordered=TRUE)) %>%
  ggplot(aes(x=Interaction, y=value)) +
  geom_boxplot(aes(fill=Group.x)) +
  geom_point(aes(shape=anxiety), size = 2) +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  labs(title="Integrative Effects of Anxiety and Psychosis on Efficiency", y="Efficiency") +
  theme_classic(base_size=10) +
  theme(axis.title.x=element_blank())
```

![](NCP_Analysis_Prelim_files/figure-gfm/interacbox-2.png)<!-- -->

``` r
##### 3. Score #####
pcet.score <- merge(pcet.score, anx.data, by = "Subject.ID")

pcet.score %>%
  filter(!is.na(anxiety)) %>%
  mutate(Group.x = ifelse(Group.x == "high", "High NCP", "Low NCP"),
         anxiety = ifelse(anxiety == "above", "High Anx", "Low Anx"),
         Interaction = factor(str_replace(interaction(Group.x, anxiety),
                                          '\\.', '&'),
                              ordered=TRUE)) %>%
  ggplot(aes(x=Interaction, y=value)) +
  geom_boxplot(aes(fill=Group.x)) +
  geom_point(aes(shape=anxiety), size = 2) +
  scale_fill_manual(values = wes_palette(n=3, name = "Chevalier1")) +
  labs(title="Integrative Effects of Anxiety and Psychosis on Score", y="PCET Score") +
  theme_classic(base_size=10) +
  theme(axis.title.x=element_blank())
```

![](NCP_Analysis_Prelim_files/figure-gfm/interacbox-3.png)<!-- -->

### High- vs. Low- Performers

Two clusters, especially among the high-NCP group, in Efficiency and
Score. Compare two groups (low = below mean, high = above mean) by
Anxiety scores.

``` r
# 1. Accuracy
accuracy <- data.frame(data$Subject.ID, data$Group, data$BAI.sum, data$acc)
accuracy$performance <- ifelse(accuracy$data.Group == "high" & accuracy$data.acc > h.acc$mean, "Above", 
                             ifelse(accuracy$data.Group == "low" & accuracy$data.acc > l.acc$mean, "Above",
                                    ifelse(accuracy$data.Group == "high" & accuracy$data.acc < h.acc$mean, "Below", "Below")))

# Above and Below Average Performers among High-NCP Group
acc_high <- accuracy %>% filter(data.Group == "high") %>% drop_na()
ggplot(acc_high, aes(x = data.BAI.sum, y = data.acc, col = performance), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Accuracy Correlation of Above vs. Below Average Performers among High-NCP Group", 
       x = "BAI score", y = "Accuracy") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-1.png)<!-- -->

``` r
# Above and Below Average Performers among Low-NCP Group
acc_low <- accuracy %>% filter(data.Group == "low") %>% drop_na()
ggplot(acc_low, aes(x = data.BAI.sum, y = data.acc, col = performance), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Accuracy Correlation of Above vs. Below Average Performers among Low-NCP Group", 
       x = "BAI score", y = "Accuracy") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-2.png)<!-- -->

``` r
# 2. Efficiency
efficiency <- data.frame(data$Subject.ID, data$Group, data$BAI.sum, data$PCET_EFF)
efficiency$performance <- ifelse(efficiency$data.Group == "high" & efficiency$data.PCET_EFF > h.eff$mean, "Above", 
                             ifelse(efficiency$data.Group == "low" & efficiency$data.PCET_EFF > l.eff$mean, "Above",
                                    ifelse(efficiency$data.Group == "high" & efficiency$data.PCET_EFF < h.eff$mean, "Below", "Below")))

# Above and Below Average Performers among High-NCP Group
eff_high <- efficiency %>% filter(data.Group == "high") %>% drop_na()
ggplot(eff_high, aes(x = data.BAI.sum, y = data.PCET_EFF, col = performance), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Efficiency Correlation of Above vs. Below Average Performers among High-NCP Group", 
       x = "BAI score", y = "Efficiency") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-3.png)<!-- -->

``` r
# Above and Below Average Performers among Low-NCP Group
eff_low <- efficiency %>% filter(data.Group == "low") %>% drop_na()
ggplot(eff_low, aes(x = data.BAI.sum, y = data.PCET_EFF, col = performance), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Efficiency Correlation of Above vs. Below Average Performers among Low-NCP Group", 
       x = "BAI score", y = "Efficiency") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-4.png)<!-- -->

``` r
# 3. Score
score <- data.frame(data$Subject.ID, data$Group, data$BAI.sum, data$score)
score$performance <- ifelse(score$data.Group == "high" & score$data.score > h.sc$mean, "Above", 
                             ifelse(score$data.Group == "low" & score$data.score > l.sc$mean, "Above",
                                    ifelse(score$data.Group == "high" & score$data.score < h.sc$mean, "Below", "Below")))

# Above and Below Average Performers among High-NCP Group
sc_high <- score %>% filter(data.Group == "high") %>% drop_na()
ggplot(sc_high, aes(x = data.BAI.sum, y = data.score, col = performance), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Score Correlation of Above vs. Below Average Performers among High-NCP Group", 
       x = "BAI score", y = "Score") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-5.png)<!-- -->

``` r
# Above and Below Average Performers among Low-NCP Group
sc_low <- score %>% filter(data.Group == "low") %>% drop_na()
ggplot(sc_low, aes(x = data.BAI.sum, y = data.score, col = performance), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Score Correlation of Above vs. Below Average Performers among Low-NCP Group", 
       x = "BAI score", y = "Score") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-6.png)<!-- -->

``` r
# 4. RT -- below average is faster
# 4.1 Correct Responses RT 
RT <- data.frame(data$Subject.ID, data$Group, data$BAI.sum, data$PCETRTER)
RT$speed <- ifelse(RT$data.Group == "high" & RT$data.PCETRTER > mean(incorr.high.rt$value, na.rm = T), "Above", 
                    ifelse(RT$data.Group == "low" & RT$data.PCETRTER > mean(incorr.low.rt$value, na.rm = T), "Above",
                      ifelse(RT$data.Group == "high" & RT$data.PCETRTER < mean(incorr.high.rt$value, na.rm = T), "Below", "Below")))

# Above and Below Average Performers among High-NCP Group
RT_high <- RT %>% filter(data.Group == "high") %>% drop_na()
ggplot(RT_high, aes(x = data.BAI.sum, y = data.PCETRTER, col = speed), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Incorrect RT Correlation of Above vs. Below Average Performers among High-NCP Group", 
       x = "BAI score", y = "Incorrect RT") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-7.png)<!-- -->

``` r
# Above and Below Average Performers among Low-NCP Group
RT_low <- RT %>% filter(data.Group == "low") %>% drop_na()
ggplot(RT_low, aes(x = data.BAI.sum, y = data.PCETRTER, col = speed), na.rm = T) + geom_point() +
  geom_smooth(method = lm , se = F) +
  scale_color_manual(values = wes_palette(n=2, name = "Chevalier1")) +
  labs(title = "BAI-Incorrect RT Correlation of Above vs. Below Average Performers among Low-NCP Group", 
       x = "BAI score", y = "Incorrect RT") +
  theme_classic() +
  stat_cor(label.x = 15)
```

![](NCP_Analysis_Prelim_files/figure-gfm/learn-8.png)<!-- -->
