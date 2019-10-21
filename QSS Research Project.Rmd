---
title: "Donor Tendencies at Dartmouth College"
output:
  pdf_document: default
  html_notebook: default
linestretch: 1.5
---
Research Question: Are Dartmouth students more willing to donate to charities with greater recognition? Furthermore, are students more willing to donate to a cause with a personal story than to a cause without a personal story? 

#Introduction:

Many students at Dartmouth are heavily involved with supporting charities and local organizations, frequently sending out emails and setting up tables to raise money. Given that charities are a large part of the lives of students, we ask the following questions: are Dartmouth students more willing to donate to charities with greater recognition? Furthermore, are students more willing to donate to causes with a personal story than to causes without? By studying and answering these questions, we hope to gain a better understanding of any donating tendencies Dartmouth students may have.

These questions are interesting primarily due to their practical applications. The data we gather and interpret through our project could depict the influence of brand power or personal stories on Dartmouth students’ tendency to donate. Utilizing this research could allow students to create more marketable and successful campaigns in the future. Furthermore, this question is also interesting to us on a personal level, as all of us have an interest in philanthropy and helping out the community. After gathering all the data we need, we plan on donating all of the proceeds we raise to the respective charity.


##Variables: 

Our variables were having a UNICEF (a famous, global charity) logo/description and having Amir Bonjam’s story (a personal story). 

We set up a two-way table with the following sample space: No UNICEF association with a personal story, UNICEF association with a personal story, no UNICEF association without a personal story, UNICEF association without a personal story.


#Data Collection: 

##Randomization Process: 
We had four different handouts - all of them had the same description of the purpose of the fundraising and a picture of our child Amir Bomjan. The only difference between the handouts was that two had the name of a well-known charity, UNICEF, in the title, as well as a picture of the logo and a small description of what UNICEF’s disability sector supports. Furthermore, we conditioned, one handout with UNICEF  and one without UNICEF by adding a paragraph with a specific touching story related to the purpose of the fundraiser. For this story, we went on the UNICEF website and looked up the name of one of the featured children on their website. From there, we found an article about this child, Amir Bomjan, and created a short paragraph with the details of his story. The lengths of the paragraphs on each of the handouts were very similar and the same pictures were used. 

![Figure](/Users/eileenxia/Desktop/unicef.jpg) ![Figure](/Users/eileenxia/Desktop/handouts.jpg)

With four different handouts according to each of the conditions, we randomly shuffled the handouts to ensure that each participant receives a randomly selected condition. The person handing out gave the participant a randomly selected handout from the pile. The person handing out did not know which type of condition was being applied to the participant since the handouts were single-side printed and the pile was stacked content side down. Furthermore, the individual should not look at the paper after it is handed out to ensure the process is double-blind.

On a randomly chosen day, we set up a table outside of Novack, a popular site for fundraising, and handed people the sheets, performing the aforementioned experiment. Students who responded chose whether they wanted to donate or not - if they did, they wrote down their DA$H number and a specified amount (or donated cash). Each time an individual was handed a sheet, we recorded how much they donated on that sheet. 

In short,
1. The treatments were randomized by shuffling the four types of sheets, ensuring they were mixed in no particular order.
2. The data collection was double-blind. The person receiving the sheet was not conscious about other conditions and the person handing out the paper also did not know which condition the participant received.

On Thursday night (Nov 2), we set up our table outside of Novack from 8:30-10:00 P.M. for the first half of data collection. The second day was Monday (Nov 6) where we set up from 7:30-9:15 P.M., and collected the rest of our data. The times were chosen based on the periods where the most people would be walking by the staircases of Novack. Both days, we had 2 dozen cookies from Foco set up at the front of our table as well as a large poster hanging on the wall behind us to encourage donations. All four members sat at the table - two members, who were randomly chosen, called and advertised to people to donate, and the remaining two handed out flyers and explained the cause. 


#Results & Data Analysis:

```{r, echo = FALSE}
dta = read.csv("Donations.csv")
```

During our fundraising process, we raised a total of $571. We collected data for each of the four groups and calculated sample means and sample deviations. For the no UNICEF affiliation without a personal story group, we calculated a sample mean of `r format(mean(dta$NoUNoS[1:20]))` and a sample standard deviation of `r format(sd(dta$NoUNoS[1:20]), digits = 4)`.
For the no UNICEF affiliation with a personal story group, we calculated a sample mean of `r format(mean(dta$NoUYesS[1:21]), digits = 4)` and a sample standard deviation of `r format(sd(dta$NoUYesS[1:21]), digits = 4)`.
For the UNICEF affiliation with a personal story group, we calculated a sample mean of `r format(mean(dta$YesUYesS[1:31]), digits = 4)` and a sample standard deviation of `r format(sd(dta$YesUYesS[1:31]), digits = 4)`.
For the UNICEF affiliation without a personal story group, we calculated a sample mean of `r format(mean(dta$YesUNoS[1:27]), digits = 4)` and a sample standard deviation of `r format(sd(dta$YesUNoS[1:27]), digits = 4)`.


```{r, echo = FALSE}
dta = read.csv("Donations.csv")
hist(dta$YesUNoS, xlim = range(0, 20), ylim = range(0, 0.6), freq = FALSE, main = "UNICEF & No Story", xlab = "Donation Amounts ($)", breaks = 10, col = "cornflowerblue")
```

```{r, echo = FALSE}
hist(dta$NoUNoS, xlim = range(0, 20), ylim = range(0, 0.6), freq = FALSE, main = "No UNICEF & No Story", xlab = "Donation Amounts ($)", breaks = 10, col = "lightblue1")
```

```{r, echo = FALSE}
hist(dta$NoUYesS, xlim = range(0, 20), ylim = range(0, 0.6), freq = FALSE, main = "No UNICEF & Story", xlab = "Donation Amounts ($)", breaks = 10,  col = "plum1")
```

```{r, echo = FALSE}
hist(dta$YesUYesS, xlim = range(0, 20), ylim = range(0, 0.6), freq = FALSE, main = "UNICEF & Story", xlab = "Donation Amounts ($)", breaks = 20, col = "lightpink")
```

##Test to remove \$30 as an outlier from the No UNICEF & No Story group:
 
In the group of sample data with no UNICEF logo/description and no personal story, we received a donation of \$30. This donation was extreme in comparisons to the other donations we received; we believed it would be appropriate to conduct a test to determine if this donation was in fact an outlier, and consequently if we would be able to remove it from our sample data. Assuming normality, we excluded the value of 30 and calculated a sample mean of `r format(mean(dta$NoUNoS[1:20]), digits = 2)` and a sample standard deviation of  `r format(sd(dta$NoUNoS[1:20]), digits = 3)`.

```{r, echo = FALSE}
dta = read.csv("Donations.csv") 
p.value <- 1 - pnorm(30, mean(dta$NoUNoS[1:20]), sd(dta$NoUNoS[1:20]))
curve(dnorm(x, mean(dta$NoUNoS[1:20]), sd(dta$NoUNoS[1:20])), from = 0, to =40, main = "Normal Density Curve for No UNICEF & No Story Group", ylab = "dnorm(x, mean = 6.3, sd = 3.672")
lines (x=c(30, 30), y = c(-1, 1), col = "blue")
text (x = 29, y = 0.01, "p-value = 5.443257e-11", pos = 1, cex = 0.8)
```

We calculated a p-value of `r format(p.value, digits = 4)` using 1 - pnorm(30, `r mean(dta$NoUNoS[1:20])`, `r format(sd(dta$NoUNoS[1:20]), digits = 4)`), showing that the probability of obtaining as extreme as or more extreme than \$30 is approximately 0. This confirmed that 30 is an outlier and was extreme enough to be removed from the data for further calculations.



##Two-Sample t - test:

The data collected could have been influenced by the date since different data gathering periods meant different dates. We compared the data gathered on day one to the data gathered on day two to see if the differences in data collecting days had a significant impact on the data collected.
  
The two-sample t test was used to make sure that the observed samples were not influenced on the day of collection. Setting the alpha value as 0.05, the hypotheses were:

$H_0$ : 
There is no difference between the data collected on Day 1 and Day 2
mean of data collected on Day 1 - mean of data collected on Day 2 = 0

$H_1$ : 
There is a difference between the data collected on Day 1 and Day 2
mean of data collected on Day 1 - mean of data collected on Day 2 $\neq$ 0.

```{r, echo=FALSE}
dta = read.csv("Donation with Days.csv")
t.test = t.test(dta$Donation ~ dta$Day)
t.test
p.value <- t.test$p.value
```

The resulted p-value of  `r format(p.value, digits = 4)` is a value larger than the alpha value. Therefore, we cannot reject the null hypothesis. There is no statistically significant evidence from the sample to assume that the true mean of donation differs based on the day of data collection.


##Chi-Squared Test:

We ran two chi-square tests of independence to determine if there was any significant relationship between the UNICEF variable and donation amounts, as well as between the personal story variable and donation amounts. To do this, we created two matrices, one for the UNICEF variable and one for the personal story variable. Because we noticed our donation amounts naturally fell around \$1, \$5, and \$10, we divided our donation amounts into three categories: low(\$0 to \$3), medium(\$4 to \$7), and high(\$8 and up). We realize these categories are arbitrary and affect our P-value; we address this issue later with our ANOVA test. 

First, we tested whether there was any association between a UNICEF label and donation amount with the following null and alternate hypothesis:

$H_0$: There is no association between donation amounts and UNICEF affiliation.
$H_1$: There is an association between donation amounts and UNICEF affiliation.

```{r, echo = FALSE}
unicefmatrix <- matrix(nrow = 2, ncol = 3, c(18, 12, 28, 17, 12, 12))
rownames(unicefmatrix) <- c("Yes Unicef", "No Unicef")
colnames(unicefmatrix) <- c("low", "medium", "high")
dimnames(unicefmatrix) <- list(rownames(unicefmatrix), colnames(unicefmatrix))
unicefmatrix
chisq.test(unicefmatrix, correct = FALSE)
unicefchi <- chisq.test(unicefmatrix, correct = FALSE)
```
For the Unicef chi square test, our chi square statistic was `r format(unicefchi$statistic, digits = 4)` with `r unicefchi$parameter` degrees of freedom.

Because our p-value of `r format(unicefchi$p.value, digits = 4)` is larger than our alpha of 0.05, we fail to reject the null hypothesis. We have convincing evidence that there is no association between donation amounts and UNICEF affiliation.


Next, we tested whether there was any association between a personal story and donation amount with the following null and alternate hypothesis:

$H_0$: There is no association between donation amounts and personal story.
$H_1$: There is an association between donation amounts and personal story.

```{r, echo = FALSE}
storymatrix <- matrix(nrow = 2, ncol = 3, c(16, 14, 26, 19, 10, 14))
rownames(storymatrix) <- c("Yes Story", "No Story")
colnames(storymatrix) <- c("low", "medium", "high")
dimnames(storymatrix) <- list(rownames(storymatrix), colnames(storymatrix))
storymatrix
chisq.test(storymatrix, correct = FALSE)
storychi <- chisq.test(storymatrix, correct = FALSE)
```
For the story chi square test, our chi square statistic was `r format(storychi$statistic, digits = 4)` with `r storychi$parameter` degrees of freedom.

Because our p-value of `r format(storychi$p.value, digits = 4)` is larger than our alpha of 0.05, we fail to reject the null hypothesis. We have convincing evidence that there is no association between donation amounts and personal story.


##Anova Test for Differences in Sample Means:

The Analysis of Variance (ANOVA) test was used to determine whether there were any statistically significant differences between the means of the four treatment groups in our test. This test differs from the previous Chi-Squared test as it compares the quantitative values of the mean instead of comparing arbitrary groups of “low”, “medium”, and “high”. The null and alternative hypotheses were as follows:

$H_0$: There is no significant difference in sample average donation amounts for each of the categorical variables (combinations of story/no story and UNICEF/no UNICEF).
$H_1$: There is a significant difference in sample average donation amounts for each of the categorical variables (combinations of story/no story and UNICEF/no UNICEF).

```{r, echo = FALSE}
dta <- read.csv("ANOVAdata.csv")
fit <- aov(dta$Donations ~ dta$Type)
summary (fit)
```

The p-value from the ANOVA test was `r format(unlist(summary(fit))[9], digits=4)`, which was greater than the significance level of 0.05 and did not allow us to reject the null. We have reason to conclude that there is no statistically significant difference between the sample mean donation amounts for each of the treatment groups (combinations of story/no story and UNICEF/no UNICEF).	


#Caveats:

##GoFundMe Failures:

The original plan for data collection was to use a fundraising page for receiving donations. We created four separate GoFundMe pages, corresponding to combinations of UNICEF/no UNICEF and story/no story, randomly selected a thousand Dartmouth undergraduate students for each of the conditioned pages, and sent out an email. We received no response for all four conditions for a week. We took the pages down and designed an alternative experiment that shares the same conditions but uses a different method.

The failed trial becomes a factor of caveat. Before taking the page down, we checked the number of visitors to see if anybody clicked the link. All the links had around 30 visitors, which means that some students were pre-exposed to the conditioning. Since we do not know who they are and what type of condition they received, if they were to come up to our booth and donate, they could have received a different treatment than the one they were previously exposed to.
	
##Individuals with Questions: 

When given the treatment of a flier without a charity name written on, some people questioned how we were going to use the donation; in other words, where the donations would be sent to.

Because we were in front of the participants, we were able to observe participants who questioned the validity of the fundraiser. While the intention of the test was to see if a charity name would be able to influence the amount of donation, because we were asked on spot, we thought that it looked too suspicious for us to refuse to answer the questions of some individuals. 

Therefore, when asked where the money would go, we informed the participants that we planned on donating to UNICEF's program for children with disabilities, eliminating the conditioning factor and reallocating their donation from the No UNICEF treatment to the corresponding Yes UNICEF treatment (keeping the personal story variable the same). Because these individuals with suspicions should have been recorded in corresponding No UNICEF categories, and presumably would have donated smaller amounts of money, the No UNICEF groups should have had some of these lower values included in the samples. We lost individuals from the samples that presumably would have reduced the means for the No UNICEF groups; instead, these individuals were included in corresponding Yes UNICEF groups. However, these reallocated individuals did not receive the exact same treatments as the rest of the individuals in the groups as their handouts lacked the UNICEF logo and description and they received the treatments with pre-existing biases that arose from their suspicions about the lack of charity affiliation.

With the alternative hypothesis stating a difference between the true mean of the four conditions, having a lower sample mean for No UNICEF groups and a higher sample mean for Yes UNICEF groups could result in a higher z-score and thus a lower p value. The hypothesis tests we performed, therefore, could have a Type II error due to this caveat. If this issue was resolved, the data could have a different set of sample means, which could result in a lower p value, giving us convincing evidence to reject the null.


##Cookies:

Cookies were a confound because some students seemed to be donating to get a cookie rather than donating in response to the cause. It is possible that since some people donated in order to get a cookie, they were not treated properly. In order for participants to receive and respond to the treatment, they needed to read through the flier and question the credibility of the fundraiser. However, individuals who donated not for the cause but for the cookie donated what they thought was the appropriate value of a single cookie. Therefore, some of the observed values might be a result of ineffective responses to treatments.


##Exposure to Others’ Papers:

When there were numerous donors at the table at the same time, we ran into the problem where we had to ensure people who got different handouts couldn’t tell they got a different handout than other people. It could be that some of those who were in the non-UNICEF groups inadvertently saw the UNICEF logo and donated more due to the reputation that UNICEF has. This could mean that several of the donations that we believed to be treated under the non-UNICEF should have been assigned under the UNICEF group.  

Given this limitation, the sample means for the non-UNICEF treatment groups could be lower when we resolve the issue of exposure to other conditions. This suggests that there could be a Type II error given the lower p-value associated with decreasing the sample mean to accomodate for the caveat. A data collection method that eliminated this caveat could have allowed us to reject the null hypothesis. 

#Conclusion:

Our research was predicated upon answering two specific questions about the donation tendencies of Dartmouth undergraduate students: are students more likely to donate to charities of greater recognition? Furthermore, are they more willing to donate to charities that present a personal and touching story? Utilizing our data collection method, we gathered and analyzed data to find whether or not the differences in sample means for each treatment group were statistically significant. Both the Chi-Squared test and the ANOVA test presented p-values greater than our significance value, which suggested that there were no statistically significant differences between the sample means for the treatment groups. 

Throughout the process of data collection, we recognized various shortcomings and caveats that could have affected our sample size and sample means. It is important to note that these caveats affected our data and results, and could be eliminated through a more extensive method of data collection that controls for confounding variables. With the caveats resolved, we might have been able to observe a lower p - value, which would have allowed us to reject the null and possibly find a statistically significant difference between the means of the four treatment groups. Furthermore, our sample sizes were relatively low, which impacted our ability to properly conduct our previous statistical tests. Larger sample sizes would have minimized the effect extreme values had on the results of our tests and analysis. 

Although we were not able to find any statistically significant differences between treatment groups, we still believe that this research was worthwhile and should be further pursued. Charities and fundraisers are essential aspects of any college campus, and identifying certain donor tendencies would be highly beneficial for both students and non-profit organizations. 
