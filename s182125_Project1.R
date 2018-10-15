
###########################################################################
## Set the working directory

## In RStudio the working directory is easily set via the menu
## "Session -> Set Working Directory -> To Source File Location" 
## Note: In R only "/" is used for separating in paths 
## (i.e. no backslash).
setwd("")

###########################################################################
## Read data into R

## Read data from bmi1_data.csv
D <- read.table("bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)

#1: height    cm
#2: weight    kg
#3: gender    0: Female   1: Male
#4: urbanity  1: Outside urban areas
#             2: City with less than 10,000 inhabitants
#             3: City with 10,000 to 49,999 inhabitants
#             4: City with 50,000 to 99,999 inhabitants
#             5: City with over 100,000 inhabitants
#5: fastfood  1: Never
#             2: Less than 1 time per year 3) 1-11 times per year
#             4: 1-3 times per month
#             5: 1-2 times per week
#             6: 3-4 times per week
#             7: 5-6 times per week
#             8: Every day

###########################################################################
## Simple overview of the data

## Dimensions of D (number of rows and columns)
dim(D)
##  Column/variable names
names(D)
## The first rows/observations
head(D)
## The last rows/observations
tail(D)
## Selected summary statistics
summary(D)
## Another type of summary of the dataset
str(D)
## Check for missing values
sum(is.na.data.frame(D))

###########################################################################
## Calculate BMI scores

## Calculate BMI scores and add new variable to D
D$bmi <- D$weight/(D$height/100)^2

###########################################################################
## Histogram (empirical density)

## Histogram describing the empirical density of the BMI scores
## (histogram of the BMI scores normalized to have an area of 1)

hist(D$bmi, main = "Density Histogram of BMI", xlim = c(15,40),xlab="BMI", prob=TRUE,
     las=1, 
     col = "green")
abline(v=median(D$bmi),col="red")
abline(v=mean(D$bmi),col="blue")

###########################################################################
## Taking subsets of the data using 'subset'

## Divide data into two subsets according to gender
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)

###########################################################################
## Density histograms by gender

## Density histograms describing the empirical density
## of the BMI scores of women and men, respectively.
hist(Dfemale$bmi, xlab="BMI (female)", prob=TRUE)
hist(Dmale$bmi, xlab="BMI (male)", prob=TRUE)

hist(Dfemale$bmi, main = "Density Histogram of BMI for Females", xlim = c(15,40),xlab="BMI", prob=TRUE,
     las=1, 
     col = "green")
abline(v=median(Dfemale$bmi),col="red")
abline(v=mean(Dfemale$bmi),col="blue")

hist(Dmale$bmi, main = "Density Histogram of BMI for Males", xlim = c(15,40),xlab="BMI", prob=TRUE,
     las=1, 
     col = "green")
abline(v=median(Dmale$bmi),col="red")
abline(v=mean(Dmale$bmi),col="blue")

###########################################################################
## Box plot

## Box plot of BMI scores by gender
boxplot(Dfemale$bmi, Dmale$bmi, names=c("Female", "Male"), 
        xlab="Gender", ylab="BMI")

###########################################################################
## Summary statistics for BMI

Tbl <- data.frame()
Tbl[1, "Number of obs."] <- sum(!is.na(D$bmi))
Tbl[1, "Sample mean"] <- mean(D$bmi)
Tbl[1, "Sample Variance"] <- var(D$bmi)
Tbl[1, "Sample st. dev."] <- sd(D$bmi)
Tbl[1, "Lower quartile"] <- quantile(D$bmi, 0.25)
Tbl[1, "Median"] <- median(D$bmi)
Tbl[1, "Upper quartile"] <- quantile(D$bmi, 0.75)
for(i in 0:1){
  Tbl[i+2, "Number of obs."] <- sum(!is.na(D$bmi[D$gender == i]))
  Tbl[i+2, "Sample mean"] <- mean(D$bmi[D$gender == i])
  Tbl[i+2, "Sample Variance"] <- var(D$bmi[D$gender == i]) 
  Tbl[i+2, "Sample st. dev."] <- sd(D$bmi[D$gender == i]) 
  Tbl[i+2, "Lower quartile"] <- quantile(D$bmi[D$gender == i], 0.25)
  Tbl[i+2, "Median"] <- median(D$bmi[D$gender == i])
  Tbl[i+2, "Upper quartile"] <- quantile(D$bmi[D$gender == i], 0.75)
}
#It's kinda ugly I know

row.names(Tbl) <- c("Everyone", "Women","Men")
xtable(Tbl, align = "p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}")

###########################################################################
## qq-plot for model validation

## New variable 'logbmi' with log-transformed BMI
D$logbmi <- log(D$bmi)
## qq-plot of log-transformed BMI
qqnorm(D$logbmi)
qqline(D$logbmi)

## histogram of log-transformed BMI
hist(D$logbmi, main = "Density Histogram of log transformed BMI", xlab="BMI", prob=TRUE,
     las=1, 
     col = "green")

mean(D$logbmi)
sd(D$logbmi)
length(D$logbmi)
##student-t
qt(p=0.975, df=length(D$logbmi)-1)
res <- t.test(D$logbmi, conf.level = 0.95)

##range of median BMI
exp(res[['conf.int']][1])
exp(res[['conf.int']][2])

###########################################################################
## One-sample t-test
mean <- mean(D$logbm)
u0 <- log(25)
s = sd(D$logbm)
n = length(D$logbm)
tobs <- (mean-u0)/(s/sqrt(n))
tobs
pvalue <- 2*(1-pt(abs(tobs), df=n-1))
pvalue
## Testing hypothesis mu=log(25) for log-transformed BMI

t.test(D$logbmi, mu=log(25))

###########################################################################
## Q-Q plot for women
women <- subset(D, gender == 0)$logbmi
qqnorm(women)
qqline(women)
## Q-Q plot for men
men <- subset(D, gender == 1)$logbmi
qqnorm(men)
qqline(men)

##means and sds
mean(women)
mean(men)
sd(women)
sd(men)

## CI's for the mean and median

## Compute CI for mean log-BMI score
women_conf <- t.test(women, conf.level=0.95)$conf.int
men_conf <- t.test(men, conf.level=0.95)$conf.int

## "Back-transform" to get a CI for median BMI score
exp(women_conf)
exp(men_conf)

###########################################################################
## Welch t-test for comparing two (independent) samples
mw <- mean(women)
mm <- mean(men)
sw <- sd(women)
sm <- sd(men)
tobs <- (mw-mm)/(sqrt(sw*sw/72+sm*sm/73))
tobs
s1 <- sw*sw
s2 <- sm*sm
n1 <-length(women)
n2 <- length(men)
num <- (s1/n1+s2/n2)*(s1/n1+s2/n2)
den <- (s1/n1)*(s1/n1)/(n1-1) + (s2/n2)*(s2/n2)/(n2-1)
v <- num/den
v
pvalue <- 2*(1-pt(abs(tobs), df=v))
pvalue
## Comparison of mean logBMI for women and men
t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])

###########################################################################
## Computing correlations
bmi <- D$bmi
weight <- D$weight
fastfood <- D$fastfood
cor(bmi, weight)
cor(bmi, fastfood)
cor(weight, fastfood)
plot(bmi, weight, xlab = "bmi", ylab = "weight")
plot(bmi, fastfood, xlab = "bmi", ylab = "fastfood")
plot(weight, fastfood, xlab = "weight", ylab = "fastfood")

## Computing correlations between selected variables
cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")