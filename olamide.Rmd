---
title: An R Markdown document converted from "olamide.ipynb"
output: html_document
---

```{r}
df <- read.table("melanoma-1.csv", 
                 header = TRUE,
                 sep = ",")
```

```{r}
df
```

```{r}
# rows and column
```

```{r}
dim(df)
```

```{r}
head(df , n=5)
```

```{r}
df = subset(df, select=-c(X))
# drop column X
summary(df)
```

```{r}
str(df)
```

```{r}
options(bitmapType='cairo', repr.plot.width = 6, repr.plot.height = 6, repr.plot.res = 200)
options(repr.font.family='DejaVu Serif')
names(df)
```

```{r}
par(family="DejaVu Serif")

boxplot(
df$age,
main="Boxplot Age ",
xlab="Boxplot",
ylab="Age ",
col="orange",
border="brown"
)
# evenly distributed, and an outlier
```

```{r}
par(family="DejaVu Serif")

boxplot(df$status,
        horizontal = TRUE,
main="Different boxplots of survival",
xlab="Status",
ylab="Boxplot",
col="orange",
border="brown"
)
# most people are still living , skewed to the right
```

```{r}
par(family="DejaVu Serif")

boxplot(df$sex,
        horizontal = TRUE,
main="Different boxplots of sex",
xlab="Status",
ylab="Boxplot",
col="orange",
border="brown"
)
# most people are still living , skewed to the right
```

```{r}
par(family="DejaVu Serif")

boxplot(df$year,
        horizontal = TRUE,
main="Different boxplots of year",
xlab="Year",
ylab="Boxplot",
col="orange",
border="brown"
)
# most people are still living , skewed to the right
```

```{r}
par(family="DejaVu Serif")

boxplot(df$thickness,
        horizontal = TRUE,
main="Different boxplots of Thickness",
xlab="Thickness",
ylab="Boxplot",
col="orange",
border="brown"
)
# most people are still living , skewed to the right
```

```{r}
par(family="DejaVu Serif")

boxplot(df$ulcer,
        horizontal = TRUE,
main="Different boxplots of Ulcer",
xlab="Ulcer",
ylab="Boxplot",
col="orange",
border="brown"
)
# most people are still living , skewed to the right
```

```{r}
library(dplyr)
```

```{r}
par(family="DejaVu Serif")

boxplot(df$time,
        horizontal = TRUE,
main="Different boxplots of time",
xlab="Time",
ylab="Boxplot",
col="orange",
border="brown"
)
```

```{r}
library(dplyr)  
```

```{r}
# counting survival status
df %>%
group_by(status) %>%
summarise(status_count=n())
```

```{r}
# many people have similar survival status
```

```{r}
par(family="DejaVu Serif")

plot(df$thickness, df$age, pch = 16, cex = 1.3, col = "blue", main = "THICKNESS PLOTTED AGAINST AGE", xlab = "THICKNESS(mm)", ylab = "AGE")
abline(lm(df$age ~ df$thickness ))
```

```{r}
par(family="DejaVu Serif")

plot(df$time,  df$age, pch = 16, cex = 1.3, col = "blue", main = "AGE PLOTTED AGAINST TIME", xlab = "TIME (days)", ylab = "AGE")
abline(lm(df$age ~ df$time  ))
```

```{r}
par(family="DejaVu Serif")

plot(df$time, df$thickness, pch = 16, cex = 1.3, col = "blue", main = "THICKNESS PLOTTED AGAINST TIME", xlab = "TIME (days)", ylab = "THICKNESS(mm)")
abline(lm(df$thickness ~ df$time ))
```

```{r}
df <- df %>% group_by(sex) 
eruption.lm = lm(thickness ~ age, data=df)
```

```{r}
summary(eruption.lm)
```

```{r}
# As the p-value is much less than 0.05, we reject the null hypothesis that β = 0. Hence there is a significant relationship between the variables in the linear regression model of the data set faithful
```

```{r}
options(bitmapType='cairo')
mapply(hist,as.data.frame(df),main=colnames(df),xlab="x", family='DejaVu Serif') 
```

```{r}
df <- df %>% group_by(sex)
par(family="DejaVu Serif")
qqnorm(df$age, pch = 1, frame = FALSE)
qqline(df$age, col = "steelblue", lwd = 2)
```

```{r}
df <- df %>% group_by(sex)
par(family="DejaVu Serif")
qqnorm(df$time, pch = 1, frame = FALSE)
qqline(df$time, col = "steelblue", lwd = 2)
```

```{r}
df <- df %>% group_by(sex)
par(family="DejaVu Serif")
qqnorm(df$thickness, pch = 1, frame = FALSE)
qqline(df$thickness, col = "steelblue", lwd = 2)
```

```{r}
tbl <- with(df, table(year))
```

```{r}
par(family="DejaVu Serif")
barplot(tbl, beside=TRUE, legend=TRUE)
```

```{r}
tbl <- with(df, table(status, year))
par(family="DejaVu Serif")
barplot(tbl, beside=TRUE, legend=TRUE, xlab="year")
```

```{r}
tbl <- with(df, table(status, sex))
par(family="DejaVu Serif")
barplot(tbl, beside=TRUE, legend=TRUE, xlab="sex")
```

```{r}
# CORRELATION BETWEEN TIME AND THICKNESS
res <- cor.test(df$time, df$thickness, 
                    method = "pearson")
res
# negative correlation
```

```{r}
# NEGATIVE CORRELATION
res_1 <- cor.test(df$time, df$age, 
                    method = "pearson")
res_1
```

```{r}
# POSITIVE CORRELATION
res_2 <- cor.test(df$thickness, df$age, 
                    method = "pearson")
res_2
```

```{r}
tbl <- with(df, table(status, ulcer))
par(family="DejaVu Serif")
barplot(tbl, beside=TRUE, legend=TRUE, xlab="ulcer")
```

```{r}
tbl <- with(df, table(sex, status))
par(family="DejaVu Serif")
barplot(tbl, beside=TRUE, legend=TRUE, xlab="status")
```

```{r}
tbl <- with(df, table(status))
par(family="DejaVu Serif")
barplot(tbl, beside=TRUE, legend=TRUE, xlab="status")
```

```{r}
tbl <- with(df, table(sex))
par(family="DejaVu Serif")
barplot(tbl, beside=TRUE, legend=TRUE, xlab="sex")
```

```{r}
# correlation
cormat <- round(cor(df),2)
```

```{r}
cormat
```

```{r}
sd(df$age)
```

```{r}
sd(df$time)
```

```{r}
sd(df$sex)
```

```{r}
sd(df$status) 
```

```{r}
sd(df$year)
```

```{r}
sd(df$thickness)
```

```{r}
sd(df$ulcer)
```

```{r}
rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(df$time, df$thickness)
```

```{r}
rsq(df$time, df$age)
```

```{r}
rsq(df$thickness, df$age)
```

```{r}
df_grp_region = df %>% group_by(sex)  %>%
                    summarise(total_sales = sum(age),
                              total_profits = sum(time),
                              .groups = 'drop')
 

View(df_grp_region)
```

```{r}
df2 <- df %>% group_by(sex) 
eruption.lm = lm(thickness ~ age, data=df2)
summary(eruption.lm)
```

```{r}
t.test(df$time, df$thickness)$p.value
```

```{r}
names(ttest)
```

```{r}
ttest$p.value
```

```{r}
df_grp_region = df %>% group_by(sex)  %>%
                    summarise(time_thickness = t.test(time, thickness)$p.value,
                              time_age = t.test(time, age)$p.value,
                              thickness_age = t.test(thickness, age)$p.value,
                              .groups = 'drop')
 

View(df_grp_region)
```

```{r}
convert_ipynb('olamide.ipynb','assignment.R')
```

```{r}
library(rmarkdown)
```

```{r}
convert_ipynb(input, output = xfun::with_ext(input, "Rmd"))
```

```{r}
install.packages("rmarkdown")
```

