setwd("C:/Users/Lesly Yaneth/Documents")
df=read.csv("problema 12.csv",sep = ";" )
df


df$T=factor(df$T)
str(df)

boxplot(Y~T,data=df)

modelo=aov(Y~T,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(Y~T,data=df)


plot(modelo$residuals)
abline(h=0)

plot(modelo$fitted.values, modelo$residuals)
abline(h=0)