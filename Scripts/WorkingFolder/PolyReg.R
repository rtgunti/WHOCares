p <- 0.5
q <- seq(0,100,1)
y <- p*q
plot(q,y,type='l',col='red',main='Linear relationship')

y <- 450 + p*(q-10)^3
plot(q,y,type='l',col='navy',main='Nonlinear relationship',lwd=3)

set.seed(20)
q <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (q-10)^3
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)

model <- lm(noisy.y ~ poly(q,3))
summary(model)
confint(model, level=0.95)

predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',
                               level=0.99)

lines(q,predicted.intervals[,1],col='green',lwd=3)
lines(q,predicted.intervals[,2],col='black',lwd=1)
lines(q,predicted.intervals[,3],col='black',lwd=1)

legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)
plot(fitted(model),residuals(model))
