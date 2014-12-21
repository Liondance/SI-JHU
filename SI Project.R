#
# SI - Project
#

library(ggplot2);

#
# Exponential
#

set.seed(997);

N <- 1000;

lambda <- 0.2;

sample <- rexp(N, lambda);
qplot(sample, binwidth=1);

hist(sample, breaks=16);
abline(v=mean(sample), lwd=2, col="red");
abline(v=1/lambda, lwd=2, col="blue");

means <- NULL;
ks <- NULL;

for (k in c(10, 20, 40, 80)) {
    for (i in 1:N) {
        means <- c(means, mean(rexp(k, lambda)));
        ks <- c(ks, k);
    }
}

df <- data.frame(x=means, k=ks);
qplot(x, data=df, facets= . ~ k, color=factor(k));

qplot(x, data=df[df$k==10,], facets= . ~ k, color=factor(k));
qplot(x, data=df[df$k==20,], facets= . ~ k, color=factor(k));
qplot(x, data=df[df$k==40,], facets= . ~ k, color=factor(k));
qplot(x, data=df[df$k==80,], facets= . ~ k, color=factor(k));

sample <- df[df$k==10,]$x
hist(sample, breaks=16);
abline(v=mean(sample), lwd=2, col="red");
abline(v=1/lambda, lwd=2, col="blue");


#
# Part 2
#

rm(list = ls());

data(ToothGrowth);

TG = ToothGrowth;

xtabs(    ~ supp + dose, data=TG)

growth <- xtabs(len ~ supp + dose, data=TG) / xtabs(    ~ supp + dose, data=TG);

for (supp in levels(TG$supp)) {
    for (dose in unique(TG$dose)) {
        rows <- which(TG$supp==supp & TG$dose==dose);
        obs <- TG[rows, ];
        print(mean(obs$len));
        print(sd(obs$len));
    }
}

qplot(y=len, x=dose, data=TG, color=supp, geom=c("point", "smooth"));
