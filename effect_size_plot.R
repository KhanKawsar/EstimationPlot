#load library
library("boot")
library(bootES) ## will be replaced by our package 
library(yarrr) ## will be replaced by jk_col, in our package  
library(vioplot) ## will be replaced by violplot 

## function for colour and shade 

jk_col <-  function(colour, shade) {
  rgb.val <- col2rgb(colour)
  t.col <- rgb(rgb.val[1, ], rgb.val[2, ], rgb.val[3, ],
               max = 255,
               alpha = (100 - shade*100) * 255 / 100)
  invisible(t.col)
}

## function for unpair mean differnce 

jim_unpair <- function(data, indices){
  subset = data[indices, ]
  m1 = mean(subset$length[subset$male == "yellow"]) 
  m2 = mean(subset$length[subset$male == "red"])
  m1 - m2
}



## load data 
len <- read.csv("data/length.csv")
head(len)

##calculate effect size 
set.seed(12345)

contrast <- c("yellow", "red")

a <- bootES(len, R =10000, 
            data.col = "length", contrast = contrast,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)

str(a)
a$bounds
a$t0
a$t
d <- density(a$t)

plot(d$y, d$x, 
     axes = FALSE,
     type = "n")
polygon(d$y, d$x, col="red", border="red")

dev.off()
#### Cummings_plot_unpaired####

# create layout for Cummings plot 

j1 <- boot(len, jim_unpair, R = 10000)
k1 <- boot.ci(j1, type = "bca")
k1$t0


####

l <- layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE),
            heights = c(1,0.8))
layout.show(l)
par(mar=c(4,5,1,2))

##add boxplot with individual data point with jitter  
boxplot(length~male, data = len,
        col = jk_col(c("blue", "red"), shade = .6),
        border = jk_col(c("blue", "red"), shade = .6),
        las = 1, staplewex= 0.4, outline = FALSE)

# boxplot with border only 
boxplot(length~male, data = len,
col = "NA",
border = jk_col(c("blue", "red"), shade = .6),
las = 1, staplewex= 0.4, outline = FALSE, lwd = 2)

stripchart(length~male, data = len, method = "jitter",
            pch = 19, vertical = TRUE,
           col = jk_col(c("blue", "red"), shade = .5),
            add = TRUE)

## add effect size in lower panel using density plot

plot(NULL,xlim = c(0, max(range(d$y)+1.5)), ylim = range(d$x),
     type ="n", axes = FALSE, ylab = "Mean difference", xlab = "")

polygon(d$y+1.5, d$x,
        col=jk_col("pink", shade = .4), 
        border=jk_col("pink", shade = .4))

points (1.5,a$t0, pch = 19, col = "grey20", cex = 1.5)
segments(1.5, a$bounds[1], 1.5, a$bounds[2], col = "grey20", lty =1, lwd=2)

axis(2, at = pretty(range(a$t)),
     labels = pretty(range(a$t)))
abline(h = 0)

axis(1, at = 1.0+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

dev.off()

#### Cummings_plot_paired####
l <- layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE),
            heights = c(1,0.8))
layout.show(l)
par(mar=c(4,5,2,2))

boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, at = c(1,2))

## boxplot with border only 
boxplot(length~male, data = len,
        col = "NA",
        border = yarrr::transparent(c("blue", "red"), trans.val = .4),
        las = 1, staplewex= 0.4, outline = FALSE, lwd =1.5)


stripchart(length~male, data = len,
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           at = c(1,2), add = TRUE)

segments(1.0,len$length[len$male == "red"][1:length(len$length[len$male == "red"])], 
         2.0, len$length[len$male == "yellow"][1:length(len$length[len$male == "yellow"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)

###calculate pair differnce 


head(len)

len$id <- c(1:19, 1:19)

id = unique(len$id)

jim_pair <- len$length[len$male== "red" & len$id %in% id]- len$length[len$male== "yellow" & len$id %in% id]

#bootES(jim_pair)

j2 <- boot(jim_pair, function(data, indices)mean(data[indices]), R =10000)

boot.ci(j2, type = "bca")

head(len) 

## bootstrap iD only 
#https://stats.stackexchange.com/questions/1399/obtaining-and-interpreting-bootstrapped-confidence-intervals-from-hierarchical-d?rq=1
# as suggested by the upper question (ref: "Bootstrap methods and their application", 1997, Section 3.8))
### task how to add jitter + lines 

## add effect size in in lower panel with density plot 


plot(NULL,xlim = c(0, max(range(d$y)+1.5)), ylim = range(d$x),
     type ="n", axes = FALSE, ylab = "Paired mean difference", xlab = "")

polygon(d$y+1.5, d$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (1.5,a$t0, pch = 16, col = "blue", cex = 1.5)
segments(1.5, a$bounds[1], 1.5, a$bounds[2], col = "blue", lty =1, lwd=2)

axis(2, at = pretty(range(a$t)),
     labels = pretty(range(a$t)))
abline(h = 0)

axis(1, at = 1.0+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))


dev.off()


#### Gardner-Altman plot unpaired ####


par(mar=c(4,5,2,4))
##box and jitter 
boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        xlim = c(0,4.5), at = c(1,2))

## boxplot with border only 

boxplot(length~male, data = len,
        col = "NA",
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        xlim = c(0,4.5), at = c(1,2), lwd =1.5)


p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

points(1.4,q, pch = 19, col = yarrr::transparent("blue", trans.val = .2), cex =1.5)
points(2.4,p, pch = 19, col = yarrr::transparent("red", trans.val = .2), cex =1.5)


stripchart(length~male, data = len, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

polygon(d$y/max(d$y)+3.0, d$x+p, 
        col=yarrr::transparent("pink", trans.val = .3), 
        border=yarrr::transparent("pink", trans.val = .3))

#polygon(d$y/(max(d$y)*2)+3.0, d$x+p, 
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (3,p+a$t0, pch = 19, col = "grey20", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "grey20", lty =1, lwd=2.0)


segments(2+0.4, p, 5, p, col = "grey20", lty =1, lwd=1.5)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "grey20", lty =1, lwd=1.5)###
axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)), las = 1)

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext("Mean difference",  side =4, line = 2.5)

dev.off()


## violin and jitter 
par(mar=c(4,5,2,4))
##box and jitter 

## add violin chart 
vioplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        rectCol = "NA",
        lineCol = "NA",
        colMed = "NA",
        xlim = c(0,4.5), at = c(1,2))

##replace violin with density chart 


a1 <- density(len$length[len$male == "red"], adjust = 1.5, bw ="nrd0")

plot(NULL,xlim = c(0.5,1), ylim = range(c(a1$x)),
     type ="n", axes = FALSE, ylab = "length")

polygon(1-a1$y, a1$x,
        col=yarrr::transparent("red", trans.val
                               = .6), 
        border= "NA")



b1 <- density(len$length[len$male == "yellow"], adjust = 1.5)

plot(NULL,xlim = c(0,4.5), ylim = range(c(a1$x)),
     type ="n", axes = FALSE, ylab = "length")

polygon(2-b1$y, b1$x,
        col=yarrr::transparent("red", trans.val = .6), 
        border= "NA")

#polygon(1+a1$y, a1$x,
        col=yarrr::transparent("red", trans.val = .6), 
        border="NA")

#segments(1, min(len$length[len$male == "red"]), 1, max(len$length[len$male == "red"]), 
         col = yarrr::transparent("red", trans.val = .6), 
         lty =1, lwd =1)

#polygon(1-f$y, f$x,
        col=yarrr::transparent("purple", trans.val = .6), 
        border=yarrr::transparent("purple", trans.val = .6))


points(2, p, pch = 19, cex = 1.5)
points(1, q, pch = 19, cex = 1.5)

stripchart(length~male, data = len, method = "jitter",
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))


p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2+0.4, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Unpaired mean difference"),  side =4, line = 2.5)
dev.off()


####split violoin and jitter 
par(mar=c(4,5,2,4))

vioplot(length~male, data = len,side = "left",
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        rectCol = "NA",
        lineCol = "NA",
        colMed = "NA",
        xlim = c(0,4.5), at = c(1,2))

points(2, p, pch = 19, cex = 1.5)
points(1, q, pch = 19, cex = 1.5)

stripchart(length~male, data = len, method = "jitter",
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))


p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2.1, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1.1, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Unpaired Mean difference"),  side =4, line = 2.5)

dev.off()


## rain cloud plot 

par(mar=c(4,5,2,4))

vioplot(length~male, data = len,side = "left",
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        rectCol = "NA",
        lineCol = "NA",
        colMed = "NA",
        xlim = c(0,4.5), at = c(1,2))

boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE,boxwex = 0.3, 
        xlim = c(0,4.5), at = c(1.1,2.1), add = TRUE, names = FALSE)

stripchart(length~male, data = len, method = "jitter",
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

points(2.1, p, pch = 19, cex = 1.5)
points(1.1, q, pch = 19, cex = 1.5)

p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2.1, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1.1, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Unpaired mean difference"),  side =4, line = 2.5)

dev.off()

### mean, CI and jitter  

par(mar=c(4,5,2,4))

stripchart(length~male, data = len, method = "jitter",
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = FALSE, at = c(1,2),
           xlim = c(0,4.5))

m <- bootES(len$length[len$male=="red"])
n <- bootES(len$length[len$male=="yellow"])

p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

segments(1, m$bounds[1], 1, m$bounds[2], col = "blue", lty =1, lwd=2)
segments(2, n$bounds[1], 2, n$bounds[2], col = "blue", lty =1, lwd=2)

points(2, p, pch = 19, cex = 1.5)
points(1, q, pch = 19, cex = 1.5)

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2.1, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1.1, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Unpaired mean difference"),  side =4, line = 2.5)

dev.off()


#### #### Gardner-Altman plot paired ####

par(mar=c(4,5,2,4))

##box and jitter 
boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = TRUE, 
        xlim = c(0,4.5), at = c(1,2))

stripchart(length~male, data = len,
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

segments(1.0,len$length[len$male == "red"][1:length(len$length[len$male == "red"])], 
         2.0, len$length[len$male == "yellow"][1:length(len$length[len$male == "yellow"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)


#par(new = TRUE) 
p <- mean(len$length[len$male=="yellow"])
polygon(d$y+3.0, d$x+p, 
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2+0.4, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Paired mean difference"),  side =4, line = 2.5)

dev.off()

## violin and jitter 
par(mar=c(4,5,2,4))

##box and jitter 
vioplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        rectCol = "NA",
        lineCol = "NA",
        colMed = "NA",
        xlim = c(0,4.5), at = c(1,2))

points(2, p, pch = 19, cex = 1.5)
points(1, q, pch = 19, cex = 1.5)

stripchart(length~male, data = len, 
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

segments(1.0,len$length[len$male == "red"][1:length(len$length[len$male == "red"])], 
         2.0, len$length[len$male == "yellow"][1:length(len$length[len$male == "yellow"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)


p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2+0.4, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Paired mean difference"),  side =4, line = 2.5)

dev.off()


####split violoin and jitter 
par(mar = c(4,5,2,4))
vioplot(length~male, data = len,side = "left",
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        rectCol = "NA",
        lineCol = "NA",
        colMed = "NA",
        xlim = c(0,4.5), at = c(1,2))

points(2, p, pch = 19, cex = 1.5)
points(1, q, pch = 19, cex = 1.5)

stripchart(length~male, data = len, 
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

segments(1.0,len$length[len$male == "red"][1:length(len$length[len$male == "red"])], 
         2.0, len$length[len$male == "yellow"][1:length(len$length[len$male == "yellow"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)


p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2.1, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1.1, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Paired mean difference"),  side =4, line = 2.5)

dev.off()


## rain cloud plot 

par (mar = c(4,5,2,4))
vioplot(length~male, data = len,side = "left",
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        rectCol = "NA",
        lineCol = "NA",
        colMed = "NA",
        xlim = c(0,4.5), at = c(1,2))

boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE,boxwex = 0.3, 
        xlim = c(0,4.5), at = c(1.1,2.1), add = TRUE, names = FALSE)

stripchart(length~male, data = len, 
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

segments(1.0,len$length[len$male == "red"][1:length(len$length[len$male == "red"])], 
         2.0, len$length[len$male == "yellow"][1:length(len$length[len$male == "yellow"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)

points(2.1, p, pch = 19, cex = 1.5)
points(1.1, q, pch = 19, cex = 1.5)


p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2.1, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1.1, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Paired mean difference"),  side =4, line = 2.5)

dev.off()

### mean, CI and jitter  

par(mar = c(4,5,2,4))

stripchart(length~male, data = len, 
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = FALSE, at = c(1,2),
           xlim = c(0,4.5))

m <- bootES(len$length[len$male=="red"])
n <- bootES(len$length[len$male=="yellow"])

p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

segments(0.8, m$bounds[1], 0.8, m$bounds[2], col = "blue", lty =1, lwd=2)
segments(2.2, n$bounds[1], 2.2, n$bounds[2], col = "blue", lty =1, lwd=2)

points(2.2, p, pch = 19, cex = 1.5)
points(0.8, q, pch = 19, cex = 1.5)

segments(1.0,len$length[len$male == "red"][1:length(len$length[len$male == "red"])], 
         2.0, len$length[len$male == "yellow"][1:length(len$length[len$male == "yellow"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)

polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2.2, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(0.8, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext(c("Paired mean difference"),  side =4, line = 2.5)

dev.off()



#### multi group unpaired plot ####

## create another colum in male name orange and create length data with 19 sample, 33 mean and 1 sd
df <- data.frame(rnorm(19, 33, 1), rep("orange",19), stringsAsFactors = TRUE)

colnames (df) <- c("length", "male")

len2 <- rbind(len,df)

## do calculation 
str(len2)

set.seed(12345)

contrast <- c("yellow", "red")
contrast2 <- c("red", "orange")

a <- bootES(len2, R =10000, 
            data.col = "length", contrast = contrast,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)

b <- bootES(len2, R =10000, 
            data.col = "length", contrast = contrast2,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)

d <- density(a$t)
e <- density(b$t)


plot(d$y, d$x, 
     axes = FALSE,
     type = "n")
polygon(d$y, d$x, col="red", border="red")

## box + jitter multi plot  
l <- layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE),
            heights = c(1,0.8))
layout.show(l)
par(mar=c(4,5,1,2))

##add boxplot with individual data point with jitter  
boxplot(length~male, data = len2,
        col = yarrr::transparent(c("blue", "red", "purple"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE)

stripchart(length~male, data = len2, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red", "purple"), trans.val = .5),
           add = TRUE)

## add effect size in lower panel using density plot

plot(NULL,xlim = c(0, max(range(d$y)+1.5)), ylim = range(d$x),
     type ="n", axes = FALSE, ylab = "Paired mean difference", xlab = "")

##
polygon(1.5+d$y, d$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))


points (1.5,a$t0, pch = 16, col = "blue", cex = 1.5)
segments(1.5, a$bounds[1], 1.5, a$bounds[2], col = "blue", lty =1, lwd=2)

axis(2, at = pretty(range(a$t)),
     labels = pretty(range(a$t)))
abline(h = 0)

axis(1, at = 1.5+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

##
polygon(e$y, e$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (0,b$t0, pch = 16, col = "blue", cex = 1.5)
segments(0, b$bounds[1], 0, b$bounds[2], col = "blue", lty =1, lwd=2)

axis(1, at = max(e$y)/2,
     labels = sprintf("%s minus %s", contrast2[2], contrast2[1]))

#box()

dev.off()

## violin + jitter multi group plot 

l <- layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE),
            heights = c(1,0.8))
layout.show(l)
par(mar=c(4,5,1,2))

##add boxplot with individual data point with jitter  
#vioplot(length~male, data = len2,side = "left",
        # col = yarrr::transparent(c("blue", "red", "purple"), trans.val = .6),
        # border = yarrr::transparent(c("blue", "red", "purple"), trans.val = .6),
        # las = 1, staplewex= 0.4, outline = FALSE, 
        # rectCol = "NA",
        # lineCol = "NA",
        # colMed = "NA")

## 

h <- density(len2$length[len2$male == "red"], adjust = 1.5)

f <- density(len2$length[len2$male == "yellow"],
             bw = "SJ")

g <- density(len2$length[len2$male == "orange"],
             bw = "SJ")

plot(NULL,xlim = c(0.5,3.5), ylim = range(c(f$x, g$x)),
     type ="n", axes = FALSE, ylab = "length")

# polygon(2-h$y, h$x,
#         col=yarrr::transparent("red", trans.val = .6), 
#         border=yarrr::transparent("red", trans.val = .6))
polygon(c(2-h$y, 2+rev(h$y)), c(h$x, rev(h$x)),
        col=yarrr::transparent("red", trans.val = .6), 
        border=yarrr::transparent("red", trans.val = .6))

polygon(3-f$y, f$x,
        col=yarrr::transparent("purple", trans.val = .6), 
        border=yarrr::transparent("purple", trans.val = .6))

polygon(1-g$y, g$x,
        col=yarrr::transparent("blue", trans.val = .6), 
        border=yarrr::transparent("blue", trans.val = .6))


stripchart(length~male, data = len2, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red", "purple"), trans.val = .5),
           add = TRUE)

axis(2, at = pretty(range(len2$length)),
     labels = pretty(range(len2$length)))

axis(1, at = c(1,2,3),
     labels = c("orange", "red", "yellow"))

box()
## add effect size in lower panel using density plot

plot(NULL,xlim = c(0, max(range(d$y)+1.5)), ylim = range(d$x),
     type ="n", axes = FALSE, ylab = "Paired mean difference", xlab = "")

##
polygon(1.5+d$y, d$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))


points (1.5,a$t0, pch = 16, col = "blue", cex = 1.5)
segments(1.5, a$bounds[1], 1.5, a$bounds[2], col = "blue", lty =1, lwd=2)

axis(2, at = pretty(range(a$t)),
     labels = pretty(range(a$t)))
abline(h = 0)

axis(1, at = 1.5+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

##
polygon(e$y, e$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (0,b$t0, pch = 16, col = "blue", cex = 1.5)
segments(0, b$bounds[1], 0, b$bounds[2], col = "blue", lty =1, lwd=2)

axis(1, at = max(e$y)/2,
     labels = sprintf("%s minus %s", contrast2[2], contrast2[1]))

dev.off()

#### multi pair-group unpaired plot ####

## create another colum in male name orange and create length data with 19 sample, 33 mean and 1 sd
df2 <- data.frame(rnorm(19, 35, 2), rep("black",19), stringsAsFactors = TRUE)

colnames (df2) <- c("length", "male")

len3 <- rbind(len2,df2)

## do calculation 
str(len3)

set.seed(12345)

contrast3 <- c("orange", "black")
contrast4 <- c("yellow", "red")

m <- bootES(len3, R =10000, 
            data.col = "length", contrast = contrast3,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)

n <- bootES(len3, R =10000, 
            data.col = "length", contrast = contrast4,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)

o <- density(m$t)
r <- density(n$t)
str(d)

## start plotting 
l <- layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE),
            heights = c(1,0.8))
layout.show(l)
par(mar=c(4,5,1,2))

##add boxplot with individual data point with jitter  
boxplot(length~male, data = len3,
        col = yarrr::transparent(c("blue", "red", "purple", "orange"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, at = c(1,2,4,5))

stripchart(length~male, data = len3, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red", "purple", "orange"), 
           at = c(1,2,4,5), trans.val = .5), add = TRUE)

## task: how to add jitter at desired position using at 
## add effect size in lower panel using density plot

plot(NULL,xlim = c(0, 1+length(unique(len3$male))), ylim = range(o$x),
     type ="n", axes = FALSE, ylab = "Paired mean difference", xlab = "")

##
polygon(4.0+r$y, r$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (4.0,n$t0, pch = 16, col = "blue", cex = 1.5)
segments(4.0, n$bounds[1], 4.0, n$bounds[2], col = "blue", lty =1, lwd=2)

#axis(2, at = pretty(range(n$t)),
   #  labels = pretty(range(n$t)))
abline(h = 0)

axis(1, at = 4.0+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast4[2], contrast4[1]))

##
polygon(1+o$y, o$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (1,m$t0, pch = 16, col = "blue", cex = 1.5)
segments(1, m$bounds[1], 1, m$bounds[2], col = "blue", lty =1, lwd=2)

axis(2, at = pretty(range(m$t)),
     labels = pretty(range(m$t)))

axis(1, at = max(o$y)/2,
     labels = sprintf("%s minus %s", contrast3[2], contrast3[1]))

box()

## task for ploting the y axis label: 
#do maths between groups, find the max and min and plot y axis lebel, especially for the bottom density
dev.off()


#### multi pair-group paired plot ####

## create another colum in male name orange and create length data with 19 sample, 33 mean and 1 sd
df2 <- data.frame(rnorm(19, 35, 2), rep("black",19), stringsAsFactors = TRUE)

colnames (df2) <- c("length", "male")

len3 <- rbind(len2,df2)

## do calculation 
str(len3)

set.seed(12345)

contrast3 <- c("orange", "black")
contrast4 <- c("yellow", "red")

m <- bootES(len3, R =10000, 
            data.col = "length", contrast = contrast3,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)

n <- bootES(len3, R =10000, 
            data.col = "length", contrast = contrast4,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)

o <- density(m$t)
r <- density(n$t)
str(d)

## start plotting 
l <- layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE),
            heights = c(1,0.8))
layout.show(l)
par(mar=c(4,5,1,2))

##add boxplot with individual data point with jitter  
boxplot(length~male, data = len3,
        col = yarrr::transparent(c("blue", "red", "purple", "orange"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, at = c(1,2,3,4))

stripchart(length~male, data = len3,
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red", "purple", "orange"), 
           trans.val = .5), add = TRUE)

segments(1.0,len3$length[len3$male == "black"][1:length(len3$length[len3$male == "black"])], 
         2.0, len3$length[len3$male == "orange"][1:length(len3$length[len3$male == "orange"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)

segments(3.0,len3$length[len3$male == "red"][1:length(len3$length[len3$male == "red"])], 
         4.0, len3$length[len3$male == "yellow"][1:length(len3$length[len3$male == "yellow"])], 
         col = yarrr::transparent(c("grey20"), trans.val = .7),
         lty =1, lwd=2)


## task: how to add jitter at desired position using at 
## add effect size in lower panel using density plot

plot(NULL,xlim = c(0, 1+length(unique(len3$male))), ylim = range(o$x),
     type ="n", axes = FALSE, ylab = "Paired mean difference", xlab = "")

##
polygon(4.0+r$y, r$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (4.0,n$t0, pch = 16, col = "blue", cex = 1.5)
segments(4.0, n$bounds[1], 4.0, n$bounds[2], col = "blue", lty =1, lwd=2)

#axis(2, at = pretty(range(n$t)),
#  labels = pretty(range(n$t)))
abline(h = 0)

axis(1, at = 4.0+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast4[2], contrast4[1]))

##
polygon(1+o$y, o$x,
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (1,m$t0, pch = 16, col = "blue", cex = 1.5)
segments(1, m$bounds[1], 1, m$bounds[2], col = "blue", lty =1, lwd=2)

axis(2, at = pretty(range(m$t)),
     labels = pretty(range(m$t)))

axis(1, at = max(o$y)/2,
     labels = sprintf("%s minus %s", contrast3[2], contrast3[1]))



## task for ploting the y axis label: 
#do maths between groups, find the max and min and plot y axis lebel, especially for the bottom density
dev.off()


####old codes ####

# create layout for Gardner-Altman plot

l <- layout(matrix(c(1,2), nrow=1,ncol=2,byrow =  TRUE),
            widths = c(1,0.8))

layout.show(l)

par(mar=c(4,5,1,2))

##add boxplot with individual data point with jitter  
boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE)

stripchart(length~male, data = len, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE)

#abline (h = 32.5)
#abline (h = 31.5)

## add effect size in cplit violin plot 
#par(new = TRUE) 
vioplot(a$t, side = "right",
        rectCol = "NA",
        lineCol = "NA",
        colMed = "NA",
        cex = 1.5,
        border = yarrr::transparent("purple", trans.val = .7),
        pchMed = 16,
        plotCentre = "points",
        col = yarrr::transparent("purple", trans.val = .7),
        ylab = "mean difference",
        axes = FALSE,
        las = 1, at = 2.5)

axis(4, at = pretty(range(a$t)))
abline(0,0)

points (1,a$t0, pch = 16, col = "blue", cex = 1.5)
segments(1, 0.239, 1, 1.566, col = "blue", lty =1, lwd=2)


dev.off()

#### into one plot 

par(mar=c(4,5,1,2)+ 0.5)

##add boxplot with individual data point with jitter  
boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE)

stripchart(length~male, data = len, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE)

#abline (h = 32.5)
#abline (h = 31.5)

segments(1, 32.5, 2.5, 32.5, col = "blue", lty =1, lwd=2)

points (2.5,32.5, pch = 16, col = "blue", cex = 1.5)

segments(2.5, 32.5 -0.239 , 2.5, 32.5+1.566, col = "blue", lty =1, lwd=2)
a

## add effect size in cplit violin plot 


boxplot(length~male, data = len,
        col = yarrr::transparent(c("blue", "red"), trans.val = .6),
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        xlim = c(0,4.5), at = c(1,2))

stripchart(length~male, data = len, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

#par(new = TRUE) 
p <- mean(len$length[len$male=="yellow"])
polygon(d$y+3.0, d$x+p, col="red", border="red")
points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2+0.4, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

dev.off()


##

