
#task: 

### task : put file sin git hub 
## cogen's d, heghen's g; one sample t test  
## write possible way of plotting-done  
##polygon to violin 
## check violin code in github to determine how they made the violin smooth -done, but logic

## what if we chop the head and tail a bit to make the effect plot longer or 
## any other way to make effect plot nicer 

difference <- (data,
               type = c("unstandarrised", "cohens", "hegens"), # select type of difference #deffault unstandarised
               paired = FALSE, # if true calculate paired mean difference 
               ci.type = "bca", #default
               R = 10000, #default
               contrast = c("group 1" - "group 2"), # default larger group minus small group
               ci.conf = 0.95, #default)

## output= raw data + bootstrapped mean difference + conffidence interval off mean difference + individual bootstrapped difference + CI of group 1 +CI of group 2 +mean of group 1 + mean pf group 2 + median of group 1 + median of group 2              
## print bootstrap mean difference, bootsrapped confidence interval (R value, bootstrapped corrections"bca")



jk_plot(data, 
        box = TRUE,# draw boxplot 
        box_fill = TRUE,# fill up box colour # if false only border will be drawn 
        points = TRUE, #add individual data point 
        density = TRUE, # add half violin; if desnity is TRUE, violin is false  
        violin = TRUE, # add violin; if violin is true density is false 
        mean = TRUE, #draw mean of the data 
        CI = TRUE, # draw confidence interval line of the data; if, box, density, violin is TRUE, CI is FALSE
        median_line = FALSE, # if TRUE horizontal line in median is drawn # if, box, density, violin is TRUE, median_line is FALSE
        ef_size =TRUE, # if false do not plot effect size
        ef_size_density =TRUE, #if true draw effect size conffidence interbval
        ef_size_position = c( "right/Gardner-Altman_plot", "down/Cummings_plot"),# when Gardner-Altman_plot is choosen effect size plotted right, otherwise down 
        paired = FALSE, # if true draw lines between paired points
        xlab = "",
        left_ylab = "",
        right_ylab = "",
        bottom_ylab = "",
        col = c("col1", "col2", "col3"), shade = 0.6, #colour of box, violin, box border, density, col 1 = group 1, col2 = group 2, col3 = ef plot, {col = n+1, n = group no) #   
        points_col = c("col1", "col2", "col3"), shade = 0.4, # points colour 
          
        

#### calculate bootstrap ####
set.seed(12345)

library(boot)
library(bootES)
library(dabestr)

data(city)
head(city)
head(gravity)
str(gravity)



#### dabestr ####

library(dplyr)

set.seed(54321)

N = 40
c1 <- rnorm(N, mean = 100, sd = 25)
c2 <- rnorm(N, mean = 100, sd = 50)
g1 <- rnorm(N, mean = 120, sd = 25)
g2 <- rnorm(N, mean = 80, sd = 50)
g3 <- rnorm(N, mean = 100, sd = 12)
g4 <- rnorm(N, mean = 100, sd = 50)
gender <- c(rep('Male', N/2), rep('Female', N/2))
dummy <- rep("Dummy", N)
id <- 1: N


wide.data <- 
  tibble::tibble(
    Control1 = c1, Control2 = c2,
    Group1 = g1, Group2 = g2, Group3 = g3, Group4 = g4,
    Dummy = dummy,
    Gender = gender, ID = id)

my.data   <- 
  wide.data %>%
  tidyr::gather(key = Group, value = Measurement, -ID, -Gender, -Dummy)


head(my.data, 25)
my.data$Measurement[my.data$Group == "Group1"][1]



library(dabestr)

two.group.unpaired <- 
  my.data %>%
  dabest(Group, Measurement, 
         # The idx below passes "Control" as the control group, 
         # and "Group1" as the test group. The mean difference
         # will be computed as mean(Group1) - mean(Control1).
         idx = c("Control1", "Group1"), 
         paired = FALSE)

two.group.unpaired.meandiff <- mean_diff(two.group.unpaired)

two.group.paired <- 
  my.data %>%
  dabest(Group, Measurement, 
         idx = c("Control1", "Group1"), 
         paired = TRUE, id.col = ID)


write.csv(my.data, "my.data.csv")
pair_diff <- len$length[len$male == "red"][1:length(len$length[len$male == "red"])]- 
  len$length[len$male == "yellow"][1:length(len$length[len$male == "yellow"])]

meanDiff = function(dataFrame, indexVector) { 
  m1 = mean(subset(dataFrame[indexVector, 1], dataFrame[indexVector, 2] == "initial"))
  m2 = mean(subset(dataFrame[indexVector, 1], dataFrame[indexVector, 2] == "final"))
  m = m1 - m2
  return(m)
}  

#### bootES #### 

set.seed(12345)
contrast <- c("7", "8")
bootES(grav1, R =10000,
       data.col = "g", contrast = contrast,
       group.col = "series", effect.type = "unstandardized",
       ci.conf = 0.95)

### 

data("aircondit")
head(aircondit)
#### transparency code in base R ####
set.seed(12345)

# colour transparency 

col2rgb("red")

mycol <- rgb(255, 0, 0, max = 255, alpha = 125)

boxplot(rnorm(100), col = "red")
boxplot(rnorm(100), col = mycol)

col2rgb("red")[1]

mycol2 <- rgb(col2rgb("red")[1],  col2rgb("red")[2], col2rgb("red")[3], max = 255, alpha = 125)

jk_col <-  function(colour, shade) {
  rgb.val <- col2rgb(colour)
  t.col <- rgb(rgb.val[1, ], rgb.val[2, ], rgb.val[3, ],
               max = 255,
               alpha = (100 - shade*100) * 255 / 100)
  invisible(t.col)
}

boxplot(rnorm(100), col = jk_col ("blue", shade = 0.6))

boxplot(rnorm(100), col = "red")

boxplot(rnorm(100), col = jk_col("red", shade = 0.4))
boxplot(rnorm(100), col = yarrr::transparent("red", trans.val = .4))


## 
l <- layout(matrix(c(1,2), nrow=2,ncol=1,byrow =  TRUE),
            heights = c(1,1))
layout.show(l)
par(mar=c(4,5,2,4))




#A) 

boxplot(length~male, data = len,
        col = "NA",
        border = k_col(c("blue", "red"), shade = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        xlim = c(0,4.0), at = c(1,2), lwd =1.5)

stripchart(length~male, data = len, method = "jitter",
           pch = 19, vertical = TRUE,
           col = k_col(c("blue", "red"), shade = .6),
           add = TRUE, at = c(1,2))

#

boxplot(length~male, data = len,
        col = "NA",
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.4, outline = FALSE, 
        xlim = c(0,4.0), at = c(1,2), lwd =1.5)

stripchart(length~male, data = len, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))

#par(new = TRUE) 
p <- mean(len$length[len$male=="yellow"])
polygon(d$y/max(d$y)+3.0, d$x+p, 
        col=yarrr::transparent("orange", trans.val = .3), 
        border=yarrr::transparent("orange", trans.val = .3))

points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)), las = 1)

segments(2+0.4, p, 5, p, col = "blue", lty =1, lwd=1)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=1)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext("Unpaired mean difference",  side =4, line = 2.5)


#B) 



boxplot(length~male, data = len,
        col = "NA",
        border = yarrr::transparent(c("blue", "red"), trans.val = .6),
        las = 1, staplewex= 0.35, outline = FALSE, 
        xlim = c(0.5,3.5), at = c(1,2), lwd =1.5)

stripchart(length~male, data = len, method = "jitter",
           pch = 19, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .5),
           add = TRUE, at = c(1,2))


polygon(d$y/(max(d$y)*2)+2.8, d$x+p, 
col=yarrr::transparent("orange", trans.val = .3), 
border=yarrr::transparent("orange", trans.val = .3))

points (2.8,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(2.8, a$bounds[1]+p, 2.8, a$bounds[2]+p, col = "blue", lty =1, lwd=2)

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)), las = 1)

segments(2+0.4, p, 5, p, col = "blue", lty =1, lwd=1)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=1)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

mtext("Unpaired mean difference",  side =4, line = 2.5)


dev.off()








###test

set.seed(12345)

a <- rnorm(10, 5, 2)
b <- rnorm(10, 8, 2)
c <- rep(c("be","af"), 5)

df <- cbind.data.frame(a,b,c)

str(df)
head(df)

boxplot(a~c, data = df, xlim = c(1,4), at = c(1.5,3.5),
        outline = FALSE)

stripchart(a~c, data = df, add = TRUE,
           xlim = c(1,4), at = c(1.8,3.2),
           vertical = TRUE, pch = 16)

df$a[df$c == "be"][1:5]
df$a[df$c == "af"][1:5]

segments(1.8,df$a[df$c == "af"][1:5], 3.2, df$a[df$c == "be"][1:5], 
         col = yarrr::transparent(c("blue"), trans.val = .8),
         lty =1, lwd=1)

dev.off()

#### stricptchart and lines

df <- data.frame(pre = runif(20), post = runif(20))

stripchart(x = list(df$pre, df$post), vertical=TRUE,
           group.names=c("Pre","Post"), method = "jitter")

for(i in 1:nrow(df)){segments(1, df$pre[i], 2, df$post[i])}


##
### plot idea 
#eplot(a,b, plot = c(plota = box+jitter, plotb = violin + jitter, plotc = box+violin+jitter), effectplot = c(density, mean_ci), 
## effect_size = c(unstandarised, cohen's, hegens's), col = "", paired = (TRUE, FALSE))



##box and jitter 

a <- bootES(len, R =10000, 
            data.col = "length", contrast = contrast,
            group.col = "male",
            effect.type = "unstandardized", plot = TRUE)
d <- density(a$t)


####


e <- density(len$length[len$male == "red"],
             bw = "SJ")

f <- density(len$length[len$male == "yellow"],
             bw = "SJ")

p <- mean(len$length[len$male=="yellow"])
q <- mean(len$length[len$male=="red"])

plot(NULL,xlim = c(0,4.5), ylim = range(c(e$x, f$x)),
     type ="n", axes = FALSE)

polygon(1-e$y, e$x,
        col=yarrr::transparent("blue", trans.val = .5), 
        border=yarrr::transparent("blue", trans.val = .5))
polygon(2-f$y, f$x,
        col=yarrr::transparent("red", trans.val = .5), 
        border=yarrr::transparent("red", trans.val = .5))

points(1, q, pch = 19, cex = 1.5)
points(2, p, pch = 19, cex = 1.5)

stripchart(length~male, data = len, method = "jitter",
           pch = 16, vertical = TRUE,
           col = yarrr::transparent(c("blue", "red"), trans.val = .3),
           add = TRUE,at = c(1,2))

axis(2, at = pretty(range(len$length)),
     labels = pretty(range(len$length)))


points (3,p+a$t0, pch = 16, col = "blue", cex = 1.5)

segments(3, a$bounds[1]+p, 3, a$bounds[2]+p, col = "blue", lty =1, lwd=2)
polygon(d$y+3.0, d$x+p,
        col=yarrr::transparent("orange", trans.val = .5), 
        border=yarrr::transparent("orange", trans.val = .5))

axis(4, at = pretty(range(a$t))+p,
     labels = pretty(range(a$t)))

segments(2+0.4, p, 5, p, col = "blue", lty =1, lwd=2)### 
segments(1+0.4, p+a$t0, 5, p+a$t0, col = "blue", lty =1, lwd=2)###

axis(1, at = 3+max(d$y)/2,
     labels = sprintf("%s minus %s", contrast[2], contrast[1]))

axis(1, at = c(1,2),
     labels = c("red", "yellow"))
box()

dev.off()







