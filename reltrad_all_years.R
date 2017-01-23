library(tidyverse)
library(haven)
library(car)

gss <- read_dta("D:/reltrad.dta")



df <- select(gss, year, reltrad, sex, abdefect, abnomore, abhlth, abpoor, abrape, absingle, abany)
df$abdefect <- as_factor(df$abdefect)
df$abdefect <- as.numeric(df$abdefect)
df$abnomore <- as_factor(df$abnomore)
df$abnomore <- as.numeric(df$abnomore)
df$abhlth <- as_factor(df$abhlth)
df$abhlth <- as.numeric(df$abhlth)
df$absingle <- as_factor(df$absingle)
df$absingle <- as.numeric(df$absingle)
df$abany <- as_factor(df$abany)
df$abany <- as.numeric(df$abany)
df$abrape <- as_factor(df$abrape)
df$abrape <- as.numeric(df$abrape)

df$abdefect[df$abdefect >=3] <- NA
df$abnomore[df$abnomore >=3] <- NA
df$abhlth[df$abhlth >=3] <- NA
df$absingle[df$absingle >=3] <- NA
df$abany[df$abany >=3] <- NA
df$abrape[df$abrape >=3] <- NA

df$abdefect <- recode(df$abdefect, "1=1; 2=0")
df$abnomore <- recode(df$abnomore, "1=1; 2=0")
df$abhlth <- recode(df$abhlth, "1=1; 2=0")
df$abpoor <- recode(df$abpoor, "1=1; 2=0")
df$absingle <- recode(df$absingle, "1=1; 2=0")
df$abany <- recode(df$abany, "1=1; 2=0")
df$abrape <- recode(df$abrape, "1=1; 2=0")


abort <- df %>% group_by(year, reltrad) %>% summarise(defect = mean(abdefect, na.rm =TRUE), nomore = mean(abnomore, na.rm =TRUE), health = mean(abhlth, na.rm =TRUE), poor = mean(abpoor, na.rm =TRUE), rape = mean(abrape, na.rm =TRUE), single = mean(absingle, na.rm =TRUE), any= mean(abany, na.rm =TRUE))
a2 <- melt(abort, id.vars=c("year", "reltrad"))

abort <- df %>% group_by(year, sex) %>% summarise(defect = mean(abdefect, na.rm =TRUE), nomore = mean(abnomore, na.rm =TRUE), health = mean(abhlth, na.rm =TRUE), poor = mean(abpoor, na.rm =TRUE), rape = mean(abrape, na.rm =TRUE), single = mean(absingle, na.rm =TRUE), any= mean(abany, na.rm =TRUE))
a2 <- melt(abort, id.vars=c("year", "sex"))

a2$reltrad[a2$reltrad ==1] <- "Evangelical"
a2$reltrad[a2$reltrad ==2] <- "Mainline"
a2$reltrad[a2$reltrad ==3] <- "Black Protestant"
a2$reltrad[a2$reltrad ==4] <- "Catholic"
a2$reltrad[a2$reltrad ==5] <- "Jewish"
a2$reltrad[a2$reltrad ==6] <- "Other"
a2$reltrad[a2$reltrad ==7] <- "No Religion"
a2$reltrad <- as_factor(a2$reltrad)

a2 <- Nth.delete(a2, 8)
a2 <- filter(a2, year != 1986)


ggplot(a2 %>% filter(variable == "nomore"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent in Favor of Abortion") + 
  ggtitle("If she is married and does not want any more children?") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100)+  
  scale_colour_brewer("Religious Tradition", palette="Set2") 

ggplot(a2 %>% filter(variable == "defect"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent in Favor of Abortion") + 
  ggtitle("If there is a strong chance of serious defect in the baby?") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100)+  
  scale_colour_brewer("Religious Tradition", palette="Set2") 

ggplot(a2 %>% filter(variable == "health"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent in Favor of Abortion") + 
  ggtitle("If the woman''s own health is seriously endangered by the pregnancy?") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100)+  
  scale_colour_brewer("Religious Tradition", palette="Set2") 

ggplot(a2 %>% filter(variable == "poor"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent in Favor of Abortion") + 
  ggtitle("If the family has a very low income and cannot afford any more children?") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100)+  
  scale_colour_brewer("Religious Tradition", palette="Set2") 

ggplot(a2 %>% filter(variable == "rape"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent in Favor of Abortion") + 
  ggtitle("If she became pregnant as a result of rape?") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans")) + ylim(0,100)+  
  scale_colour_brewer("Religious Tradition", palette="Set2") 

ggplot(a2 %>% filter(variable == "single"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent in Favor of Abortion") + 
  ggtitle("If she is not married and does not want to marry the man?") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100)+  
  scale_colour_brewer("Religious Tradition", palette="Set2") 

ggplot(a2 %>% filter(variable == "any"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent in Favor of Abortion") + 
  ggtitle("The woman wants it for any reason?") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100) +  
  scale_colour_brewer("Religious Tradition", palette="Set2") 








