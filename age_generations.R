library(tidyverse)
library(haven)
library(car)

gss <- read_dta("D:/reltrad.dta")

df <- filter(gss, year >=2010) %>% select(year, age, reltrad, abdefect, abnomore, abhlth, abpoor, abrape, absingle, abany)


#df$age <- as_factor(df$age)
#df$age <- as.numeric(df$age)

df <- na.omit(df)

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

cut(df$age, breaks =3)
df$gen <- recode(df$age, "17.9:41 =1; 42:65=2; 65:90=3")

a1 <- df %>% group_by(gen, reltrad) %>% summarise(defect = mean(abdefect, na.rm =TRUE), nomore = mean(abnomore, na.rm =TRUE), health = mean(abhlth, na.rm =TRUE), poor = mean(abpoor, na.rm =TRUE), rape = mean(abrape, na.rm =TRUE), single = mean(absingle, na.rm =TRUE), any= mean(abany, na.rm =TRUE))

a1$reltrad[a1$reltrad ==1] <- "Evangelical"
a1$reltrad[a1$reltrad ==2] <- "Mainline"
a1$reltrad[a1$reltrad ==3] <- "Black Protestant"
a1$reltrad[a1$reltrad ==4] <- "Catholic"
a1$reltrad[a1$reltrad ==5] <- "Jewish"
a1$reltrad[a1$reltrad ==6] <- "Other"
a1$reltrad[a1$reltrad ==7] <- "No Religion"
a1$reltrad <- as_factor(a1$reltrad)

ab1 <- melt(a1, id.vars=c("gen", "reltrad"))

ab1$gen[ab1$gen ==1] <- "18-41 years old"
ab1$gen[ab1$gen ==2] <- "42-65 years old"
ab1$gen[ab1$gen ==3] <- "Over 65 years old"

ab1$variable <- factor(ab1$variable, labels = c("Birth Defect", "No More Kids", "Mother's Health", "Poverty", "Rape", "Unmarried", "Any Reason"))



p1 <- ggplot(ab1 %>% filter(gen == "18-41 years old"), aes(x = value*100, y = variable))  +
  geom_point(shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  + scale_fill_brewer(palette = "Set2") + 
  ylab("Abortion Situation") + xlab("Percent in Favor of Abortion") +
  ggtitle("18-41 years old")+ 
  theme(text=element_text(size=16, family="KerkisSans")) + xlim(0,100)  + guides(fill=FALSE)

p2 <- ggplot(ab1 %>% filter(gen == "42-65 years old"), aes(x = value*100, y = variable))  +
  geom_point(shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  + scale_fill_brewer(palette = "Set2") + 
  ylab("Abortion Situation") + xlab("Percent in Favor of Abortion") +
  ggtitle("42-65 years old")+ 
  theme(text=element_text(size=16, family="KerkisSans")) + xlim(0,100) + guides(fill=FALSE)


p3 <- ggplot(ab1 %>% filter(gen == "Over 65 years old"), aes(x = value*100, y = variable))  +
  geom_point(shape=21, size =4, aes(fill = factor(reltrad))) +  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom")  + scale_fill_brewer(palette = "Set2") + 
  ylab("Abortion Situation") + xlab("Percent in Favor of Abortion") +
  ggtitle("Over 65 years old")+ 
  theme(text=element_text(size=16, family="KerkisSans")) + xlim(0,100) + 
  annotate("text", x = 8, y = 1.25, label = "religioninpublic.blog", size = 4)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))

