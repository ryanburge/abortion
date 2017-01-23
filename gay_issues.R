library(tidyverse)
library(haven)
library(car)

gss <- read_dta("D:/reltrad.dta")

df <- select(gss, year, reltrad, marhomo, homosex)

df$marhomo <- as_factor(df$marhomo)
df$marhomo <- as.numeric(df$marhomo)
df$homosex <- as_factor(df$homosex)
df$homosex <- as.numeric(df$homosex)


df$marhomo[df$marhomo >=6] <- NA
df$homosex[df$homosex >=6] <- NA

df$marhomo <- recode(df$marhomo, "1=1; 2=1; 3=0; 4=0; 5=0")
df$homosex <- recode(df$homosex, "4=1; 1=0; 2=0; 3=0; 5=0")



gay <- df %>% group_by(year, reltrad) %>% summarise(marriage = mean(marhomo, na.rm =TRUE), gaysex = mean(homosex, na.rm =TRUE))
a2 <- melt(gay, id.vars=c("year", "reltrad"))

a2$reltrad[a2$reltrad ==1] <- "Evangelical"
a2$reltrad[a2$reltrad ==2] <- "Mainline"
a2$reltrad[a2$reltrad ==3] <- "Black Protestant"
a2$reltrad[a2$reltrad ==4] <- "Catholic"
a2$reltrad[a2$reltrad ==5] <- "Jewish"
a2$reltrad[a2$reltrad ==6] <- "Other"
a2$reltrad[a2$reltrad ==7] <- "No Religion"
a2$reltrad <- as_factor(a2$reltrad)

a2 <- Nth.delete(a2, 8)

ggplot(a2 %>% filter(variable == "marriage"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Percent Who Strongly Agree or Agree") + 
  ggtitle("Homosexual couples should have the right to marry one another.") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100)+  xlim(2006, 2014) +
  scale_colour_brewer("Religious Tradition", palette="Set2") 

ggplot(a2 %>% filter(variable == "gaysex"), aes(x=year, y=value*100, label = reltrad, color =reltrad)) + 
  geom_line(aes(group = reltrad), size=1.5 ) + xlab("Year") + ylab("Not Wrong At All (Percent)") + 
  ggtitle("What about sexual relations between two adults of the same sex") + labs(colour = "Religious Tradition") +
  theme(legend.position="bottom")  + 
  theme(text=element_text(size=16, family="KerkisSans"))+ ylim(0,100) + 
  scale_colour_brewer("Religious Tradition", palette="Set2") 



