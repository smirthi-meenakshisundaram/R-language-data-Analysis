#setwd("~/Desktop/SPL_Project/")
project <- read.csv(file = 'dataset-forgraphs.csv', header = TRUE)
head(project)
install.packages("ggplot2")
library("ggplot2")
piechart <- table(project$state)
head(piechart)
png(file = "piechart_of_status.jpg")
data <- data.frame(
  status =c("canceled","failed","live","successful"," suspended","undefined"),
  countvalue =c(38779,197719,2799,133956,1846,3562)
)

# Basic piechart which shows the different states of projects
ggplot(data, aes(x="", y=countvalue, fill=status)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()
dev.off()

#barplot of the no. of projects in each category
dat <- table(project$main_category)
head(dat,15)
png(file = "barchart_projectcategories.jpg")
#barplot((sort(dat, decreasing = TRUE)),main = "Projects in different categories",xlab = "Categories", ylab = "count of projects", col = "blue")
#print(barplot)
ggplot(data = project) + geom_bar(mapping = aes(x = main_category,fill=main_category))
dev.off()

#box plot of backers vs state
png(file = "boxplot_backers-vs-projectstatus.jpg")
plot2<-ggplot(project, aes(x=state, y=backers)) +
  geom_boxplot()
print(plot2)
dev.off()


install.packages("dplyr")
library("dplyr")
seplot <- project %>% filter(
  state == "successful"
)
#ggplot(project, aes(x=pledged, y=goal)) + geom_point()

#Team Member: Sai Krishna Mannava (801136361)
#Team Member:Kumar Mani Chandra Yelisetty (801168244)
#Team Member:Smirthi Meenakshisundaram (801129947)
