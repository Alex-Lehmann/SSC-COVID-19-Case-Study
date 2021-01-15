library(tidyverse)

incRank = c(1,2,3,4,5)
decRank = c(1,2,3,4,5)

inc = c(0.0112771051, 0.0106240204, 0.0084900587, 0.0032887984, 0.0018486792)
dec = c(-0.0069268028, -0.0058739493, -0.0022949690, -0.0012208098, -0.0007035862)

coefDF = tibble(Rank = c(decRank, incRank),
                `Coefficient Magnitude` = c(abs(dec), inc),
                Type = c(rep("Decreasing Factor", 5), rep("Increasing Factor", 5)))

ggplot(coefDF, aes(x=Rank, y=`Coefficient Magnitude`, fill=Type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual("Type", values=c("#147e3b", "#bb141a"))
