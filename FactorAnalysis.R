library(tidyverse)

Fact <- WVS_EVS %>%
  select(starts_with("Value")) %>%
  drop_na()



FacAn <- factanal(Fact, 2, rotation = "promax")

# plot(Test_FA$loadings[,1],
#      Test_FA$loadings[,2],
#      xlab = "Factor",
#      ylim = c(-1,1),
#      xlim = c(-1,1),
#      main = "No Rotation")
# abline(h = 0, v = 0) 

plot(FacAn$loadings[,1],
     FacAn$loadings[,2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     xlim = c(-1,1),
     ylim = c(-1,1),
     main = "ProMax Rotation")
  abline(h = 0, v = 0)
  text(FacAn$loadings[,1],
       FacAn$loadings[,2], 
       rownames(FacAn$loadings),
       offset = 1.2,
       cex = .6
       )
