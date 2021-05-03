VAR <- c(3,5,6,7,8,9,10,11,12,13,14,15,16,17,18, 19, 21,23, 27, 28, 29, 30,
         31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46)

for (i in VAR) {
  tryCatch({
    
    png(
      paste("OverImputedAp/", names(ImputedData[[1]][[1]][i]), "OI.png", sep = ""),
      width = 862,
      height = 538
    )
    
    overimpute(ImputedData, var= i, subset = year > 1990,
               main = paste("Observed and Imputed Values of:",
                            names(ImputedData[[1]][[1]][i]), sep = " ")
                 )
    

    
    dev.off()
  }, error = function(e){})
}


overimpute(ImputedData, "ValueGodTim", 4, subset = year > 1990, main = "Imputed versus Observed Value: Value Good Time (WVS A192)")

compare.density(ImputedData, 4,
                main = paste("Observed and Imputed Values of:",
                             names(ImputedData[[1]][[1]][4]), sep = " "))
