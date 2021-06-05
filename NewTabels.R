AIC_Extract <- function(Model){
  
  q <- lapply(from_zelig_model(Model), AIC)
  
  q <- as.data.frame(q)
  
  q <- round(rowMeans(q),2)
  
  return(q)
  
}


Models_UCDP <- list(UCDP_1, UCDP_2, UCDP_3, UCDP_4, UCDP_5, UCDP_6)
Models_UCDP_FE <- list(UCDP_1_FE, UCDP_2, UCDP_3, UCDP_4, UCDP_5,  UCDP_6)


Models_MID <- list(MID_1, MID_2, MID_3, MID_4, MID_5, MID_6)
Models_MID_FE <- list(MID_1_FE, MID_2_FE, MID_3_FE, MID_4_FE, MID_5_FE, MID_6_FE)

aic_list_UCDP <- lapply(Models_UCDP, AIC_Extract)
aic_list_UCDP_FE <- lapply(Models_UCDP_FE, AIC_Extract)

aic_list_MID <- lapply(Models_MID, AIC_Extract)
aic_list_MID_FE <- lapply(Models_MID_FE, AIC_Extract)


