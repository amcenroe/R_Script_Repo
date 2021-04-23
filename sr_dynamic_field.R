sr_dynamic_field <- function(SR_DT_FILT) {
  # Processing Dynamic Field
  SR_DT_1_LBL <- SR_DT_FILT  %>% select(contains('Dynamic Field Label'))
  SR_DT_1_VAL <- SR_DT_FILT  %>% select(contains('Dynamic Field Value'))
  SR_DT_1_LBL <- cbind(select(SR_DT_FILT ,`ServiceRequest No`,Combined),SR_DT_1_LBL)
  SR_DT_1_VAL <- cbind(select(SR_DT_FILT ,`ServiceRequest No`,Combined),SR_DT_1_VAL)
  
  LABEL <- colnames(SR_DT_1_LBL[,c(-1,-2)])
  VALUE <- colnames(SR_DT_1_VAL[,c(-1,-2)])
  DYNFILED_LBL <- melt(SR_DT_1_LBL,id.vars = c("ServiceRequest No","Combined"),measure.vars = LABEL , variable.name="Label")
  DYNFILED_VAL <- melt(SR_DT_1_VAL,id.vars = c("ServiceRequest No","Combined"),measure.vars = VALUE , variable.name="Value")
  SR_DT_2 <- cbind(DYNFILED_LBL,DYNFILED_VAL)
  SR_DT_2<- SR_DT_2[,c(1,2,4,8)]
  names(SR_DT_2) <- c("ServiceRequest No", "Combined", "Dyn_Field","Value")
  #SR_DT_2 %>% distinct(Dyn_Field)
  SR_DT_2 <- SR_DT_2 %>% filter(!is.na(Dyn_Field) )
  SR_DT_3 <- SR_DT_2 %>% dcast(`ServiceRequest No`+Combined~Dyn_Field,max) 
  return(SR_DT_3)
}

