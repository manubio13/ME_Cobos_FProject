ku.enm.omrat <- function(model, threshold, occ.tra, occ.test) {
  suit_val_cal <- na.omit(extract(model, occ.tra))
  suit_val_eval <- na.omit(extract(model, occ.test))
  val <- ceiling(length(occ.tra[,1]) * threshold / 100) + 1
  omi_val_suit <- sort(suit_val_cal)[val]
  om_rate <- as.numeric(length(suit_val_eval[suit_val_eval < omi_val_suit])/length(suit_val_eval))
  return(om_rate)
}