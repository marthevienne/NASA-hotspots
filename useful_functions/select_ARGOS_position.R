select_ARGOS_position <- function(deployment, ref, date) {
  positions = deployment[, 1:6][which(deployment$D_DATE == date & deployment$REF == ref),]
  positions = positions[order(-positions$LQ),]
  return(positions[1,])
}