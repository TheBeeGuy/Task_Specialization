#Hypergeometric test for Food genes compared to whitfield
overlap_food_whit<-58##58 in common
whitfield<-864##864 in whitfield
both<-6578#6578 total in both data
food<-339##339in all food
1-phyper(overlap_food_whit, whitfield, both-whitfield, food, lower.tail = TRUE, log.p = FALSE)
##significant overlap between food related genes and nurse forager genes in whitefield

#Hypergeometric test for BP compared to BP5
overlapbp5<-1
BP5<-104
myBP<-58
1-phyper(overlapbp5, BP5, both-BP5, myBP, lower.tail = TRUE, log.p = FALSE)
#0.25

overlapbp_15<-3
BP15<-87
1-phyper(overlapbp_15, BP15, both-BP15, myBP, lower.tail = TRUE, log.p = FALSE)
#significant overlap with bp_15
#p=0.008

overlap_allphero_bp15<-7
overlap_allphero_bp5<-5
allphero<-288

##all phero with bp 5
1-phyper(overlap_allphero_bp5, BP5, both-BP5, allphero, lower.tail = TRUE, log.p = FALSE)
#notsignificant
#0.3

##all phero with bp 15
1-phyper(overlap_allphero_bp15, BP15, both-BP15, allphero, lower.tail = TRUE, log.p = FALSE)
#significant
#0.0359

overlap_allphero_whit<-55
##all phero with bp 15
1-phyper(overlap_allphero_whit, whitfield, both-whitfield, allphero, lower.tail = TRUE, log.p = FALSE)
#p=0.001
##also overlap between pheromone and nurse forager


#overlap all bp
overlapbp5_bp15<-4
BP5_15<-BP5+BP15
myBP<-61
1-phyper(overlapbp5_bp15, BP5_15, both-BP5_15, myBP, lower.tail = TRUE, log.p = FALSE)
#0.25




