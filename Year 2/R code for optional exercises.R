######## CODE FOR WEEK 8 EXTRA EXERCISES

# PART 1:
Treatment <- c("A","B","C","D")
	# POSSIBLE TREATMENTS
Treatment_order <- numeric()
	# EMPTY VECTOR TO FILL
for(i in 1:20) Treatment_order <- c(Treatment_order,sample(Treatment,replace=F))
	# REPEATED SAMPLING OF TREATMENTS TO FILL VECTOR
Treatment_order
	# CHECK

Side <- c("Left","Right")
Side_order <- numeric()
for(i in 1:40) Side_order <- c(Side_order,sample(Side,replace=F))
Side_order
	# REPEATED FOR THE OTHER VARIABLE

Experimental_design <- data.frame(Treatment_order,Side_order)
	# COMBINE BOTH INTO A DATA FRAME
write.csv(Experimental_design,"Experimental_design.csv",row.names=F)
	# WRITE TO CSV

# PART 2:
table(Treatment_order)
table(Side_order)
	# CHECK
table(Treatment_order,Side_order)
	# IS EACH COMBINATION OF TREATMENT AND SIDE AS FREQUENT AS ONE ANOTHER?

Treatment_side <- c("A_left","B_left","C_left","D_left","A_right","B_right","C_right","D_right")
Treatment_side_order <- numeric()
for(i in 1:10) Treatment_side_order <- c(Treatment_side_order,sample(Treatment_side,replace=F))
table(Treatment_side_order)
	# CHECK

# PART 3:
Treatment_side <- expand.grid(Treatment,Side)
	# FINDS EVERY COMBINATION OF TREATMENT AND SIDE
Treatment_side_order <- matrix(ncol=2,nrow=0)
	# EMPTY MATRIX RATHER THAN EMPTY VECTOR
for(i in 1:10) Treatment_side_order <- rbind(Treatment_side_order,Treatment_side[sample(1:8,replace=F),])
	# UNLIKE ABOVE, RANDOMISES THE ROW ORDER
	# ALSO NOTE THAT THE FUNCTION TO COMBINE THE EXISTING AND NEWLY SAMPLED ORDER IS DIFFERENT
nrow(Treatment_side_order)
	# CHECK

