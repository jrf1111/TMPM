
dx = data.table::fread("test_data.csv")


#calculate TMPM
library(tmpm)







#lets see if I can't speed up tmpm::tmpm()

ICs = tmpm::marcTable[tmpm::marcTable$lexi == "icdIX", ]
ICs$lexi = NULL

tmpm2 = function(Pdat){
	xBeta <- NULL
	
	MortModel <- function(marc1, marc2, marc3, marc4, marc5, S_Region, Interaction) {
		xBeta <- (1.406958 * marc1) + (1.409992 * marc2) + 
			(0.5205343 * marc3) + (0.4150946 * marc4) + 
			(0.8883929 * marc5) + (-0.0890527 * S_Region) - 
			(0.7782696 * Interaction) - 2.217565
		return(xBeta)
	}
	
	app <- function(x){
		marclist <- ICs[match(x[-1], ICs$index), ]
		marclist <- marclist[order(marclist$marc, decreasing = T), ]
		TCs <- marclist[1:5, ]
		TCs$marc[is.na(TCs$marc)] = 0
		
		RegionCheck <- function(TCs) {
			if (TCs$marc[1] != 0 & TCs$marc[2] != 0 & TCs$bodyregion[1] == TCs$bodyregion[2]) {
				sr <- 1
			}
			else {
				sr <- 0
			}
			return(sr)
		}
		
		same_region <- RegionCheck(TCs)
		Imarc <- TCs$marc[1] * TCs$marc[2]
		Model_Calc <- MortModel(TCs$marc[1], TCs$marc[2], 
														TCs$marc[3], TCs$marc[4], TCs$marc[5], same_region, Imarc)
		probDeath <- pnorm(Model_Calc)
		return(probDeath)
	}
	
	
	pDeath <- apply(Pdat, 1, app)
	
	return(data.frame(pDeath = pDeath))
	
	
	
}


tmpm3 = function(Pdat){
	
	
	# MortModel <- function(marc1, marc2, marc3, marc4, marc5, S_Region, Interaction) {
	# 	xBeta <- (1.406958 * marc1) + (1.409992 * marc2) + 
	# 		(0.5205343 * marc3) + (0.4150946 * marc4) + 
	# 		(0.8883929 * marc5) + (-0.0890527 * S_Region) - 
	# 		(0.7782696 * Interaction) - 2.217565
	# 	return(xBeta)
	# }
	
	app <- function(x){
		marclist <- ICs[match(x[-1], ICs$index), ]
		marclist <- marclist[order(marclist$marc, decreasing = T), ]
		TCs <- marclist[1:5, ]
		TCs$marc[is.na(TCs$marc)] = 0
		
		# RegionCheck <- function(TCs) {
		# 	if (TCs$marc[1] != 0 & TCs$marc[2] != 0 & TCs$bodyregion[1] == TCs$bodyregion[2]) {
		# 		sr <- 1
		# 	}
		# 	else {
		# 		sr <- 0
		# 	}
		# 	return(sr)
		# }
		
		
		if (TCs$marc[1] != 0 & TCs$marc[2] != 0 & TCs$bodyregion[1] == TCs$bodyregion[2]) {
			same_region = 1
		} else same_region = 0
		
		
		Imarc <- TCs$marc[1] * TCs$marc[2]
		
		# Model_Calc <- MortModel(TCs$marc[1], TCs$marc[2], 
		# 												TCs$marc[3], TCs$marc[4], TCs$marc[5], same_region, Imarc)
		
		Model_Calc = {1.406958 * TCs$marc[1]} + {1.409992 * TCs$marc[2]} + 
			{0.5205343 * TCs$marc[3]} + {0.4150946 * TCs$marc[4]} + 
			{0.8883929 * TCs$marc[5]} + {-0.0890527 * same_region} - 
			{0.7782696 * Imarc} - 2.217565
		
		
		
		probDeath <- pnorm(Model_Calc)
		probDeath
	}
	
	
	pDeath <- apply(Pdat, 1, app)
	
	return(data.frame(pDeath = pDeath))
	
}


tmpm4 = function(Pdat){
	
	
	get_marcs = function(x){
		marclist <- ICs[match(x[-1], ICs$index), ]
		marclist <- marclist[order(marclist$marc, decreasing = T), ]
		marclist <- marclist[1:5, ]
		marclist$marc[is.na(marclist$marc)] = 0
		
		marclist$index = NULL
		
		if (marclist$marc[1] != 0 & marclist$marc[2] != 0 & marclist$bodyregion[1] == marclist$bodyregion[2]) {
			same_region = 1
		} else same_region = 0
		
		marclist = c(marclist$marc, same_region)
		names(marclist) = c(paste0("marc", 1:5), "same_region")
		
		marclist
	}
	
	
	# marcs = matrix(NA, nrow = nrow(Pdat), ncol = 6)
	# colnames(marcs) = c("marc1", "marc2", "marc3", "marc4", "marc5", "same_region")
	# marcs = as_tibble(marcs)
	
	# for(i in 1:nrow(Pdat)){
	# 	marcs[i, ] = get_marcs(Pdat[i, ])
	# }
	
	
	marcs = apply(Pdat, 1, get_marcs)
	marcs = as_tibble(t(marcs)) #transposing takes a little bit...
	
	
	
	marcs$Imarc = marcs$marc1 * marcs$marc2
	
	
	Model_Calc = 
		{1.406958 * marcs$marc1} + 
		{1.409992 * marcs$marc2} + 
		{0.5205343 * marcs$marc3} +
		{0.4150946 * marcs$marc4} + 
		{0.8883929 * marcs$marc5} + 
		{-0.0890527 * marcs$same_region} - 
		{0.7782696 * marcs$Imarc} - 2.217565
	
	
	pnorm(Model_Calc)
	
	
	
	
}




#parallel version of tmpm4, not really any faster
tmpm4p = function(Pdat, cores = 6, max_combine = max(c(ceiling(nrow(Pdat)*0.1), 100))){
	
	
	get_marcs = function(x){
		marclist <- ICs[match(x[-1], ICs$index), ]
		marclist <- marclist[order(marclist$marc, decreasing = T), ]
		marclist <- marclist[1:5, ]
		marclist$marc[is.na(marclist$marc)] = 0
		
		marclist$index = NULL
		
		if (marclist$marc[1] != 0 & marclist$marc[2] != 0 & marclist$bodyregion[1] == marclist$bodyregion[2]) {
			same_region = 1
		} else same_region = 0
		
		marclist = c(marclist$marc, same_region)
		names(marclist) = c(paste0("marc", 1:5), "same_region")
		
		marclist
	}
	
	
	library(foreach)
	library(doParallel)
	
	doParallel::registerDoParallel(cores = cores)
	marcs = foreach::foreach(i = 1:nrow(Pdat), .combine=dplyr::bind_rows, 
													 .multicombine = T, .maxcombine = max_combine) %dopar% {
													 	get_marcs(Pdat[i, ])
													 }
	doParallel::stopImplicitCluster()
	
	marcs = as.data.frame(marcs)
	marcs$Imarc = marcs$marc1 * marcs$marc2
	
	
	Model_Calc = 
		{1.406958 * marcs$marc1} + 
		{1.409992 * marcs$marc2} + 
		{0.5205343 * marcs$marc3} +
		{0.4150946 * marcs$marc4} + 
		{0.8883929 * marcs$marc5} + 
		{-0.0890527 * marcs$same_region} - 
		{0.7782696 * marcs$Imarc} - 2.217565
	
	
	pnorm(Model_Calc)
	
	
}


tmpm5 = function(Pdat){
	
	
	get_marcs = function(x){
		marclist <- ICs[match(x[-1], ICs$index), ]
		marclist <- marclist[order(marclist$marc, decreasing = T), ]
		marclist <- marclist[1:5, ]
		marclist$marc[is.na(marclist$marc)] = 0
		
		marclist$index = NULL
		
		if (marclist$marc[1] != 0 & marclist$marc[2] != 0 & marclist$bodyregion[1] == marclist$bodyregion[2]) {
			same_region = 1
		} else same_region = 0
		
		Imarc = marclist$marc[1]*marclist$marc[2]
		
		marclist = c(marclist$marc, same_region, Imarc)
		names(marclist) = c(paste0("marc", 1:5), "same_region", "Imarc")
		
		marclist
	}
	
	
	marcs = apply(Pdat, 1, get_marcs) 
	
	betas = c(1.406958,      #marc1
						1.409992,      #marc2
						0.5205343,     #marc3
						0.4150946,     #marc4
						0.8883929,     #marc5
						-0.0890527,    #same_region
						-0.7782696     #Imarc
	)
	
	
	Model_Calc = {betas %*% marcs} - 2.217565
	
	
	Model_Calc = pnorm(Model_Calc)
	
	c(Model_Calc)
	
	
}

#parallel version of tmpm5, not really any faster
tmpm5p = function(Pdat, cores = 6, max_combine = max(c(ceiling(nrow(Pdat)*0.1), 100))){
	
	get_marcs = function(x){
		marclist <- ICs[match(x[-1], ICs$index), ]
		marclist <- marclist[order(marclist$marc, decreasing = T), ]
		marclist <- marclist[1:5, ]
		marclist$marc[is.na(marclist$marc)] = 0
		
		marclist$index = NULL
		
		if (marclist$marc[1] != 0 & marclist$marc[2] != 0 & marclist$bodyregion[1] == marclist$bodyregion[2]) {
			same_region = 1
		} else same_region = 0
		
		Imarc = marclist$marc[1]*marclist$marc[2]
		
		marclist = c(marclist$marc, same_region, Imarc)
		names(marclist) = c(paste0("marc", 1:5), "same_region", "Imarc")
		
		marclist
	}
	
	
	library(foreach)
	library(doParallel)
	doParallel::registerDoParallel(cores = cores)
	marcs = foreach::foreach(i = 1:nrow(Pdat), .combine=dplyr::bind_rows, 
													 .multicombine = T, .maxcombine = max_combine) %dopar% {
													 	get_marcs(Pdat[i, ])
													 }
	doParallel::stopImplicitCluster()
	
	marcs = as.matrix(marcs)
	
	betas = c(1.406958,      #marc1
						1.409992,      #marc2
						0.5205343,     #marc3
						0.4150946,     #marc4
						0.8883929,     #marc5
						-0.0890527,    #same_region
						-0.7782696     #Imarc
	)
	
	betas = matrix(betas, ncol = 1)
	
	Model_Calc = {marcs %*% betas} - 2.217565
	
	Model_Calc = pnorm(Model_Calc)
	
	c(Model_Calc)
}




mb = microbenchmark::microbenchmark(
	{tmpm(dx[1:100, ], ILex = 9)},
	{tmpm2(dx[1:100, ])},
	{tmpm3(dx[1:100, ])},
	{tmpm4(dx[1:100, ])},
	{tmpm4p(dx[1:100, ])},
	{tmpm4p(dx[1:100, ], max_combine = 100)},
	{tmpm4p(dx[1:100, ], max_combine = 50)},
	{tmpm5(dx[1:100, ])},
	{tmpm5p(dx[1:100, ])},
	{tmpm5p(dx[1:100, ], max_combine = 100)},
	{tmpm5p(dx[1:100, ], max_combine = 50)},
	times = 200, 
	control = list(warmup = 5))

gc()
mb2 = microbenchmark::microbenchmark(
	{tmpm(dx[1:1000, ], ILex = 9)},
	{tmpm2(dx[1:1000, ])},
	{tmpm3(dx[1:1000, ])},
	{tmpm4(dx[1:1000, ])},
	{tmpm4p(dx[1:1000, ])},
	{tmpm4p(dx[1:1000, ], max_combine = 1000)},
	{tmpm4p(dx[1:1000, ], max_combine = 500)},
	{tmpm5(dx[1:1000, ])},
	{tmpm5p(dx[1:1000, ])},
	{tmpm5p(dx[1:1000, ], max_combine = 1000)},
	{tmpm5p(dx[1:1000, ], max_combine = 500)},
	times = 200, 
	control = list(warmup = 5))
gc()


mb3 = microbenchmark::microbenchmark(
	{tmpm(dx[1:5000, ], ILex = 9)},
	{tmpm2(dx[1:5000, ])},
	{tmpm3(dx[1:5000, ])},
	{tmpm4(dx[1:5000, ])},
	{tmpm4p(dx[1:5000, ])},
	{tmpm4p(dx[1:5000, ], max_combine = 5000)},
	{tmpm4p(dx[1:5000, ], max_combine = 2500)},
	{tmpm5(dx[1:5000, ])},
	{tmpm5p(dx[1:5000, ])},
	{tmpm5p(dx[1:5000, ], max_combine = 5000)},
	{tmpm5p(dx[1:5000, ], max_combine = 2500)},
	times = 200, 
	control = list(warmup = 5))
gc()


mb
ggplot2::autoplot(mb)

mb2
ggplot2::autoplot(mb2)

mb3
ggplot2::autoplot(mb3)

