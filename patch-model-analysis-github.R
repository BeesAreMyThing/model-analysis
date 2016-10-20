library(doBy)
library(car)
library(lme4)

sumTab <- function(formula, data){
	summaryBy(formula, data=data,
		FUN=function(x) { c(m = mean(x, na.rm=T), 
		se = sd(x, na.rm=T)/sqrt(length(x[!is.na(x)])), n=length(x)) })}


errbarK <- function(x, y, x_se=0, y_se=0, cap=1, lwd=1, line.col = 1,
	pch=21, bg=1, col=1, cex=1, my_data=0, ...){
	##use data
	if (my_data != 0){
		x = my_data$x
		y = my_data$y
		if (y_se != 0) { y_se = my_data$y_se }
		if (x_se != 0) { x_se = my_data$x_se }
	}

	#find plot limits
	x_size = par("usr")[1] - par("usr")[2]
	y_size = par("usr")[3] - par("usr")[4]
	x_cap = x_size * (cap/100)
	y_cap = y_size * (cap/100)
	#plot point
	points(x=x, y=y, pch=pch, bg=bg, col=col, cex=cex, ...)
	if(y_se!=0){
		#y cross bar
		segments(y0=y-y_se, y1=y+y_se,
			x0=x, x1=x, 
			lwd=lwd, col=line.col)
		#y caps
		segments(y0=y+y_se, y1=y+y_se,
			x0=x-x_cap, x1=x+x_cap,
			lwd=lwd, col=line.col)
		segments(y0=y-y_se, y1=y-y_se,
			x0=x-x_cap, x1=x+x_cap, 
			lwd=lwd, col=line.col)}
	if(x_se!=0){
	#x cross bar
		segments(y0=y, y1=y,
			x0=x-x_se, x1=x+x_se, 
			lwd=lwd, col=line.col)
	#x caps
		segments(y0=y+y_cap, y1=y-y_cap,
			x0=x-x_se, x1=x-x_se,
			lwd=lwd, col=line.col)
		segments(y0=y+y_cap, y1=y-y_cap,
			x0=x+x_se, x1=x+x_se, 
			lwd=lwd, col=line.col)}
}

draw_it <- function(dat, y_name, x_name, want_errbars=FALSE, 
		ylab=y_name, xlab=x_name) {

	y_se_name = paste(substr(y_name, 1, nchar(y_name) -2), ".se", sep="")
	y_max = max(dat[y_name] + dat[y_se_name])
	y_min = min(dat[y_name] - dat[y_se_name])
	x_max = max(dat[x_name])
	x_min = min(dat[x_name])
	
	par(mar=c(5,6,4,2), oma=c(0,0,0,0))
	plot(y=dat[, y_name], x=dat[, x_name],
		pch="",
		ylab="", xlab="",
		ylim=c((y_min - ((y_max + y_min)*0.05)), 
				(y_max + ((y_max + y_min)*0.05))), 
		xlim=c((x_min - ((x_max + x_min)*0.05)), 
				(x_max + ((x_max + x_min)*0.05))),
		las=1, xaxs="i", yaxs="i", bty="n")
	title(xlab=xlab, cex.lab=1.5, line=3)
	title(ylab=ylab, cex.lab=1.5, line=4.5)
	box(bty="l", lwd=3)
	abline(0,0)

	enviros = list(dat[which(dat$enviroment_type == "random"),],
				dat[which(dat$enviroment_type == "clumped"),])
	line_types = c(1, 4)
	point_types = c(21, 24, 22)
	color_types = c("blue", "violet", "red")
	logical_list = list(c(T, F, F), c(F, T, F), c(F, F, T))
	metab_length = length(levels(enviros[[1]]$metab))
	if (metab_length == 2) {
		point_types = c(21, 22)
		color_types = c("blue", "red")
		logical_list = list(c(T, F), c( F, T)) }
	for (e in 1:2) {
		for (i in 1:metab_length) {
			lines(y=enviros[[e]][, y_name][logical_list[[i]]], 
				x=enviros[[e]][, x_name][logical_list[[i]]], 
				lwd=3, col=color_types[i], lty=line_types[e])
			if (want_errbars) {
				errbarK(y=enviros[[e]][, y_name][logical_list[[i]]],
					y_se=enviros[[e]][, y_se_name][logical_list[[i]]],
					x=enviros[[e]][, x_name][logical_list[[i]]], 
					pch=point_types[i], bg=color_types[i], 
					lty=line_types[e])}
			else {
				points(y=enviros[[e]][, y_name][logical_list[[i]]], 
					x=enviros[[e]][, x_name][logical_list[[i]]], 
					pch=point_types[i], bg=color_types[i], 
					lty=line_types[e]) 
			}
		}
	}
}



round_to <- function(x, base){ 
	base * round(x / base) 
} 


hist_it <- function(dat, x_name, main="", xlab="", rounder = 25){
	the_breaks = seq(round_to((min(dat[x_name]) - rounder), rounder), 
		round_to((max(dat[x_name]) + rounder), rounder), by=rounder)
	the_counts = c()
	for (env in c("random", "clumped")) {
		for (met in c("high", "low")){
			this_dat = dat[which(dat$enviroment_type== env & dat$metab == met),] 
			the_counts = c(the_counts, 
				hist(unlist(this_dat[x_name]), breaks=the_breaks)$counts)
		}
	}
	y_range = c(min(the_counts), max(the_counts))
	par(mfrow = c(2, 2), mar=c(5,4,4,2), oma=c(0,0,2,0), 
		las=1, xaxs="i", yaxs="i")
	for (env in c("random", "clumped")) {
		for (met in c("high", "low")){
			this_dat = dat[which(dat$enviroment_type== env & dat$metab == met),] 
			hist(unlist(this_dat[x_name]), breaks=the_breaks, ylim=y_range,
				main = paste(met, env, sep=" & "),
				xlab=xlab)
			box(bty="l", lwd=3)
	
		}
	}

	mtext(side=3, main, outer=TRUE, cex=1.5) 
	par(mfrow = c(1, 1))
}

#########################################

by_trip = read.csv("example_data.txt", header=TRUE, na.strings="NA")
by_trip$metab = relevel(by_trip$metab, "low")

### 

by_trip$if_found_patch = 1
by_trip$if_found_patch[which(is.na(by_trip$first_patch_time))] = 0

trip_deposit_by_bee = summaryBy(trip_deposit ~ bee_id, data=by_trip,
	FUN=function(x) { c(m = mean(x, na.rm=T), sum = sum(x), 
		se = sd(x, na.rm=T)/sqrt(length(x[!is.na(x)])), n=length(x)) })
trip_time_by_bee = sumTab(trip_time ~ bee_id, data=by_trip)
avg_first_patch = summaryBy(first_patch_time ~  bee_id, data=by_trip,
	FUN=function(x) { c(m = mean(x, na.rm=T), na = sum(is.na(x)), 
		se = sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))) })
patch_count_by_bee = sumTab(patch_count ~ bee_id, data=by_trip)

by_bee = unique(by_trip[, c(1:8)])
by_bee = merge(by_bee, trip_deposit_by_bee)
by_bee = merge(by_bee, rel_deposit_by_bee)
by_bee = merge(by_bee, trip_time_by_bee)
by_bee = merge(by_bee, avg_first_patch)
by_bee = merge(by_bee, patch_count_by_bee)

by_bee$trip_count = by_bee$trip_deposit.n
by_bee$trip_deposit.n = NULL
by_bee$rel_deposit.n = NULL
by_bee$unsucess_count = by_bee$first_patch_time.na
by_bee$first_patch_time.na = NULL
by_bee$trip_time.n = NULL
by_bee$patch_count.n = NULL

by_bee$sucess_rate =  1 - (by_bee$unsucess_count / by_bee$trip_count)

by_bee$net_lifetime_contribution = by_bee$trip_deposit.sum - by_bee$sleep_cost
by_bee$rel_net_lifetime_contribution = by_bee$net_lifetime_contribution / by_bee$patch_value
by_bee$search_percent = by_bee$first_patch_time.m / by_bee$trip_time.m

head(by_bee)
#############################

##Contribution
life = sumTab(net_lifetime_contribution ~  enviroment_type + patch_value + metab,
	data=by_bee)
draw_it(life, "net_lifetime_contribution.m", "patch_value", TRUE,
		xlab="Patch value (J)", ylab="Net lifetime contribution (J)")
  # Red lines are high metabloism bees, Blue are low metablism bees
  # Solid lines represent a random patch distibution, 
  # Dashed lines represent a clumped patch distibution

hist_it(dat=by_bee[which(by_bee$patch_value == 25),], 
	x_name="net_lifetime_contribution",
	xlab="Net lifetime contribution (J)", main="Low patch value", rounder=1000)

hist_it(dat=by_bee[which(by_bee$patch_value == 100),], 
	x_name="net_lifetime_contribution",
	xlab="Net lifetime contribution (J)", main="High patch value", rounder=5000)

##Relative contribution
rel_tab = sumTab(rel_net_lifetime_contribution ~ enviroment_type + patch_value + metab,
	data=by_bee)
draw_it(rel_tab, "rel_net_lifetime_contribution.m", "patch_value", TRUE,
		xlab="Patch value (J)", ylab="Relative net lifetime contribution")

## Sucess rate
sucessTab = sumTab(sucess_rate ~ enviroment_type + patch_value + metab,
	data=by_bee)
draw_it(sucessTab, "sucess_rate.m", "patch_value", TRUE,
		xlab="Path value (J)", ylab="Sucess proportion")

hist_it(dat=by_bee[which(by_bee$patch_value == 25),], x_name="sucess_rate",
	xlab="Sucess proportion", main="Low patch value", rounder=0.1)

hist_it(dat=by_bee[which(by_bee$patch_value == 100),], x_name="sucess_rate",
	xlab="Sucess proportion", main="High patch value", rounder=0.1)


###Stats
stepAIC(glm(net_lifetime_contribution ~  (enviroment_type + patch_value + metab)^3,
	data=by_bee, family="gaussian"))
mod1 = glm(formula = net_lifetime_contribution ~ enviroment_type + patch_value + 
    metab + enviroment_type:patch_value + enviroment_type:metab + 
    patch_value:metab, family = "gaussian", data = by_bee) 
summary(mod1)#AIC 80804
Anova(mod1, type=3)


stepAIC(glm(trip_count ~  (enviroment_type + patch_value + metab)^3,
	data=by_bee, family="poisson"))
mod2 = glm(formula = trip_count ~ patch_value + metab + patch_value:metab, 
    family = "poisson", data = by_bee)
summary(mod2) #AIC 26469
Anova(mod2, type=3)


first_three = by_trip[which(by_trip$trip_num <= 3),]
mod3 = glmer(if_found_patch ~ (enviroment_type + metab)^2
	+ (1|bee_id), data=first_three, family="binomial")
summary(mod3) #AIC 11583.0, BIC 11620
Anova(mod3, type=3)

mod4 = glmer(if_found_patch ~ enviroment_type + metab
	+ (1|bee_id), data=first_three, family="binomial")
summary(mod4) #AIC 11583.4, BIC 11612.9
Anova(mod4, type=3)

