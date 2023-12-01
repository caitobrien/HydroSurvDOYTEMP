# Passage-type designations salmon
# For the DART Transportation Filter, see method description here: https://www.cbr.washington.edu/dart/metadata/pit#transport
trans_type.sal <- unique(DATA$pass_type)[which(nchar(unique(DATA$pass_type)) == 5)] ##pulling out sting == 5 characters
lwg_trans_type <- c("LWG-T", "LWG-S") ##setting type option for transport
ror_type.sal <- c("ROR", unique(DATA$pass_type)[which(nchar(unique(DATA$pass_type)) == 7)]) ##pulling out sting == 7 characters AND "ROR" to assign ROR

# LGR site salmon
# More about the PIT Tag release sites, see https://www.cbr.washington.edu/dart/query/pit_relsites
LGR.site.sal <- grep("LGR", unique(DATA$rel_site), value = TRUE) ##grep searches for matches to LGR, pulling out unique rel sites and returns value not vector integer bc value=TRUE
# "LGRRBR" "LGRRRR" "LGR"    "LGRRTR" "LGRGWL" "LGRTRB" "LGRTAL" "LGROFL"  "LGRCOL" "LGRBPS" "LGRGAT" "LGRLDR"

# ROR W SS Chinook that have a release time and a length at LGR
##filtering to include wild ROR with t_run = 1,2,5 (data doesn't show any other options though) and has a release time
ChSSWR.DATA <- DATA[(DATA$pass_type %in% ror_type.sal) & DATA$t_run %in% c(1, 2, 5) & DATA$t_rear_type == "W" & (!is.na(DATA$rel_time)), ]
##of the filtered data, further subset by which have a release site with string "LGR" present
ChSSWR.DATA <- ChSSWR.DATA[which((ChSSWR.DATA$rel_site %in% LGR.site.sal) == TRUE), ]

# W SS Chinook transported from LGR
##filter to include only lwg_trans_types c("LWG-T", "LWG-S") and all run types (1,2,5) and keep only wild with a release time
ChSSWT.DATA <- DATA[(DATA$pass_type %in% lwg_trans_type) == TRUE & DATA$t_run %in% c(1, 2, 5) & DATA$t_rear_type == "W" & (!is.na(DATA$rel_time)) == TRUE, ]
##of the filtered data, further subset by which have a release site with string "LGR" present
ChSSWT.DATA <- ChSSWT.DATA[which((ChSSWT.DATA$rel_site %in% LGR.site.sal) == TRUE), ]

# W SS Chinook ROR AND transported from LGR
ChSSWRT.DATA <- rbind(ChSSWR.DATA, ChSSWT.DATA) ##combine subsetted data of ROR and T

##create holder dataframe with LGRjuv = 1 for each row of ChSSWRT.DATA and 0 for LGRadt (adult)
ChSSWRT.DH.DATA <- data.frame(
  LGRjuv = rep(1, nrow(ChSSWRT.DATA)),
  LGRadt = 0,
  Year = NA,
  DOY = NA,
  Transport = 0
)

##change LGRadt to 1 if chSSWRT.DATA has a adult return timestamp
ChSSWRT.DH.DATA$LGRadt[!is.na(ChSSWRT.DATA$adu_lgr_first)] <- 1
##populate DH dataframe with year extracted from release time of ChSSWRT.DATA
ChSSWRT.DH.DATA$Year <- as.numeric(format(strptime(ChSSWRT.DATA$rel_time, "%Y-%m-%d %H:%M:%S"), "%Y"))
##populate DH dataframe with DOY extracted from release time of ChSSWRT.DATA
ChSSWRT.DH.DATA$DOY <- as.numeric(format(strptime(ChSSWRT.DATA$rel_time, "%Y-%m-%d %H:%M:%S"), "%j"))
##change transport from 0 to 1 if pass_type of ChSSWRT.DATA matches lwg_trans_types c("LWG-T", "LWG-S")
ChSSWRT.DH.DATA$Transport[ChSSWRT.DATA$pass_type %in% lwg_trans_type] <- 1

#--->skip to next chunk for brms data wrangling

# Dead Salmon
## created as a vector of values--using adjusted DH dataframe, if adu return = 0, then dead=1, if not then alive=0
LGR.dead.ind.sal <- ifelse(ChSSWRT.DH.DATA$LGRadt == 0, 1, 0)

# Alive.Dead matrix response salmon
##next create a dataframe pulling LGRadt from DH dataframe which is now representing alive and combine with newly created dead values. Convert into matrix format and append to DH dataframe
ChSSWRT.DH.DATA$AliveDead <- as.matrix(data.frame(alive = ChSSWRT.DH.DATA$LGRadt, dead = LGR.dead.ind.sal))

# Smolt migration season salmon
#filter to include DOY between 80 to 160 (spsu outmigration range)
ChSSWRT.DH.DATA <- ChSSWRT.DH.DATA[ChSSWRT.DH.DATA$DOY %in% (80:160), ]

# Smolt migration years 1993-2018
ChSSWRT.DH.DATA <- ChSSWRT.DH.DATA[ChSSWRT.DH.DATA$Year %in% 1993:2018, ]

# Normalize salmon data to mean of 0 and 1 SD
##only need to do for DOY and add column for DOYz
ChSSWRT.DH.DATA$DOYz <- scale(ChSSWRT.DH.DATA$DOY)

glmm7<-load(here("results", "glmm7.sal"))
summary(glmm7.sal)

# salmon
W.scale.DOY.sal <- scale(ChSSWRT.DH.DATA$DOY)
W.scale.x.sal <- attr(W.scale.DOY.sal, "scaled:center")
W.scale.sd.sal <- attr(W.scale.DOY.sal, "scaled:scale")

## Check on this code--used to rescale back to DOY?
newdoyz.w.sal <- ((90:160) - W.scale.x.sal) / W.scale.sd.sal #cob changed to 90

##create new data with newDOYz TRAN YEAR
W.newdata.sal <- expand_grid(
  DOYz = newdoyz.w.sal,
  Transport = c(0, 1),
  Year = 1993:2018
)

#Add to new data for each remaining parameter
W.newdata.sal$"I(DOYz^2)" <- (W.newdata.sal$DOYz)^2
W.newdata.sal$"DOYz:Transport" <- W.newdata.sal$DOYz * W.newdata.sal$Transport
W.newdata.sal$"I(DOYz^2):Transport " <- W.newdata.sal$`I(DOYz^2)` * W.newdata.sal$Transport

##once new data is set, predict response-
##why only including certain parameters in re.form?
##dropped ~1 + DOYz + I(DOYz^2) + Transport + DOYz:Transport + I(DOYz^2):Transport from equation
W.pred.sal <- predict(glmm7.sal, W.newdata.sal, re.form = ~ (1 + DOYz + I(DOYz^2) + Transport | Year), allow.new.levels = TRUE)
predict

##pulling certain columns from new data, and all predicted data plus appending a logistic distribution of probabilities from predictions (logitSAR and SAR)
WCh.pred.df <- data.frame(W.newdata.sal[, c(3, 2, 1, 4, 5, 6)], W.pred.sal, plogis(W.pred.sal))
names(WCh.pred.df) <- c("Year", "Transport", "DOYz", "I(DOYz^2)", "DOYz:Transport", "I(DOYz^2):Transport", "logitSAR", "SAR")

##extract logitSAR and SAR

# matrix of new data for salmon
WCh.Xmat <- as.matrix(data.frame(1, WCh.pred.df[, c(3, 4, 2, 5, 6)]))
# variance-covariance matrix for model parameters
WCh.bvcov <- as.matrix(vcov(glmm7.sal))
# SE for predicted, based on matrix of new data
WCh.pred.se <- sqrt(diag(WCh.Xmat %*% WCh.bvcov %*% t(WCh.Xmat)))
WCh.pred.df$logitSAR.se <- WCh.pred.se
# Confidence interval
WCh.pred.df$SAR.lo <- plogis(WCh.pred.df$logitSAR - 1.96 * WCh.pred.df$logitSAR.se)
WCh.pred.df$SAR.hi <- plogis(WCh.pred.df$logitSAR + 1.96 * WCh.pred.df$logitSAR.se)
# factor for ggplot2
WCh.pred.df$Transport <- as.factor(WCh.pred.df$Transport)

##manually calc T:I from predicted SAR

# T:I
WCh.pred.df$TI <- NA
##why is it necessary to call code 2x? I don't think it is
WCh.pred.df$TI[WCh.pred.df$Transport == 1] <- WCh.pred.df$TI[WCh.pred.df$Transport == 1] <- WCh.pred.df$SAR[WCh.pred.df$Transport == 1] / WCh.pred.df$SAR[WCh.pred.df$Transport == 0]
# T:I confidence interval
WCh.pred.df$TI.lo[WCh.pred.df$Transport == 1] <- WCh.pred.df$TI.lo[WCh.pred.df$Transport == 0] <- WCh.pred.df$SAR.lo[WCh.pred.df$Transport == 1] / WCh.pred.df$SAR.hi[WCh.pred.df$Transport == 0]
WCh.pred.df$TI.hi[WCh.pred.df$Transport == 1] <- WCh.pred.df$TI.hi[WCh.pred.df$Transport == 0] <- WCh.pred.df$SAR.hi[WCh.pred.df$Transport == 1] / WCh.pred.df$SAR.lo[WCh.pred.df$Transport == 0]

## adding weekly sar for transport and ror by aggregating data to year/doy
# Add data points for weekly SARs from PIT tag data. Salmon
PIT.SAR.RT.sal <- ChSSWRT.DH.DATA[with(ChSSWRT.DH.DATA, order(Year, DOY)), ]
PIT.SAR.RT.sal$doybin <- cut(PIT.SAR.RT.sal$DOY, breaks = seq(80, 160, 10), include.lowest = TRUE)

# In-river. Salmon
PIT.SAR.ROR.sal <- PIT.SAR.RT.sal[PIT.SAR.RT.sal$Transport == 0, ]
# number of juvniles. Salmon
PIT.SAR.ROR.aggr.juv.sal <- aggregate(PIT.SAR.ROR.sal$LGRjuv, by = list(PIT.SAR.ROR.sal$Year, PIT.SAR.ROR.sal$doybin), FUN = sum)
# number of adults. Salmon
PIT.SAR.ROR.aggr.adt.sal <- aggregate(PIT.SAR.ROR.sal$LGRadt, by = list(PIT.SAR.ROR.sal$Year, PIT.SAR.ROR.sal$doybin), FUN = sum)
# number of juveniles and adults. Salmon
PIT.SAR.ROR.aggr.sal <- merge(PIT.SAR.ROR.aggr.juv.sal, PIT.SAR.ROR.aggr.adt.sal, by = c("Group.1", "Group.2"), all.x = TRUE)
names(PIT.SAR.ROR.aggr.sal) <- c("Year", "doybin", "N.juv.sal", "N.adt.sal")


##extracting median DOY for each bin
# first day of the bin. Salmon
PIT.SAR.ROR.aggr.sal$doy.median <- ifelse(nchar(as.character(PIT.SAR.ROR.aggr.sal$doybin)) < 9,
                                          as.numeric(substr(PIT.SAR.ROR.aggr.sal$doybin, 2, 3)),
                                          as.numeric(substr(PIT.SAR.ROR.aggr.sal$doybin, 2, 4))
) + 5

##taking the median doy per bin - centered mean /sd to scale data to the orig dataset attributes scaled data
PIT.SAR.ROR.aggr.sal$doyz <- (PIT.SAR.ROR.aggr.sal$doy.median - W.scale.x.sal) / W.scale.sd.sal


## dividing the adults/juveniles to get PIT SAR
# observed SAR.Salmon
PIT.SAR.ROR.aggr.sal$pitsar <- PIT.SAR.ROR.aggr.sal$N.adt.sal / PIT.SAR.ROR.aggr.sal$N.juv.sal

##repeating above steps for transported
# Transported. Salmon
PIT.SAR.Trans.sal <- PIT.SAR.RT.sal[PIT.SAR.RT.sal$Transport == 1, ]
# number of juvniles. Salmon
PIT.SAR.Trans.aggr.juv.sal <- aggregate(PIT.SAR.Trans.sal$LGRjuv, by = list(PIT.SAR.Trans.sal$Year, PIT.SAR.Trans.sal$doybin), FUN = sum)
# number of adults. Salmon
PIT.SAR.Trans.aggr.adt.sal <- aggregate(PIT.SAR.Trans.sal$LGRadt, by = list(PIT.SAR.Trans.sal$Year, PIT.SAR.Trans.sal$doybin), FUN = sum)
# number of juveniles and adults. Salmon
PIT.SAR.Trans.aggr.sal <- merge(PIT.SAR.Trans.aggr.juv.sal, PIT.SAR.Trans.aggr.adt.sal, by = c("Group.1", "Group.2"), all.x = TRUE)
names(PIT.SAR.Trans.aggr.sal) <- c("Year", "doybin", "N.juv.sal", "N.adt.sal")
# first day of the bin. Salmon
PIT.SAR.Trans.aggr.sal$doy.median <- ifelse(nchar(as.character(PIT.SAR.Trans.aggr.sal$doybin)) < 9,
                                            as.numeric(substr(PIT.SAR.Trans.aggr.sal$doybin, 2, 3)),
                                            as.numeric(substr(PIT.SAR.Trans.aggr.sal$doybin, 2, 4))
) + 5
PIT.SAR.Trans.aggr.sal$doyz <- (PIT.SAR.Trans.aggr.sal$doy.median - W.scale.x.sal) / W.scale.sd.sal

# observed SAR. Salmon
PIT.SAR.Trans.aggr.sal$pitsar <- PIT.SAR.Trans.aggr.sal$N.adt.sal / PIT.SAR.Trans.aggr.sal$N.juv.sal


##not really sure why this is happening--? need to run for shiny though
# Change x-axis values to the ones that match actual dates
W.dum.at.sal <- (c(91, 100, 110, 121, 130, 140, 152, 161) - W.scale.x.sal) / W.scale.sd.sal
