data.path = system.file('extdata/', package="LakeMetabolizer")
sp.data = load.all.data('sparkling', data.path)
ts.data = sp.data$data #pull out just the timeseries data


#calculate U10 and add it back onto the original
u10 = wind.scale(ts.data)
ts.data = rmv.vars(ts.data, 'wnd', ignore.offset=TRUE) #drop old wind speed column
ts.data = merge(ts.data, u10)                          #merge new u10 into big dataset


#Now calculate k600 using the Cole method
k600.cole = k.cole(ts.data)

ts.data = merge(ts.data, k600.cole)

kgas = k600.2.kGAS(ts.data)
ts.data = rmv.vars(merge(kgas, ts.data), 'k600')

o2.sat = o2.at.sat(ts.data[,c('datetime','wtr_0')])

ts.data = merge(o2.sat, ts.data)
z.mix = ts.meta.depths(get.vars(ts.data, 'wtr'), seasonal=TRUE)
names(z.mix) = c('datetime','z.mix', 'bottom')

#set z.mix to bottom of lake when undefined
z.mix[z.mix$z.mix <=0 | is.na(z.mix$z.mix), 'z.mix'] = sp.data$metadata$maxdepth
ts.data = merge(ts.data, z.mix[,c('datetime','z.mix')])

#The following extracted from the function metab.ols
# in the package LakeMetabolizer. GPP, R, and NEP now reported as
# time series rather than daily averages

# wtr.name='wtr_0.5'
# do.obs.name='doobs_0.5'
# irr.name='par'
# complete.inputs(do.obs = do.obs, do.sat = do.sat, k.gas = k.gas, 
#                 z.mix = z.mix, irr = irr, wtr = wtr, error = TRUE)

nobs <- length(ts.data$doobs_0.5)
# mo.args <- list(...)
if (any(ts.data$z.mix <= 0)) {
  stop("z.mix must be greater than zero.")
}
if (any(ts.data$wtr <= 0)) {
  stop("all wtr must be positive.")
}
# if ("datetime" %in% names(mo.args)) {
#   datetime <- mo.args$datetime
#   freq <- calc.freq(datetime)
#   if (nobs != freq) {
#     bad.date <- format.Date(datetime[1], format = "%Y-%m-%d")
#     warning("number of observations on ", bad.date, " (", 
#             nobs, ") ", "does not equal estimated sampling frequency", 
#             " (", freq, ")", sep = "")
#   }
# else {
#   warning("datetime not found, inferring sampling frequency from # of observations")
#   freq <- nobs
# }
do.diff <- diff(ts.data$doobs_0.5)
inst_flux <- (ts.data$k.gas/freq) * (ts.data$do.sat - ts.data$doobs_0.5)
flux <- inst_flux[-nobs]
noflux.do.diff <- do.diff - flux/z.mix[-nobs]
lntemp <- log(wtr)
mod <- lm(noflux.do.diff ~ irr[-nobs] + lntemp[-nobs] - 1)
rho <- mod[[1]][2]
iota <- mod[[1]][1]
mod.matrix <- model.matrix(mod)
gpp <- mean(iota * mod.matrix[, 1], na.rm = TRUE) * freq
resp <- mean(rho * mod.matrix[, 2], na.rm = TRUE) * freq
nep <- gpp + resp
results <- list(mod = mod, metab = data.frame(GPP = gpp, 
                                              R = resp, NEP = nep))
return(results)


#OLS
ols.res = metab(ts.data, method='ols',
                wtr.name='wtr_0.5', do.obs.name='doobs_0.5', irr.name='par')
write.csv(res, 'LakeAnalyzer/sp.metab.ols.csv', row.names=FALSE)