#figure generation code for a metabolism example
library(LakeMetabolizer)

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


#OLS
ols.res = metab(ts.data, method='ols',
                wtr.name='wtr_0.5', do.obs.name='doobs_0.5', irr.name='par')
write.csv(ols.res, 'LakeAnalyzer/sp.metab.ols.csv', row.names=FALSE)

#Bookkeep
# add column for day (1) and night (0)
ts.data$day_night = as.integer(is.day(datetimes=ts.data$datetime, lat=sp.data$metadata$latitude))

book.res = metab(ts.data, method='bookkeep',
                 wtr.name='wtr_0.5', do.obs.name='doobs_0.5', irr.name='day_night')

#Bring year and DOY together to get R datetime again
ols.res$datetime = ISOdate(ols.res$year, 1, 1) + book.res$doy*3600*24 - 3600*24

#summary stats
res = ols.res
cat('metab.ols GPP:', mean(res[,3]), ' R:', mean(res[,4]), 'NEP:', mean(res[,5]), '\n')

#impossible values removed
cat('Averages after removal of impossible GPP and R values\n')
res = ols.res
cat('metab.ols GPP:', mean(res[res[,3]>0,3]), ' R:', mean(res[res[,4]<0,4]), 'NEP:', mean(res[res[,3]>0,3]) + mean(res[res[,4]<0,4]), '\n')
