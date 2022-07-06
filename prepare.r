rm(list = ls())
source("init.r", encoding = "UTF-8")

years = c(2015:2022)

meta_data = fread("https://raw.githubusercontent.com/CopernicusAtmosphere/air-quality-covid19-response/master/CAMS_AQ_LOCATIONS_V1.csv")
aq_data = list()

for(year in years){
  aq_data[[as.character(year)]] = fread(glue("https://raw.githubusercontent.com/CopernicusAtmosphere/air-quality-covid19-response/master/cams_air_quality_analysis_{year}.csv"))
}

aq_final = rbindlist(aq_data)

aq_final = merge(aq_final, meta_data[, .(id, name)], by.x = "city_id", by.y = "id")
aq_final[, no2_rmean7 := frollmean(NO2, 7), by = "name"]
aq_final[, no2_rmean35 := frollmean(NO2, 35), by = "name"]
aq_final[, no2_rmean1j := frollmean(NO2, 365), by = "name"]
aq_final[, no2_rmean2j := frollmean(NO2, 365*2), by = "name"]
aq_final[, no2_rmean3j := frollmean(NO2, 365*3), by = "name"]

aq_final[, no2_rmean_c := 0.6*no2_rmean1j + 0.3*no2_rmean2j + 0.1*no2_rmean3j, by = "name"]
aq_final = aq_final[!is.na(no2_rmean_c)]

aq_final[, no2_rmean365_adv := no2_rmean1j[1] - min(no2_rmean1j), by = "name"]
aq_final = aq_final[order(-no2_rmean365_adv, basetime)]
aq_final[, name_adv := paste(name, "-", round(no2_rmean365_adv, digits = 2))]
aq_final$name_adv = factor(aq_final$name_adv, levels = unique(aq_final$name_adv))

aq_final = melt(aq_final, id.vars = c("city_id", "basetime", "name", "name_adv"))

aq_final[variable == "no2_rmean1j", variable :="365d moving average"]
aq_final[variable == "no2_rmean_c", variable :="Composite moving average"]

saveData(aq_final, "prepared.rData")

print(max(aq_final$basetime))
