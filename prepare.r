rm(list = ls())
source("init.r", encoding = "UTF-8")

years = c(2015:2022)

meta_data = fread("https://raw.githubusercontent.com/CopernicusAtmosphere/air-quality-covid19-response/master/CAMS_AQ_LOCATIONS_V1.csv")
aq_data = list()

for(year in years){
  aq_data[[as.character(year)]] = fread(glue("https://raw.githubusercontent.com/CopernicusAtmosphere/air-quality-covid19-response/master/cams_air_quality_analysis_{year}.csv"))
}

aq_data = rbindlist(aq_data)

aq_data = merge(aq_data, meta_data[, .(id, name)], by.x = "city_id", by.y = "id")

#vie_data = aq_data[name == "Berlin"]
aq_data[, no2_rmean7 := frollmean(NO2, 7), by = "name"]
aq_data[, no2_rmean35 := frollmean(NO2, 35), by = "name"]
aq_data[, no2_rmean365 := frollmean(NO2, 365), by = "name"]
aq_data[, no2_rmean2j := frollmean(NO2, 365*2), by = "name"]
aq_data[, no2_rmean3j := frollmean(NO2, 365*3), by = "name"]
aq_data[, no2_rmean_c := 0.7*no2_rmean365 + 0.2*no2_rmean2j + 0.1*no2_rmean2j, by = "name"]
aq_data = aq_data[!is.na(no2_rmean365)]
aq_data[, no2_rmean365_adv := no2_rmean365[1] - min(no2_rmean365), by = "name"]
aq_data = aq_data[order(-no2_rmean365_adv, basetime)]
aq_data[, name_adv := paste(name, "-", round(no2_rmean365_adv, digits = 2))]
aq_data$name_adv = factor(aq_data$name_adv, levels = unique(aq_data$name_adv))

aq_data = melt(aq_data, id.vars = c("city_id", "basetime", "name", "name_adv"))

aq_data[variable == "no2_rmean7", variable :="7d moving average"]
aq_data[variable == "no2_rmean365", variable :="365d moving average"]
aq_data[variable == "no2_rmean35", variable :="35d moving average"]
aq_data[variable == "no2_rmean_c", variable :="Composite moving average"]

saveData(aq_data, "prepared.rData")

print(max(aq_data$basetime))
