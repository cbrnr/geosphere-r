library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

read_station = function(id, start="1894-01-01", end="2023-12-31") {
    query = c(
        "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v1-1d",
        "?parameters=nied",  # total amount of precipitation (mm)
        "&parameters=nieda",  # type of precipitation
        "&parameters=t",  # air temperature (°C)
        sprintf("&start=%s", start),
        sprintf("&end=%s", end),
        sprintf("&station_ids=%s", id),
        "&output_format=csv",
        "&filename=test"
    )
    # types of precipitation (encoded in original "nieda" column):
    #  0: none, 1: rain, 2: snow, 3: sleet, 4: hail, 5: rain and snow, 6: rain and sleet,
    #  7: rain and hail, 8: snow and sleet, 9: sleet and hail,
    # 10: rain and snow and sleet, 11: rain and sleet and hail, 12: snow and hail,
    # 13: rain and snow and hail, 14: snow and sleet and hail,
    # 15: rain and snow and sleet and hail
    read_csv(paste0(query, collapse=""), show_col_types=FALSE, progress=FALSE) |> 
        rename(date=time, precip=nied, type=nieda, temp=t) |> 
        mutate(
            date=as.Date(date),
            precip=replace(precip, precip == -1, 0),
            snow=ifelse(type %in% c(2, 5, 8, 10, 12, 13, 14, 15), precip, 0)
        ) |> 
        select(!c(type, precip, station))
}

df1 = read_station("16402", start="1894-01-01", end="1988-05-31")
df2 = read_station("16412", start="1988-06-01", end="2023-06-30")
# df3 = read_station("16414", start="2023-07-01", end=Sys.Date())  # station not available yet

df = full_join(df1, df2)

# merge Feb 29 with Feb 28 (facilitates plotting if every year has 365 days)
feb29 = df |> filter(day(date) == 29, month(date) == 2, snow > 0)  # Feb 29 with snow
feb28 = df[["date"]] %in% (feb29[["date"]] - 1)  # previous days
snow28 = df[feb28, "snow"]  # snow on previous days
df[feb28, "snow"] = snow28 + feb29["snow"]  # add snow on Feb 29 to Feb 28
df = df |> filter(!(day(date) == 29 & month(date) == 2))  # remove Feb 29

theme_set(theme_minimal())

ggplot(data=df, mapping=aes(x=date, y=snow)) +
    geom_col(col="blue")

ggplot(data=df |> filter(snow < 80), mapping=aes(x=format(as.Date(date), "%m-%d"), y=year(date), fill=snow)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="blue") +
    labs(x=NULL, y=NULL)
