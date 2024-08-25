library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)


theme_set(theme_minimal())
palette10 = colorRampPalette(c("black", "red"))(10)


read_station = function(id, start="1894-01-01", end=as.character(Sys.Date() - 1)) {
    query = c(
        "https://dataset.api.hub.geosphere.at/v1/station/historical/klima-v2-1d",
        "?parameters=rr",  # total sum of precipitation (mm)
        "&parameters=rra_manu",  # type of precipitation
        "&parameters=tlmin",  # minimum air temperature (°C)
        "&parameters=tlmax",  # maximum air temperature (°C)
        sprintf("&start=%s", start),
        sprintf("&end=%s", end),
        sprintf("&station_ids=%s", id),
        "&output_format=csv",
        "&filename=test"
    )
    # types of precipitation (encoded in original "rra_manu" column):
    #  0: none, 1: rain, 2: snow, 3: sleet, 4: hail, 5: rain and snow, 6: rain and sleet,
    #  7: rain and hail, 8: snow and sleet, 9: sleet and hail,
    # 10: rain and snow and sleet, 11: rain and sleet and hail, 12: snow and hail,
    # 13: rain and snow and hail, 14: snow and sleet and hail,
    # 15: rain and snow and sleet and hail
    read_csv(paste0(query, collapse=""), show_col_types=FALSE, progress=FALSE) |> 
        rename(date=time, precip=rr, type=rra_manu, tmin=tlmin, tmax=tlmax) |>
        mutate(
            date=as.Date(date),
            precip=replace(precip, precip == -1, 0)
        )
}


daily = read_station(id="30") |> select(-station, -substation)

monthly = daily |> 
    group_by(year=year(date), month=month(date, label=TRUE)) |> 
    summarize(tmin=mean(tmin), tmax=mean(tmax), tmean=mean(mean(c(tmin, tmax))), .groups="drop") |> 
    mutate(
        decade=floor(year / 10) * 10,
        units=factor(year %% 10)
    )

ggplot(data=monthly, mapping=aes(x=month, y=tmax, color=units, group=units)) +
    geom_hline(yintercept=0) +
    geom_line(alpha=0.5) +
    scale_y_continuous(n.breaks=12) +
    scale_color_manual(values=palette10) +
    facet_wrap(vars(decade)) +
    labs(x=element_blank(), y="Temperature (°C)") +
    theme(legend.position="none", axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

ggplot(data=monthly, mapping=aes(x=year, y=tmean, color=month, fill=month, group=month)) +
    geom_hline(yintercept=0) +
    geom_ribbon(mapping=aes(ymin=tmin, ymax=tmax), alpha=0.15, linewidth=0) +
    geom_smooth(data=monthly |> drop_na(), method="loess", formula=y ~ x, se=FALSE) +
    scale_x_continuous(breaks=c(1894, seq(1900, 2020, 10)), expand=c(0.01, 0.01)) +
    scale_y_continuous(n.breaks=22) +
    labs(x=element_blank(), y="Temperature (°C)") +
    theme(legend.title=element_blank())

daily |> 
    group_by(year=year(date)) |>
    summarize(heat_days=sum(tmax >= 30), tropic_nights=sum(tmin >= 20)) |> 
    pivot_longer(-year) |> 
    ggplot(mapping=aes(x=year, y=value, fill=name)) +
    geom_col(position="identity") +
    geom_hline(yintercept=0) +
    scale_fill_manual(
        values=palette10[c(10, 1)],
        labels=c(
            expression(Heat~days~(italic(T)[max] >= 30*degree*C)),
            expression(Tropic~nights~(italic(T)[min] >= 20*degree*C))
        )
    ) +
    scale_x_continuous(breaks=c(1894, seq(1900, 2020, 10), 2024), expand=c(0.01, 0.01)) +
    scale_y_continuous(n.breaks=20, expand=c(0, 0.5)) +
    labs(x=element_blank(), y="Count") +
    theme(
        legend.title=element_blank(),
        legend.position="inside",
        legend.position.inside=c(0.12, 0.9),
        legend.box.background=element_rect(fill="white", color="white")
    )

