rm(list=ls())
library(tidyverse)
library(curl)
library(ggtext)
library(extrafont)
library(RcppRoll)
library(ragg)
library(paletteer)
library(lubridate)
library(behindbarstools)

## TO DO: 
## try to change colors -- tried, didn't work 
## try north kern -- good
## remove annotation (add in figma)
## add year labels (add in figma)

# grab all our data
hist_data <- read_scrape_data(all_dates = TRUE)

theme_custom <- function() {
    theme_classic() %+replace%
        theme(plot.title.position="plot", plot.caption.position="plot",
              strip.background=element_blank(),
              strip.text=element_text(face="bold", size=rel(1)),
              plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                      margin=margin(0,0,5.5,0)),
              text=element_text(family="Lato"))
}

# only look at the data where we have active cases
sub_active_data <- hist_data %>%
    select(Jurisdiction, Facility.ID, Name, Date, State, Residents.Active) %>%
    na.omit()

# Set value for increment size (basically controls the tightness of the spiral,
# bigger numbers => a looser spiral) havent  figured out a good way to tweak
# this other than manually
spiralincrement <- .15

# select your facility of interest by ID
#target_id <- 1846
target_id <- 2433

data <- sub_active_data %>%
    filter(Facility.ID == target_id) %>%
    right_join(tibble(
        Date=seq.Date(min(.$Date), max(.$Date), "day"))) %>%
    arrange(Date) %>%
    mutate(cases_roll = approx(
        as.numeric(Date), Residents.Active, xout = as.numeric(Date))$y) %>%
    # do rolling average to smooth things out
    mutate(cases_roll2 = roll_mean(cases_roll, 7, align="center", fill=NA)) %>%
    mutate(cases_roll = ifelse(is.na(cases_roll2), cases_roll, cases_roll2)) %>%
    mutate(year = year(Date)) %>%
    group_by(year) %>%
    mutate(yeardays=as.numeric(difftime(
        Date ,as.Date(paste0(year, "-01-01")) , units = c("days")))) %>%
    ungroup() %>%
    mutate(
           #Create variable to represent the base of each bar
           # change the number to tighten/relax the spiral
           increment=spiralincrement*c(1:n()),
           #Add cases to the base to get the top of each bar
           incrementcases=increment+cases_roll)

max_cases <- tail(data$cases_roll, n=1)

#Pull out a couple of parameters to control the positioning of the segments
seg2021 <- data$increment[data$year==2020 & data$yeardays==364]-spiralincrement*0.5
seg2122 <- data$increment[data$year==2021 & data$yeardays==360]-spiralincrement*0.5
arrowmin <- max(data$increment[data$year==2022 & !is.na(data$cases_roll)])+spiralincrement*4
arrowxpos <- max(data$yeardays[data$year==2022 & !is.na(data$cases_roll)])+4

#Function borrowed from stackoverflow answer from truenbrand to draw annotations on a polar plot
#https://stackoverflow.com/questions/66196451/draw-straight-line-between-any-two-point-when-using-coord-polar-in-ggplot2-r/66196752#66196752
geom_segment_straight <- function(...) {
    layer <- geom_segment(...)
    new_layer <- ggproto(NULL, layer)
    old_geom <- new_layer$geom
    geom <- ggproto(
        NULL, old_geom,
        draw_panel = function(data, panel_params, coord,
                              arrow = NULL, arrow.fill = NULL,
                              lineend = "butt", linejoin = "round",
                              na.rm = FALSE) {
            data <- ggplot2:::remove_missing(
                data, na.rm = na.rm, c("x", "y", "xend", "yend",
                                       "linetype", "size", "shape")
            )
            if (ggplot2:::empty(data)) {
                return(zeroGrob())
            }
            coords <- coord$transform(data, panel_params)
            # xend and yend need to be transformed separately,
            # as coord don't understand
            ends <- transform(data, x = xend, y = yend)
            ends <- coord$transform(ends, panel_params)

            arrow.fill <- if (!is.null(arrow.fill)) arrow.fill else coords$colour
            return(grid::segmentsGrob(
                coords$x, coords$y, ends$x, ends$y,
                default.units = "native", gp = grid::gpar(
                    col = alpha(coords$colour, coords$alpha),
                    fill = alpha(arrow.fill, coords$alpha),
                    lwd = coords$size * .pt,
                    lty = coords$linetype,
                    lineend = lineend,
                    linejoin = linejoin
                ),
                arrow = arrow
            ))

        }
    )
    new_layer$geom <- geom
    return(new_layer)
}


plot_out <- ggplot()+
    #Need to plot each year separately, to 'trick' coord_polar to make a spiral, not a single
    #loop
    geom_rect(data=data %>% filter(year==2020 & ! is.na(cases_roll)),
              aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases,
                  fill=cases_roll), show.legend=FALSE)+
    geom_rect(data=data %>% filter(year==2021 & ! is.na(cases_roll)),
              aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases,
                  fill=cases_roll), show.legend=FALSE)+
    geom_rect(data=data %>% filter(year==2022 & ! is.na(cases_roll)),
              aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases,
                  fill=cases_roll), show.legend=FALSE)+
    geom_line(data=data %>% filter(year==2020 & ! is.na(cases_roll)),
              aes(x=yeardays, y=increment), colour="black")+
    geom_line(data=data %>% filter(year==2021),
              aes(x=yeardays, y=increment), colour="black")+
    geom_line(data=data %>% filter(year==2022 & yeardays<arrowxpos-3),
              aes(x=yeardays, y=increment), colour="black")+
    #Add a couple of tiny segments to patch the holes in the baseline at the end of each year
    geom_segment_straight(aes(x=363.5, xend=0.5, y=seg2021, yend=seg2021+2*spiralincrement),
                          colour="black")+
    geom_segment_straight(aes(x=363.5, xend=0.5, y=seg2122, yend=seg2122+2*spiralincrement),
                          colour="black")+
    scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                       labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec")) +
    scale_colour_paletteer_c("viridis::rocket", direction=-1)+
    scale_fill_paletteer_c("viridis::rocket", direction=-1)+
    # scale_color_bbcontinous() + 
    coord_polar()+
    theme_void() +
    theme(panel.grid.major.x=element_line(colour="Grey90"),
          axis.text.x=element_text(colour="Grey60"),
          text=element_text(family="Lato"), 
          plot.title=element_text(face="bold", size=rel(1.8)),
          plot.title.position = "plot", 
          plot.caption.position = "plot") #+
    #Add low key legend for a bit of context
    # geom_segment(aes(y=arrowmin, yend=arrowmin+max_cases, x=arrowxpos, xend=arrowxpos), colour="Grey30",
    #              arrow = arrow(length=unit(0.20,"cm"), ends="both", type = "closed")) +
    #Will need to manually tweak the placement of this annotation
    # annotate("text", x=arrowxpos+3, y=max_cases,
    #          label=str_c(max_cases, " active\ncases"), hjust=0, colour="Grey30",
    #          size=rel(2.5), family="Lato") #+
    # labs(title="COVID Outbreaks in Cook County Jails",
    #      subtitle="COVID numbers hit record high among those\nincarcerated in Cook County Jails",
    #      caption="UCLA Law COVID Behind Bars")

ggsave("north_kern_spiral.svg", plot_out, width = 9, height = 5)

ggplot(data, aes(x=Date, y = cases_roll)) +
    geom_line()


