sports_analytics
================
Rick van Mil
2022-12-02

``` r
library(plyr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.0 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.1 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::arrange()   masks plyr::arrange()
    ## ✖ purrr::compact()   masks plyr::compact()
    ## ✖ dplyr::count()     masks plyr::count()
    ## ✖ dplyr::failwith()  masks plyr::failwith()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::id()        masks plyr::id()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::mutate()    masks plyr::mutate()
    ## ✖ dplyr::rename()    masks plyr::rename()
    ## ✖ dplyr::summarise() masks plyr::summarise()
    ## ✖ dplyr::summarize() masks plyr::summarize()

``` r
library(dplyr)
library(tidyverse)
library(tidyverse)
library(StatsBombR)
```

    ## Loading required package: stringi
    ## Loading required package: rvest
    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding
    ## 
    ## Loading required package: RCurl
    ## 
    ## Attaching package: 'RCurl'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete
    ## 
    ## Loading required package: doParallel
    ## Loading required package: foreach
    ## 
    ## Attaching package: 'foreach'
    ## 
    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when
    ## 
    ## Loading required package: iterators
    ## Loading required package: parallel
    ## Loading required package: httr
    ## Loading required package: jsonlite
    ## 
    ## Attaching package: 'jsonlite'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten
    ## 
    ## Loading required package: sp

    ## Warning: replacing previous import 'foreach::when' by 'purrr::when' when loading
    ## 'StatsBombR'

    ## Warning: replacing previous import 'jsonlite::flatten' by 'purrr::flatten' when
    ## loading 'StatsBombR'

    ## Warning: replacing previous import 'foreach::accumulate' by 'purrr::accumulate'
    ## when loading 'StatsBombR'

``` r
library(ggsoccer)
```

``` r
Comp <- FreeCompetitions()
```

    ## [1] "Whilst we are keen to share data and facilitate research, we also urge you to be responsible with the data. Please credit StatsBomb as your data source when using the data and visit https://statsbomb.com/media-pack/ to obtain our logos for public use."

``` r
ucl <- Comp %>%
  filter(competition_id==16 & season_name=="2018/2019")

matches <- FreeMatches(ucl)
```

    ## [1] "Whilst we are keen to share data and facilitate research, we also urge you to be responsible with the data. Please credit StatsBomb as your data source when using the data and visit https://statsbomb.com/media-pack/ to obtain our logos for public use."

``` r
events_df <- get.matchFree(matches)
```

    ## [1] "Whilst we are keen to share data and facilitate research, we also urge you to be responsible with the data. Please credit StatsBomb as your data source when using the data and visit https://statsbomb.com/media-pack/ to obtain our logos for public use."

``` r
clean_df <- allclean(events_df)
```

    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = c("period", "match_id")

``` r
finale <- clean_df %>%
  filter(player.name == 'Virgil van Dijk') %>%
  filter(type.name == 'Pass')
```

![](champions_finale1819_files/figure-gfm/pressure-1.png)<!-- -->

``` r
#Champions leugaeu finale between tottenham and liverpool

ucl<- Comp %>%
  filter(competition_id==16 & season_name=="2018/2019")
matches <- FreeMatches(ucl)
```

    ## [1] "Whilst we are keen to share data and facilitate research, we also urge you to be responsible with the data. Please credit StatsBomb as your data source when using the data and visit https://statsbomb.com/media-pack/ to obtain our logos for public use."

``` r
events_df <- get.matchFree(matches)
```

    ## [1] "Whilst we are keen to share data and facilitate research, we also urge you to be responsible with the data. Please credit StatsBomb as your data source when using the data and visit https://statsbomb.com/media-pack/ to obtain our logos for public use."

``` r
clean_df <- allclean(events_df)
```

    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = "id"
    ## Joining, by = c("period", "match_id")

``` r
Salah_pressure <- clean_df %>%
  filter(player.name == "Mohamed Salah") %>%
  filter(type.name == "Pressure")

Arnold_pressure <- clean_df %>%
  filter(player.name == "Trent Alexander-Arnold") %>%
  filter(type.name == "Pressure")
  

ggplot(Salah_pressure) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#400D51', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="Salah player stats",
       subtitle="UEFA Champions League Final 18/19",
       caption="Data Source: StatsBomb") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which will
    ## replace the existing scale.
    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

![](champions_finale1819_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(Arnold_pressure) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#400D51', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.5, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="Alexander Arnold stats",
       subtitle="UEFA Champions League Final 18/19",
       caption="Data Source: StatsBomb") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which will
    ## replace the existing scale.
    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

![](champions_finale1819_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
liverpool_shot <- clean_df %>%
  filter(type.name == 'Shot') %>%
  filter(team.name == 'Liverpool') %>%
  select(player.name, location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg)

tottenham_shot <- clean_df %>%
  filter(type.name == 'Shot') %>%
  filter(team.name == 'Tottenham Hotspur') %>%
  select(player.name, location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg)


ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#400D51') +
  geom_point(data=liverpool_shot, aes(x=location.x, y=location.y, size=shot.statsbomb_xg), color="red") +
  geom_point(data=tottenham_shot, aes(x=120-location.x, y=location.y, size=shot.statsbomb_xg), color="white") +
  labs(
    title="Liverpool vs Tottenham Hotspur",
    subtitle = "Shots Map | UEFA Champions League Final 2018/2019",
    caption="Data Source: StatsBomb"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )
```

![](champions_finale1819_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
shot_data_Liverpool <-clean_df %>% 

  filter(player.name %in% unique(clean_df$player.name),
         type.name == "Shot",
         team.name == "Liverpool") %>% 

  mutate(is_goal = ifelse(shot.outcome.name=="Goal", 1, 0) %>% 
           as.factor()) %>% 

  
  select(player.name, location.x, location.y, is_goal, shot.outcome.name, shot.statsbomb_xg, team.name)
 

shot_data_Tottenham<-clean_df %>% 

  filter(player.name %in% unique(clean_df$player.name),
         type.name == "Shot",
         team.name == "Tottenham Hotspur") %>% 

  mutate(is_goal = ifelse(shot.outcome.name=="Goal", 1, 0) %>% 
           as.factor()) %>% 

  
  select(player.name, location.x, location.y, is_goal, shot.outcome.name, shot.statsbomb_xg, team.name)

ggplot()+
  annotate_pitch(fill = "#400D51", dimensions = pitch_statsbomb)+
  theme_pitch()+
   theme(plot.background = element_rect(fill='#021e3f', color='#021e3f'),
         panel.background = element_rect(fill='#021e3f', color='#021e3f')) +
  coord_flip(xlim = c(60,120),
             ylim = c(0,80)) +

  geom_point(data = shot_data_Tottenham, 
             aes(x = location.x, 
                 y = location.y, 
                 colour = shot.outcome.name, 
                 size = shot.statsbomb_xg)) 
```

![](champions_finale1819_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
shot_data_Liverpool <-clean_df %>% 

  filter(player.name %in% unique(clean_df$player.name),
         type.name == "Shot",
         team.name == "Liverpool") %>% 

  mutate(is_goal = ifelse(shot.outcome.name=="Goal", 1, 0) %>% 
           as.factor()) %>% 

  
  select(player.name, location.x, location.y, is_goal, shot.outcome.name, shot.statsbomb_xg, team.name)
 

shot_data_Tottenham<-clean_df %>% 

  filter(player.name %in% unique(clean_df$player.name),
         type.name == "Shot",
         team.name == "Tottenham Hotspur") %>% 

  mutate(is_goal = ifelse(shot.outcome.name=="Goal", 1, 0) %>% 
           as.factor()) %>% 

  
  select(player.name, location.x, location.y, is_goal, shot.outcome.name, shot.statsbomb_xg, team.name)

ggplot()+
  annotate_pitch(fill = "#400D51", dimensions = pitch_statsbomb)+
  theme_pitch()+
   theme(plot.background = element_rect(fill='#021e3f', color='#021e3f'),
         panel.background = element_rect(fill='#021e3f', color='#021e3f')) +
  coord_flip(xlim = c(60,120),
             ylim = c(0,80)) +

  geom_point(data = shot_data_Liverpool, 
             aes(x = location.x, 
                 y = location.y, 
                 colour = shot.outcome.name, 
                 size = shot.statsbomb_xg)) 
```

![](champions_finale1819_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot()+
  annotate_pitch(fill = "#400D51",dimensions = pitch_statsbomb)+
  theme_pitch()+
  theme(plot.background = element_rect(fill='#021e3f', color='#021e3f'),
        text = element_text(family="Geneva", color='grey'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
         panel.background = element_rect(fill='#021e3f', color='#021e3f')) +
  coord_flip(xlim = c(60,120),
             ylim = c(0,80)) +

  

  geom_point(data = shot_data_Liverpool, 
             aes(x = location.x, 
                 y = location.y, 
                 colour = shot.outcome.name, 
                 size = shot.statsbomb_xg)) +
  
 
facet_wrap(~player.name) + labs(
    title="Liverpool vs Tottenham Hotspur",
    subtitle = "Shots Liverpool | UEFA Champions League Final 2018/2019",
    caption="Data Source: StatsBomb") 
```

![](champions_finale1819_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot()+
  annotate_pitch(fill = "#400D51",dimensions = pitch_statsbomb)+
  theme_pitch()+
  theme(plot.background = element_rect(fill='#021e3f', color='#021e3f'),
        text = element_text(family="Geneva", color='grey'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
         panel.background = element_rect(fill='#021e3f', color='#021e3f')) +
  coord_flip(xlim = c(60,120),
             ylim = c(0,80)) +

  

  geom_point(data = shot_data_Tottenham, 
             aes(x = location.x, 
                 y = location.y, 
                 colour = shot.outcome.name, 
                 size = shot.statsbomb_xg)) +
  
 
facet_wrap(~player.name) + labs(
    title="Liverpool vs Tottenham Hotspur",
    subtitle = "Shots Tottenham | UEFA Champions League Final 2018/2019",
    caption="Data Source: StatsBomb") 
```

![](champions_finale1819_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
