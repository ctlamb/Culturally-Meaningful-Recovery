library(tidyverse)
library(here)
library(hrbrthemes)
library(ggforce)
library(readxl)
library(mapview)
library(maps)
library(sf)
library(RColorBrewer)
library(forcats)
library(ggrepel)
library(patchwork)

##load data
df <- read_excel(here::here("data/abundance.xlsx"),"Sheet2")


##if no error, use 15% above and below
df <- df%>%
  mutate(lower=case_when(is.na(lower)~N-(N*.15), TRUE~lower),
         upper=case_when(is.na(upper)~N+(N*.15), TRUE~upper))

##plot
abundance <- ggplot(df%>%filter(!Species%in%"Atlantic cod (Canada)"), aes(x=Year, y=N, ymin=lower,ymax=upper))+
  geom_smooth(linetype="dashed",color="black",size=0.5)+
  geom_text_repel(
    data=df%>%filter(Species%in%"American bison" & Year==2022),
    aes(label=format(N, big.mark = ",",
                     scientific = FALSE)),
    color="grey50",
    nudge_y=6000000,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
  )+
  geom_text_repel(
    data=df%>%filter(Species%in%"American bison" & Year==1890),
    aes(label=format(N, big.mark = ",",
                     scientific = FALSE)),
    color="grey50",
    nudge_x=-75,
    segment.curvature = 0.1,
    segment.ncp = 3,
    segment.angle = 20,
  )+
  geom_text_repel(
    data=df%>%filter(Species%in%"Caribou (Klinse-Za)" & Year==2022),
    aes(label=format(N, big.mark = ",",
                     scientific = FALSE)),
    color="grey50",
    nudge_y=450,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
  )+
  geom_text_repel(
    data=df%>%filter(Species%in%"Caribou (Klinse-Za)" & Year==2013),
    aes(label=format(N, big.mark = ",",
                     scientific = FALSE)),
    color="grey50",
    nudge_x=-100,
    segment.curvature = 0.1,
    segment.ncp = 3,
    segment.angle = 20,
  )+
  geom_text_repel(
    data=df%>%filter(Species%in%"Pacific salmon (Columbia River)" & Year==2022),
    aes(label=format(N, big.mark = ",",
                     scientific = FALSE)),
    color="grey50",
    nudge_y=1500000,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
  )+
  geom_text_repel(
    data=df%>%filter(Species%in%"Pacific salmon (Columbia River)" & Year==1938),
    aes(label=format(N, big.mark = ",",
                     scientific = FALSE)),
    color="grey50",
    nudge_x=-100,
    segment.curvature = 0.1,
    segment.ncp = 3,
    segment.angle = 20,
  )+
  geom_point()+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE),
                     limits = c(0, NA),
                     expand=c(0.01,0))+
  geom_linerange()+
  labs(y="N")+
  facet_wrap(vars(fct_relevel(Species,"Caribou (Klinse-Za)","American bison","Pacific salmon (Columbia River)")),scales="free_y")+
  theme_ipsum()
abundance


##maps
na <-st_read(here::here("data/administrative/North_America.shp"))%>%
  mutate(place="US/CA")%>%
  select(place)%>%
  st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")%>%
  rbind(st_read(here::here("data/administrative/Mexico.shp"))%>%
          mutate(place="Mex")%>%
          select(place)%>%
          st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45"))

#bison
bison <- st_read(here::here("data/ranges/bison/historic/B_Bison_Historic_Range.shp"))%>%
  st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")%>%
  mutate(sp="American bison",
         period="historic")%>%
  select(sp,period)%>%
  rbind(st_read(here::here("data/ranges/bison/current/data_0_CL.shp"))%>%
          st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")%>%
          mutate(sp="American bison",
                 period="current")%>%
          select(sp,period)
  )%>%
  st_intersection(na%>%group_by()%>%summarise())

#caribou
caribou <- st_read(here::here("data/ranges/caribou/historic/Caribo_range_bounds.shp"))%>%
  st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")%>%
  mutate(sp="Caribou",
         period="historic")%>%
  select(sp,period)%>%
  rbind(st_read(here::here("data/ranges/caribou/current/data_0_CL.shp"))%>%
          st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")%>%
          mutate(sp="Caribou",
                 period="current")%>%
          select(sp,period)
  )%>%
  st_intersection(na%>%group_by()%>%summarise())


#salmon
salm <- st_read(here::here("data/ranges/salmon/salm_range_Pac_salm_atlas_CL.shp"))%>%
  filter(id==1)%>%
  st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")%>%
  group_by()%>%
  summarise()%>%
  mutate(sp="Pacific salmon",
         period="historic")%>%
  select(sp,period)

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
salm <- salm%>%
  rbind(salm%>%
          mutate(sp="Pacific salmon",
                 period="current")%>%
          st_make_valid()%>%
          st_erase(st_read(here::here("data/ranges/salmon/salm_range_Pac_salm_atlas_CL.shp"))%>%
                     filter(id!=1)%>%
                     group_by()%>%
                     summarise()%>%
                     st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")%>%
                     st_make_valid()%>%
                     ungroup%>%
                     mutate(sp="Pacific salmon",
                            period="current")%>%
                     select(sp,period))
        )%>%
  st_intersection(na%>%group_by()%>%summarise())






mapview(bison%>%filter(period=="current"))
mapview(caribou)
mapview(salm)

na.simp <- na%>%st_simplify(10000,preserveTopology = TRUE)%>%
  st_transform("+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")

ranges <- rbind(bison,caribou, salm)%>%
  group_by(sp,period)%>%
  summarise()%>%
  st_simplify(1000,preserveTopology = TRUE)

mapview(ranges)


range.maps <- ggplot()+
  geom_sf(data=na.simp, size=0.1)+
  geom_sf(data=ranges%>%filter(period=="historic"), alpha=0.85, fill="#fdebca", color=NA)+
  geom_sf(data=ranges%>%filter(period=="current"),alpha=0.85, fill="#c79aa6", color=NA)+
  geom_sf(data=tibble(sp=c("Caribou","Pacific salmon"),
                      X=c(-122.5412, -121.786),
                      Y=c(55.845, 45.702))%>%
            st_as_sf(coords=c("X","Y"),
                     crs=4326))+
  facet_wrap(vars(fct_relevel(sp,"Caribou","American bison","Pacific salmon")))+
  theme(legend.position = "none")+
  theme_ipsum()+
  scale_x_continuous(expand = c(0,0), limits = c(-40E5, 27E5))+
  scale_y_continuous(expand = c(0,0), limits = c(30E5, 96E5))




##plot together
range.maps/abundance+ 
  plot_layout(heights = c(1.5, 1))+
  plot_annotation(tag_levels = 'A')

ggsave(here::here("plots","fig2.png"), width=12, height=8.5, dpi=400)



##caribou abundance

##100 lbs/male bou ##https://www.adfg.alaska.gov/index.cfm%3Fadfg=caribouhunting.main#:~:text=Weights%20of%20adult%20bulls%20average,(45%20kg)%20of%20meat.
##~3% bull harvest rate is sustainable ## https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/wildlife-wildlife-habitat/caribou/review_of_northern_caribou_harvest_management_and_science.pdf
##use 6 oz (0.38 lbs) as serving size.. Canada food guide says 2.25 oz of meat.,but cmon..canada.ca/content/dam/hc-sc/migration/hc-sc/fn-an/alt_formats/hpfb-dgpsa/pdf/food-guide-aliment/serving_meat-viande_portion-eng.pdf
##Saulteau= 1270 ppl https://fnp-ppn.aadnc-aandc.gc.ca/fnp/Main/Search/FNRegPopulation.aspx?BAND_NUMBER=542&lang=eng
##West Moberly = 366 https://fnp-ppn.aadnc-aandc.gc.ca/fnp/Main/Search/FNRegPopulation.aspx?BAND_NUMBER=545&lang=eng
##total=1270+366=1636

bou.meat <- 100
serving  <- 0.38
ppl      <- 1636
meals    <-15

##harvest needed
harvest.needed <-(ppl*serving*meals)/bou.meat

##
bou.needed <-harvest.needed/0.03

# library(sf)
# herds <- st_read("/Users/claytonlamb/Dropbox/Documents/University/Work/WSC/CaribouIPM_BCAB/data/Spatial/herds/ipm_herds.shp")%>%
#   filter(herd=="Klinse-Za")%>%
#   mutate(area=st_area(.)/1E6)
##KZ herd area=6452 km2
