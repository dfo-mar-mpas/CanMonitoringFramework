#code setup -------
#load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(patchwork)
library(viridis)
library(ggrepel)

#map projections
latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

##download MPAs and OECMs in Canada -----
mpas <- read_sf("data/shapefiles/MPAs.shp")%>%st_transform(CanProj)
tht <- read_sf("data/shapefiles/ThT_MPA.shp")%>%st_transform(CanProj)

mpas[grepl("offshore",tolower(mpas$NAME_E)),"geometry"] <- tht%>%dplyr::select(geometry)

#read in the PAs post the above code ------

CCAs <- mpas%>%
        mutate(name=ifelse(grepl("Eastport",NAME_E),"Eastport Marine Protected Area",NAME_E))%>%
        filter(OWNER_T == 1)%>%
        st_transform(CanProj)#government of conservation areas

naming_convention <- CCAs%>%
                     data.frame()%>%
                     dplyr::select(name,TYPE_E)%>%
                     distinct(name,.keep_all=TRUE)%>%
                     mutate(type=case_when(TYPE_E == "Other Effective Area-Based Conservation Measure (OECM)" ~ "Marine Refuge",
                                           grepl("Marine Protected Area",TYPE_E) ~ "Marine Protected Area",
                                           TRUE ~ "OECM"))%>%
                     dplyr::select(-TYPE_E)

CCA_simple <- CCAs%>%
              group_by(name)%>%
              summarise(geometry=st_union(geometry))%>%#gets rid of the zoning
              ungroup()%>%
              left_join(.,naming_convention)

#assign the tkt the right designation 
CCA_simple[grepl("offshore",tolower(CCA_simple$name)),"type"] <- "Marine Protected Area"


#basemap
basemap <- ne_states(country = "Canada",returnclass = "sf")%>%
  dplyr::select(name_en,geometry)%>%
  st_as_sf()%>%
  st_union()%>%
  st_transform(latlong)%>%
  st_as_sf()%>%
  mutate(country="Canada")%>%
  rbind(.,ne_states(country = "United States of America",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="US"),
        ne_states(country = "Greenland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Greenland"),
        ne_states(country = "Iceland",returnclass = "sf")%>%
          dplyr::select(name_en,geometry)%>%
          st_as_sf()%>%
          st_union()%>%
          st_transform(latlong)%>%
          st_as_sf()%>%
          mutate(country="Iceland"))%>%
  st_transform(CanProj)

eez <- read_sf("Data/Shapefiles/Canada_EEZ.shp")%>%
       st_transform(CanProj)

bioregions <- read_sf("Data/Shapefiles/DFO_Marine_Bioregions_Clipped_1M_CAEAC_2012_05_31.shp")%>%
              st_transform(latlong)%>%
              filter(!Legend %in% c("Saint-Pierre et Miquelon / Saint-Pierre et Miquelon",
                                    "13. Great Lakes / Grands Lacs"))%>%
              mutate(name=sub("/.*", "", Legend),
                     name=gsub('[[:digit:]]+', '', name),
                     name=gsub("\\.","",name),
                     name=trimws(name),
                     name=factor(name,levels=c("Offshore Pacific","Northern Shelf","Southern Shelf","Strait of Georgia",
                                               "Western Arctic","Arctic Archipelago","Arctic Basin","Hudson Bay Complex","Eastern Arctic",
                                               "Newfoundland-Labrador Shelves","Gulf of Saint Lawrence","Scotian Shelf")))
        

plot_region <- eez%>%st_bbox()

#make a plot of Marine Refuges, OECMS and MPAs in Canada

p1 <- ggplot()+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=eez,fill=NA)+
  geom_sf(data=CCA_simple,fill="grey90")+
  geom_sf(data=CCA_simple%>%filter(type=="Marine Protected Area"),fill="red")+
  theme_bw()+
  coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)])+
  labs(title = "Canadian Federal Marine Protected Areas")

p2 <- ggplot()+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=eez,fill=NA)+
  geom_sf(data=CCA_simple,fill="grey90")+
  geom_sf(data=CCA_simple%>%filter(type!="Marine Protected Area"),fill="blue")+
  theme_bw()+
  coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)])+
  labs(title = "Canadian Federal OECMs")

p3 <- p1 + p2 + plot_layout(ncol=2)

ggsave("figures/ClosurePlot.png",p3,height=6,width=10,units="in",dpi=300)  

ggsave("figures/ClosurePlot_lg.png",p3,height=6*3,width=10*3,units="in",dpi=300)  


#bioregion plot

p4 <- ggplot()+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=bioregions,aes(fill=name))+
  theme_bw()+
  coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)])+
  scale_fill_viridis(discrete = TRUE)+
  geom_label_repel(data=bioregions,aes(label=name,geometry=geometry),
                   stat="sf_coordinates",
                   min.segment.length = 0)+
  theme(legend.position = "none")+
  labs(fill="")

ggsave("r:/Science/CESD/HES_MPAGroup/Manuscripts/Snelgrove - OECM report/BioregionPlot.png",p4,height=7,width=5.5,units="in",dpi=300)
