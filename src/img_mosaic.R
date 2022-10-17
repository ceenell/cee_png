
library(tidyverse)
library(imager)
library(lwgeom)
library(purrr)
library(sf)
library(truchet)
library(magick)

theme_set(theme_void()+theme(legend.position = 'none'))

cee <- load.image("in/cee_me.jpeg")

## convert to greyscale
cee_gs <- grayscale(cee) 
cee_rs <- cee_gs %>% 
  imresize(scale = 1/10, interpolation = 6) %>%
  as.data.frame()

f <- ecdf(cee_gs)
f(cee_gs) %>% as.cimg(dim=dim(cee_gs)) %>% plot()

cee_gs %>%
  ggplot()+
  geom_point(aes(x, y, color = value)) +
  # change the colors!!
  scale_color_gradientn(colors = c("purple","plum","pink", "lavender","turquoise"),
                        c(0,.22,0.37, 0.55 ,1)) +
  coord_equal()
ggsave('cee_pride.png', width = 7)

cee_gs %>%
  ggplot()+
  geom_point(aes(x, y, color = value)) +
  scale_color_viridis_c(option = 'cividis') +
  theme(legend.position = 'none') +
  coord_equal()
ggsave('cee_cvidis.png', width = 7)

## create mosaic tile
# This will use a smaller subset of points to create the mosaic, which will then be rescaled
s <- 15

xlim <- c(min(cee_gs$x)/s - 4, max(cee_gs$x)/s + 4)
ylim <- c(min(cee_gs$y)/s - 4, max(cee_gs$y)/s + 4)

# Create a data frame with the coordinates for the tiles and define a scale parameter
m_1 <- expand.grid(x = seq(xlim[1], xlim[2], 1),
                   y = seq(ylim[1], ylim[2], 1)) %>%
  mutate(tiles = sample(c("dl", "dr"), n(), replace = TRUE),
         scale_p = 1) 

## assemble mosaic
m_1 <- st_truchet_ms(df = m_1) 

## plot mosaic
ggplot() +
  geom_sf(data = m_1 ,
          aes(fill = color),
          color = "white")

m_2 <- m_1 %>% 
  # Dissolve boundaries
  st_truchet_dissolve() %>% 
  # Buffer the polygons
  st_buffer(dist = -0.15) %>%
  # Adjust the color field to distinguish it from the original polygons
  mutate(color = color + 2)

# Remove empty geometries
m_2 <- m_2[!st_is_empty(m_2), , drop = FALSE]

# convert to MULTI
m_1_lines <- m_1 %>% 
  st_truchet_dissolve() %>% 
  st_cast(to = "MULTILINESTRING")
m_2_lines <- m_2 %>% st_cast(to = "MULTILINESTRING")

## scale up for full image dimensions
m_1_union <- st_union(m_1)
m_2_union <- st_union(m_2)

m_1_union <- (m_1_lines * s) %>% st_sf()
m_2_union <- (m_2_lines * s) %>% st_sf()

ggplot() +
  geom_sf(data = m_2_union, color = "orangered") +
  geom_sf(data = m_1_union,  color = "lavender", size = 1)

mosaic <- rbind(m_1_union, m_2_union)
mosaic # 526

## slice lines into pixel-level segments 
# Use the bounding box of the mosaic to define the extents of the grid that becomes the blade
bbox <- st_bbox(mosaic) %>% round()

# Create a data frame with the start and end points of the lines that become the blade to split the mosaic lines
blade <- data.frame(
  x_start = c(bbox$xmin:bbox$xmax,rep(bbox$ymin,length(bbox$ymin:bbox$ymax))),
  x_end = c(bbox$xmin:bbox$xmax,rep(bbox$xmax,length(bbox$ymin:bbox$ymax))),
  y_start = c(rep(bbox$ymin,length(bbox$xmin:bbox$xmax)),bbox$ymin:bbox$ymax),
  y_end = c(rep(bbox$ymax,length(bbox$xmin:bbox$xmax)),bbox$ymin:bbox$ymax)) %>% 
  # Shift to avoid perfect overlap with lines in the mosaic
  mutate(across(everything(),  ~ .x + 0.18))

# Create the blade and convert to simple features
blade_sf <- purrr::pmap(blade, 
                     function(x_start, x_end, y_start, y_end){
                       st_linestring(
                         matrix(c(x_start,y_start,x_end,y_end),
                                ncol = 2,byrow = TRUE))}) %>%
  st_as_sfc() 
blade_sf # 952

# use blade to split lines
mosaic_lines <- mosaic %>% st_split(blade_sf)
mosaic_lines

# extract sliced lines
mosaic_lines_cast <- mosaic_lines %>%
  st_collection_extract(type = "LINESTRING") %>%
  st_cast(to = "LINESTRING") %>%
  mutate(id = 1:n())
mosaic_lines_cast

cee_sf <- cee_gs %>%
  st_as_sf(coords = c("x", "y"))

# borrow grayscale values from img
value <- cee_sf[mosaic_lines %>% st_nearest_feature(cee_sf),] %>%
  pull(value)
mosaic_lines$value <- value

# plot image tiles --------------------------------------------------------

ggplot() +
  geom_sf(data = mosaic_lines %>%
            st_set_agr("constant") %>%
            st_crop(cee_sf),
          # Reverse the valence of values
          aes(size = -value)
          ) +
  scale_size(range = c(0.01, 1)) + 
  coord_sf(expand = FALSE) + 
  theme_void() + 
  theme(legend.position = "none")


ggsave('gears_bw.png')
