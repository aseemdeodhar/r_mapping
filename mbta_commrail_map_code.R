# Code block for mapping:
# This is the code block along with the feeder files I used to make the MBTA Commuter Rail Map
# I showed in my presentation last week.
#
# Feel free to run it, change various aesthetics, supplement different shapefiles, etc.
# Its really easy to remember different functions and variables once you see them in action,
# so I hope this helps you!
#
# Please reach out if you ever have any questions!
#
# adeodhar at mapc org


# Packages ----------------------------------------------------------------
library(tidyverse)
library(sf)


# Loading Data ------------------------------------------------------------

# Massachusetts Municipalities with MBTA Assessment:
mbta_muni_shp <- read_sf('mbta_muni/mbta_muni.shp')

# MBTA Commuter Rail Lines
mbta_cr_lines <- read_sf('mbta_cr/mbta_cr_lines.shp') %>% 
  st_transform(crs = st_crs(mbta_muni_shp))

# Use st_transform to change the CRS (Coordinate Reference System) of a spatial object to another.
# You can either specify another spatial file to copy its CRS, or specify the EPSG code for a specific CRS
# For example, the EPSG code for WGS 84 is 4326

# MBTA Commuter Rail Stops
mbta_cr_stops <- read_sf('mbta_cr/mbta_cr_stops.shp') %>% 
  st_transform(crs = st_crs(mbta_muni_shp))


# Mapping Code ------------------------------------------------------------

ggplot()+
  
  # Remember: the first shape you load, goes at the bottom of the layers
  geom_sf(data = mbta_muni_shp, # loading the municipal boundaries shapefile (polygon)
          aes(geometry = geometry), # the geometry doesn't need to be specified, but it is good practice.
          color = 'white',
          fill = '#D9D9D2', # Hex color code. #D9D9D2 is a great yellow tinted warm grey
                            # good hex code picker website: https://htmlcolorcodes.com/color-picker/
          size = 0.25)+

    # same shapefile as above, but filtered to only MBTA municipalities
  geom_sf(data = mbta_muni_shp %>% filter(ismbta == 1),
          aes(geometry = geometry,
              fill = pp_sqmil), # adding a 'fill' aesthetic. Row values will color polygons
          color = 'white',
          size = 0.25)+
  
  # Commuter Rail tracks linestring geometry
  geom_sf(data = mbta_cr_lines,
          aes(geometry = geometry),
          color = '#7B388C')+
  
  # Commuter Rail point geometry
  geom_sf(data = mbta_cr_stops,
          aes(geometry = geometry),
          size = 1,
          color = 'red')+
  
  # Edit scaling and display of data in the 'fill' aesthetic:
  # note that depending on your purpose, and desired level of customization,
  # there are multiple ways to edit these values.
  # check:
  # https://ggplot2.tidyverse.org/reference/scale_manual.html
  # https://ggplot2.tidyverse.org/reference/scale_gradient.html
  # https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html
  # Remember the aesthetic you are making the edits for (color, alpha, fill)
  scale_fill_fermenter(breaks = c(0, 100, 500, 1000, 2500, 5000, 10000, 15000, 20000),
                       # Alternatively, 'n.breaks = 5' as an example, will break your scale at 5 points
                       # you can get a general idea of break points with the quantile() function.
                       name="Population Density",
                       palette = 'Greens',
                       direction = 1)+
  
  # limit the extents of the map to these values
  # you can get the desired limit values with the function sf::st_bbox(mbta_cr_stops)
  coord_sf(xlim = c(-71.84876, -70.62623), 
           ylim = c(41.58033, 42.79958),
           expand = TRUE)+
  
  # this theme removes all x and y axes ticks and labels as well the grey background.
  # If you'd like the grey background removed, but want the coordinate points, use theme_minimal()
  # There are multiple ggplot2 themes already loaded, try them out! Many more are available to download.
  theme_void()+
  
  # Add labels and titles to the map:
  labs(title = 'MBTA Commuter Rail Network',
       subtitle = "Overlaid on a Population Density Map",
       caption = 'Population Density is measured as persons per square mile (Census 2010) 
       \n MA municipalities under the MBTA\'s ambit highlighted')+
  
  # REMEMBER: the '\n' expression in a string gives you a new line. Very useful when adding large amounts of text
  
  # the theme() function can add and edit lots of different visual elements to your plot.
  # Here, I've only edited the text in the map using element_text().
  # Other stuff you can add are background colors, borders, positions of text, fonts etc.
  theme(plot.title = element_text(face = 'bold',
                                  size = 14),
        plot.subtitle = element_text(face = 'plain',
                                     size = 11),
        plot.caption = element_text(face = 'italic',
                                    size = 6),
        plot.caption.position = 'plot',
        legend.title = element_text(face = 'bold',
                                    size = 9))+
  
  # Save your map file.
  # ggsave() supports multiple image file extensions like .jpeg, .png, .bmp, .tiff, etc .svg etc.
  # For raster outputs like jpeg and png, you can specify a DPI (Dots Per Inch) as well.
  # Other variables are height, width, and units.
  # default accepted units are 'in' for inches, 'cm' for centimeters, and 'mm' for millimeters
  # values accepted for dpi are any numeric value, or 'retina' for 320, 'print' for 300, or 'screen' for 72.
  ggsave('mbta_commrail_network.jpeg',
         height = 7,
         width = 5,
         units = 'in',
         dpi = 'retina')
