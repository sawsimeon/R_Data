ptt <- readxl::read_excel("C:/Users/Saw/Downloads/Gas_Station_Excel.xlsx",
                          sheet = 1)
esso <- readxl::read_excel("C:/Users/Saw/Downloads/Gas_Station_Excel.xlsx",
                           sheet = 2)
shell <- readxl::read_excel("C:/Users/Saw/Downloads/Gas_Station_Excel.xlsx",
                            sheet = 3)
bcp <- readxl::read_excel("C:/Users/Saw/Downloads/Gas_Station_Excel.xlsx", 
                          sheet = 4)
caltex <- readxl::read_excel("C:/Users/Saw/Downloads/Gas_Station_Excel.xlsx", 
                             sheet = 5)
ptg <- readxl::read_excel("C:/Users/Saw/Downloads/Gas_Station_Excel.xlsx", 
                          sheet = 6)
other <- readxl::read_excel("C:/Users/Saw/Downloads/Gas_Station_Excel.xlsx", 
                            sheet = 7)
ptt$Label <- c("PTT")
esso$Label <- c("Esso")
shell$Label <- c("Shell")
bcp$Label <- c("BCP")
caltex$Label <- c("Caltex")
ptg$Label <- c("PTG")
other$Label <- c("Others")
df <- rbind(ptt, esso, shell, bcp, caltex, ptg, other)
library(data.table)
df <- df[!duplicated(rleidv(df, cols = c("long", "lat"))), ]

library(maps)
library(mapdata)
library(jsonlite)
library(ggmap)
w2hr <- map_data("world2Hires")
thailand <- subset(w2hr, region == "Thailand")

library(rgeos)
library(raster)

Thailand <- getData('GADM', country = 'Thailand', level = 0)
Thailand <- spTransform(Thailand, CRS("+proj=longlat +datum=WGS84")) 


inout = over(
  SpatialPoints(df[,c("long","lat")],proj4string=CRS(projection(Thailand))),
  as(Thailand,"SpatialPolygons")
)

df_fixed <- cbind(df, inout)

df_fixed_no_na <- na.omit(df_fixed)

library(ggplot2)
ggplot() + geom_polygon(data = thailand, aes(x = long, y = lat, group = group), fill = "#E8E8E8") +
  coord_fixed(1.3) +
  geom_point(data = df_fixed_no_na, aes(x = long, y = lat, colour = Label, fill = Label), size = 1.5, alpha = 0.4) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    legend.position = "none"
    
  )


  ### Gas Station Geo Coding

  library(rvest)
  library(XML)
  library(rlist)
  url <- c("C:/Users/Saw/Downloads/PTT_MAP_Central.html")
  x <- readHTMLTable(url, encoding = "UTF-8", trim = TRUE)
  tables <- list.clean(x, fun = is.null, recursive = TRUE)
  typeof(tables)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
tables[[which.max(n.rows)]]

### Gas Station Geo Location of Caltex
library(rvest)
url <- c("C:/Users/Saw/Downloads/Caltex.html")
webpage <- read_html(url, encoding = "UTF-8") %>%
html_nodes(".station-item") %>% html_text()

### Gas Station Geo Location of Shell 10330
library(rvest)
url <- c("C:/Users/Saw/Downloads/Shell_10330_Thailand.html")
read_html(url, encoding = "UTF-8") %>%
html_nodes(".iframed-app__iframe") %>% html_text()


#### Thailand Gas Station Company 
### PTT, ESSO, Bangchak, Shell, Chevron, IRPC, Thaioil, Susco, PTG and others
### PTT Public Company Limited (PTT)
### Thai Oil Public Company Limited (Thai Oil)
### Esso (Thailand) Public Company Limited (Esso)
### Shell (Thailand) Co., Ltd. (Shell)
### Bangchak Petroleum Public Company Limited (Bangchak)
### IRPC Public Company Limited (IRPC)
### PTG Energy Pulic Company Limited (PTG)
### Susco Public Company Limited ("Susco")
library(rvest)
url <- c("https://en.wikipedia.org/wiki/List_of_districts_of_Thailand")
webpage <- read_html(url, encoding = "UTF-8") %>%
html_node(".mw-parser-output , tr:nth-child(1) td") %>% html_table()

library(httr)
library(XML)
r <- GET(url)
doc <- readHTMLTable(doc = content(r, "text"))

library(ggmap)
