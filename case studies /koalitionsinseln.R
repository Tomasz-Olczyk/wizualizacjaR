####### Package management #######
## install and load needs, if not yet present
# install.packages("needs")
# library(needs)

## packages used in this markdown document
needs(tidyverse, sf, ggpattern)


####### Loading data #######
# official elections results provided here: https://bundeswahlleiterin.de/bundestagswahlen/2025/ergebnisse/opendata.html#a2ed5bd6-ea23-4641-8f5b-e5fbb634d352
data = read_csv2("https://bundeswahlleiterin.de/bundestagswahlen/2025/ergebnisse/opendata/btw25/csv/kerg2.csv", skip = 9) %>% 
    filter(Gebietsart == "Wahlkreis") %>% 
    filter(Stimme == 2) %>% 
    select(name = Gebietsname, party = Gruppenname, share = Prozent) %>%
    mutate(party = if_else(party %in% c("CDU", "CSU"), "Union", party)) %>%
    mutate(party = if_else(party == "GRÜNE", "Grüne", party)) %>%
    mutate(party = if_else(party == "Die Linke", "Linke", party)) %>%
    pivot_wider(names_from = party, values_from = share) %>% 
    print()


#######  Calculating possible coalitions #######
possible_coalitions = data %>% 
    rowwise() %>%
    select(-Ungültige, -Gültige) %>% 
    mutate_at(2:92, ~ if_else(. < 5, 0, .)) %>%
    mutate(share_in_parliament = sum(c_across(2:92)[c_across(2:92) >= 5.0], na.rm = T)) %>%
    mutate(Union_SPD = if_else(
        Union >= 5 & SPD >= 5 &
        (Union + SPD) / share_in_parliament >= 0.5, T, F)) %>%
    ungroup() %>%
    select(name, Union, AfD, SPD, Grüne, Linke, BSW, FDP, share_in_parliament, Union_SPD) %>%
    print()


#######  Reading geodata #######
# official constituencies map data as provided here: https://bundeswahlleiterin.de/bundestagswahlen/2025/wahlkreiseinteilung/downloads.html
geo_wk = st_read("btw25_geo_wk.geojson") %>% mutate(WKR_NR = formatC(WKR_NR, width = 3, flag = "0")) %>% st_transform(3035)
geo_laender_lines = st_read("laender_lines.geojson") %>% st_transform(3035)


#######  Preparing function for hachures with ggpatterns #######
# see: https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns.html
multicolor_stripe_pattern = function(params, boundary_df, aspect_ratio, legend = FALSE){
    args = as.list(params)
    args = args[grep("^pattern_", names(args))]
    
    args$pattern_colour = strsplit(args$pattern_colour, ",")[[1]]
    args$pattern_fill = strsplit(args$pattern_fill, ",")[[1]]

    args$pattern = "stripe"
    args$x = boundary_df$x
    args$y = boundary_df$y
    args$id = boundary_df$id
    args$prefix = ""

    do.call(gridpattern::patternGrob, args)
}
options(ggpattern_geometry_funcs = list(multicolor_stripe = multicolor_stripe_pattern))


#######  Plotting the map #######
# Filtering districts with majority
ids_wks_with_majority = possible_coalitions %>% filter(Union_SPD == T) %>% pull(name)
data_this_coaltion = geo_wk %>% filter(WKR_NAME %in% ids_wks_with_majority) 
# Dissolving, so we get stripes all across the area and not for each district individually
data_this_coaltion_multi = data_this_coaltion %>% group_by() %>% summarise()
    
ggplot() +
    # grey base map
    geom_sf(data = geo_wk, fill = "#eef1f3", alpha = 1, color = "transparent", show.legend = F) +
    # pattern fill / hachures
    geom_sf_pattern(
        data = data_this_coaltion_multi,
        pattern = 'multicolor_stripe',
        pattern_fill = "#d94d41,#615952",
        pattern_density = 1,
        pattern_size = 0,
        pattern_spacing = 0.035,
        pattern_angle = 35,
        color = "white",
        show.legend = FALSE
  ) +
    # country boders on top
    geom_sf(data = geo_laender_lines, color = "#f0f0f0", linewidth = 0.35, fill = NA) +
    coord_sf(datum = NA) +
    theme_light()
