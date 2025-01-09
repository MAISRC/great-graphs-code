library(tidyverse)
library(viridis)
library(svglite)

set.seed(123) #CONTROL RANDOMNESS


# GRAPH 1 (BAR GRAPH) -----------------------------------------------------------------

#CREATE FAKE GROUP MEAN DATA FIRST
graph1.df = data.frame(
  materials = rep( #OUR MATERIAL TYPES
    c(
      "Anthropogenic",
      "Bark",
      "Feathers",
      "Fine stems",
      "Flowers",
      "Grasses and sedges",
      "Hair",
      "Intact coarse stems",
      "Invertebrate silk and eggs",
      "Leaves",
      "Moss",
      "Mud",
      "Needles",
      "Plant down",
      "Rootlets",
      "Shredded coarse stems",
      "Sticks"
    ),
    each = 4  #4 SPECIES
  ),
  species = rep(c("Setophaga discolor", "Setophaga pensylvanica", "Spizella pusilla", "Spizella passerina"), times = 17 ), #17 MATERIALS
  prop.nest.mass = rbeta( #OUR RANDOM SUBGROUP MEAN DATA
    n = 4 * 17,
    shape1 = 1,
    shape2 = 15
  )
)

#WE NOW WANT TO EXPAND THESE TO PRODUCE 10 RAW DATA POINTS FOR EACH SUBGROUP BASED ON THE ROUGH MEANS WE GOT ABOVE
graph1.df.raw = data.frame(materials = character(0),
                           species = character(0),
                           prop.nest.mass = numeric(0)) #BLANK DATA FRAME TO HOLD OUR RANDOM DRAWS

for(row in 1:nrow(graph1.df)) { #FOR EACH SPECIES/MATERIAL COMBO...
  for(data in 1:10) { #DRAW A RANDOM VALUE 10 TIMES
    
    graph1.df.raw = 
      rbind(graph1.df.raw,
            data.frame(materials = graph1.df$materials[row],
                       species = graph1.df$species[row],
                       prop.nest.mass = 
                         rbeta(1, shape1 = graph1.df$prop.nest.mass * 5,
                               shape2 = (1-graph1.df$prop.nest.mass) * 5))) #HERE, WE USE THE MEAN PROPORTION FROM THE DATA FRAME ABOVE AS THE MEAN VALUE IN A NEW RBETA DRAW.
  }
}

#NOW, WE OVERWRITE THE MEANS DATA FRAME ABOVE WITH THE ACTUAL NEW MEANS OF THE RAW DATA WE RANDOMLY GENERATED, JUST SO THEY MATCH.
graph1.df = graph1.df.raw %>% 
  group_by(species, materials) %>% 
  summarize(mean.prop = mean(prop.nest.mass))

#I THINK 17 MATERIALS IS TOO MANY TO PRESENT. LET'S INSTEAD CHOOSE 4 THAT SHOW THE RANGE IN VARIATION BETWEEN SPECIES AS EXEMPLARS.
(graph1.df.ranges = graph1.df %>% 
  group_by(materials) %>% #FOR EACH MATERIAL...
  summarize(prop.range = max(mean.prop) - min(mean.prop)) %>% #WHAT'S THE RANGE IN MEANS SEEN ACROSS SPECIES (MAX-MIN)?
  arrange(desc(prop.range)))

#SHREDDED COARSE STEMS IS HIGHEST (0.158), FLOWERS IS LOWEST (0.023). MOSS (.0877) AND STICKS (0.0458) ARE EQUIDISTANT IN THE MIDDLE. PLUS, ALL 4 OF THESE SEEM LIKE THEY'D BE UNIVERSALLY RELATABLE MATERIALS FOR MOST AUDIENCES, UNLIKE, SAY, "ANTHROPOGENIC" OR "PLANT DOWN", SO THAT'S A BONUS. 

#CUT TO JUST THOSE MATERIALS FOR GRAPHING
graph1.df.short = graph1.df %>% 
  filter(materials %in% c("Shredded coarse stems", "Moss", "Sticks", "Flowers")) 
graph1.df.raw.short = graph1.df.raw %>% 
  filter(materials %in% c("Shredded coarse stems", "Moss", "Sticks", "Flowers"))

#TO PLOT THESE SUB-GROUPS IN A LOGICAL ORDER, WE CAN REDEFINE THEM AS FACTORS AND SET THEIR LEVELS AND LABELS HERE. 
graph1.df.short$materials = factor(graph1.df.short$materials , 
                                   levels = rev(c("Shredded coarse stems", "Moss", "Sticks", "Flowers")),
                                   labels = rev(c("Shredded\ncoarse\nstems", "Moss", "Sticks", "Flowers")))
graph1.df.raw.short$materials = factor(graph1.df.raw.short$materials , 
                                   levels = rev(c("Shredded coarse stems", "Moss", "Sticks", "Flowers")),
                                   labels = rev(c("Shredded\ncoarse\nstems", "Moss", "Sticks", "Flowers")))

#OUR PLOT!
ggplot(graph1.df.short, #PROVIDE THIS DATA SET FOR MOST ELEMENTS, BUT NOT ALL
       aes(y = materials, #FLIP MATERIALS TO THE Y AXIS TO PLOT THE LONG LABELS HORIZONTALLY.
           x = mean.prop, #FLIP PROPORTION NEST MASS TO X AXIS.
           fill = species)) + #USE FILL COLOR FOR SPECIES
  #THE FIRST ELEMENT (SO IT'S ON THE BOTTOM) IS THE CONNECTING LINES THAT LINK THE RAW DATA TO EACH OTHER IN EACH SUBGROUP, HELPING CONVEY THE RANGE OF THE DATA AT A GLANCE. 
  geom_segment(inherit.aes = FALSE, #THIS USES DIFFERENT MAPPING AND DATA.
               #HERE, WE USE THE RAW DATA SET TO FIND THE MIN AND MAX VALUES OF THE RAW PROPORTION MASS DATA AND SET THOSE AS X AND XENDS FOR THE SEGMENTS. WE ALSO REPLICATE THE POSITION_DODGE USED BY LATER LAYERS HERE BY APPLYING OFFSET VALUES TO THE Y POSITIONS. 
               data = graph1.df.raw.short %>% 
                 mutate(materials = as.numeric(factor(materials)),
                        species = (as.numeric(factor(species))),
                        species_recode = recode(species,
                                                `1` = -0.3, 
                                                `2` = -0.1,
                                                `3` = 0.1, 
                                                `4` = 0.3)) %>% 
                 mutate(y_pos = materials + species_recode) %>% 
                 group_by(species, materials) %>% 
                 summarize(x_min = min(prop.nest.mass), 
                           x_max = max(prop.nest.mass),
                           y_pos = first(y_pos)),
               #MAP WHERE THE SEGMENTS SHOULD BE USING THE CALCULATED VALUES ABOVE.
               aes(x = x_min, 
                   xend = x_max, 
                   y = y_pos, 
                   yend = y_pos, 
                   group = species), 
               #MAKE THESE CONNECTING LINES THIN AND GRAY TO BE DE-EMPHASIZED.
               size = 0.6, 
               color = "gray80") +
  #NEXT, PLOT THE RAW DATA AS THIN GRAY VERTICAL BARS, ALA A BAR CODE PLOT. 
  geom_point(inherit.aes = FALSE, #THIS ALSO USES THE RAW DATA AND DIFFERENT MAPPING.
             data = graph1.df.raw.short,
             mapping = aes(x = prop.nest.mass, 
                           y = materials, 
                           group = species),
             color = "gray80",
             shape = "│", #CUSTOM UNICODE SHAPE
             position = position_dodge(width = 0.8), #DODGE THE SUBGROUPS APART VERICALLY INTO THEIR OWN "LINES"
             size = 4.5, 
             show.legend = FALSE) + #DON'T ADD THIS LAYER TO THE LEGEND.
  geom_point(shape = 21, #A CIRCLE WITH BOTH FILL AND STROKE COLORS.
           position = position_dodge(width = 0.8), #SAME AS ABOVE.
           size = 6) + 
  scale_y_discrete("") + #ELIMINATE THE REDUNDANT Y-AXIS TITLE
  scale_x_continuous(
    "Percent of total nest mass", #X-AXIS TITLE
    expand = c(0, 0), #REMOVE THE GAP THAT OTHERWISE GETS PLACED BETWEEN THE Y AXIS AND THE STARTS OF THE DATA.
    limits = c(-0.01, 0.61), #ENSURE ALL THE DATA ARE SHOWN BY FINDING GOOD BREAK POINTS AND GOING JUST A LITTLE BEYOND THEM. 
    breaks = c(0, 0.15, 0.3, 0.45, 0.6), #PROVIDE EVEN BREAKS AT REASONABLE VALUES, INCLUDING AT BOTH ENDS. 
    labels = c(0, 15, 30, 45, 60) #MAKE THE LABELS WHOLE %S INSTEAD OF PROPORTIONS LIKE THE RAW DATA.
  ) +
  scale_fill_discrete(
    "", #ELIMINATE THE REDUNDANT LEGEND TITLE--IT'S CLEAR THESE ARE SPECIES.
    type = c(viridis(4, end = 0.9)), #SWITCH TO THE VIRIDIS COLORBLIND-FRIENDLY PALETTE, BUT AVOID THE YELLOW COLOR AT THE END THAT LACKS CONTRAST WITH A WHITE BACKGROUND.
    limits = rev(levels(factor(graph1.df$species))), #SORT THE LEGEND KEYS TO MATCH THE ORDER IN THE FIGURE.
    labels = c("Spizella\npusilla", "Spizella\npasserina", "Setophaga\npensylvanica", "Setophaga\ndiscolor") #ADD IN LINE BREAKS TO FIT ALL THE LABELS IN.
  ) +
  theme(
    panel.background = element_rect(fill = "white"), #ELIMINATE GRAY BACKGROUND
    axis.line = element_line(color = "black", linewidth = 1.2), #INTENSIFY THE AXES LINES
    axis.title = element_text(size = 22, face = "bold", color = "black"), #MAKE THE AXES TITLES EASIER TO READ
    axis.text = element_text(size = 20, color = "black"), #ENLARGE AXIS LABELS.
    legend.text = element_text(size = 18, color = "black", face = "italic"), #SAME
    axis.ticks.length = unit(0.3, "cm"), #ENLARGE THE TICKS A LITTLE SO IT'S EASIER TO SEE THEM AND TO LINK MATERIAL TYPE LABELS BETTER WITH THEIR DATA VISUALLY.
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")), #ADD SPACE BETWEEN THE AXIS TITLE AND THE LABELS FOR EASIER PARSING.
    panel.grid.major.x = element_line(color = "gray60"), #ADD VERTICAL GRID LINES AT KEY INTERVALS BUT ZEBRA-STRIPE THEM AND USE GRAYS TO DE-EMPHASIZE THEM.
    panel.grid.minor.x = element_line(color = "gray80"),
    legend.key.spacing.x = unit(0.4, "cm"), #SPACE THE LEGEND KEYS APART FROM EACH OTHER SOMEWHAT
    legend.justification = "left", #CENTER ALIGN THE LEGEND
    legend.box = "horizontal", #PLACE IT HORIZONTAL AT THE TOP
    legend.position = "top" #PLACE IT ABOVE THE GRAPH
  )

ggsave(filename = "barplot.svg", height = 8.3, width = 8.3, dpi = "print") #SAVE IT PROGRAMMATICALLY AS A VECTOR-GRAPHICS FILE TYPE, THEN INSPECT AND ITERATE FOR PROPER SIZING. A WIDTH OF 8.3 INCHES IS ROUGHLY STANDARD FULL PAGE WIDTH. OTHER COMMON HEIGHTS ARE 3.5 IN AND 7.1 IN FOR SINGLE AND DOUBLE COLUMNS. CHECK WITH YOUR TARGET JOURNAL FOR MORE DETAILS 

# GRAPH 2 (BOXPLOT) -----------------------------------------------------------------


#CREATE FAKE DATA
graph2.df = data.frame(
  ecosystem = rep(c("Open", "Closed"), each = 60),
  perc.endemic.richness = c(rnorm(60, 46, 11), rnorm(60, 54, 13))
)

#BECAUSE SO MANY GROUPS HAVE RELATIVELY LITTLE DATA, I WANT TO COLLAPSE THE DATA DOWN A BIT. 

#OUR PLOT
ggplot(data = graph2.df, #MAP DATA SET GLOBALLY
       aes(x = ecosystem, y = perc.endemic.richness)) + #MAP X AND Y GLOBALLY
  geom_boxplot( #ADD OUR BOXPLOTS
    outlier.size = 5, #INCREASE OUTLIER SIZE
    linewidth = 1.5, #FATTEN ALL LINES
    outlier.shape = 21, #CHANGE OUTLIER POINTS TO ONES WITH SEPARATE STROKES AND FILLS
    outlier.color = "gray60", #SET BLACK STROKES
    outlier.fill = "transparent", #SET TRANSPARENT FILLS SO OVERLAPPING OUTLIERS ARE MORE NOTICEABLE.
    color = "gray60"
  ) +
  scale_x_discrete("", #SUPPRESS AXIS TITLE
                   limits = rev(levels(factor(graph2.df$ecosystem)))) + #RESORT GROUPS
  scale_y_continuous("Percent endemic\nspider species", #INFORMATIVE TITLE, PARSED ONTO TWO LINES.
                     limits = c(0, 100)) + #SET LIMITS TO MINIMIZE DISTORTION
  annotate("text", #ADD X AXIS LABELS INSIDE THE PLOTTING AREA AS ANNOTATIONS
    label = "Open (unforested)\necosystems\n(n = 25)",
    x = 1, #SPECIFY THEIR LOCATIONS MANUALLY
    y = 8.9,
    size = 6, #ENLARGE AND BOLDEN
    fontface = "bold"
  ) +
  annotate("text", #SAME AS ABOVE
    label = "Closed (forested)\necosystems\n(n = 31)",
    x = 2,
    y = 9,
    size = 6,
    fontface = "bold"
  ) +
  stat_summary(fun = mean, #MANUALLY ADD MEAN VALUES AS POINTS
    geom = "point",
    shape = 23, #USE DIAMONDS TO DISTINGUISH FROM OUTLIERS
    size = 8,
    color = "black", #MAXIMIZE CONTRAST WITH BACKGROUND
    fill = "white",
    stroke = 2.5
  ) +
  annotate("errorbar", #ADD AN "ERROR BAR" STYLE LINE SEGMENT TO HIGHLIGHT MEAN DIFFERENCE
    x = 1.5,
    ymin = mean(graph2.df$perc.endemic.richness[graph2.df$ecosystem == "Open"]),
    ymax = mean(graph2.df$perc.endemic.richness[graph2.df$ecosystem == "Closed"]),
    color = "black", #DE-EMPHASIZE TO REDUCE COGNITIVE LOAD
    linewidth = 1.25,
    width = 0.1 #REDUCE WIDTH OF CROSS-BEAMS
  ) +
  #ADD STATISTICS FOR THE MEAN DIFFERENCE COMPARISON. NEEDS TO BE THREE SEPARATE ANNOTATIONS TO HANDLE LINE BREAKS AND ITALICS OF THE p NICELY. 
  annotate("text", label = "Mean diff. = 12.9", x = 1, y = 94, size = 6, color = "black") +
  annotate("text", label = "t = 5.60", x = 1, y = 89, size = 6, color = "black") +
  annotate("text", label = expression(italic("p") * " < 0.001"), x = 1, y = 84, size = 6, color = "black") +
    geom_segment( #ADD AN ARROW CONNECTING THE ERROR-BAR SEGMENT AND THE STATISTICS
    x = 1.13,
    xend = 1.44,
    y = 82,
    yend = 54,
    arrow = arrow(length = unit(0.3, "cm")),
    color = "black"
  ) +
  theme(
    panel.background = element_rect(fill = "white"), #ELIMINATE GRAY BACKGROUND
    axis.line.y = element_line(color = "black", linewidth = 1.2), #INTENSIFY THE AXIS LINE
    axis.line.x = element_blank(), #ELIMINATE X AXIS LINE AND LABELS
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 20, 
                                face = "bold", 
                                color = "black"), #MAKE THE Y AXIS TITLE EASIER TO READ BY MAKING IT BIGGER AND BOLD. 
    axis.text.y = element_text(size = 18, color = "black"), #MAKE THE LABELS LARGER.
    axis.ticks.length.y = unit(0.3, "cm"), #ADD Y AXIS TICKS
    panel.grid.major.y = element_line(color = "gray80") #ADD SOME GRID LINES BUT  USE GRAYS TO DE-EMPHASIZE THEM.
  ) +
  theme( #A LITTLE RADICAL--THIS CODE MOVES THE Y AXIS TITLE ABOVE THE Y AXIS LINE AND ARRANGES IT HORIZONTALLY, EXPANDING THE TOP AND LEFT PLOT MARGINS TO MAKE SUFFICIENT ROOM.
  axis.title.y = element_text(angle = 0, #TURN HORIZONTAL.
                              vjust = 1.11, #MOVE WAY UP, ABOVE THE CURRENT AXIS.
                              hjust = 0, #MAKE LEFT-JUSTIFIED.
                              margin = margin(r = -160, unit = "pt")), #FORCE THE AXIS LABEL QUITE A WAYS TO THE RIGHT
  plot.margin = margin(2, 0, 0, 0.2, "cm") #EXPAND THE TOP AND LEFT PLOT MARGINS
  )
ggsave(filename = "boxplot.svg", height = 7.0, width = 7.0, dpi = "print") #SAVE IT PROGRAMMATICALLY TO INSPECT FOR PROPER SIZING.


# GRAPH 3 (SCATTERPLOT WITH TREND LINE) -----------------------------------------------------------------

library(ggrepel) #OPTIONAL--TO REPEL THE POINT LABELS APART TO PREVENT OVERPLOTTING.

set.seed(123) #CONTROL RANDOMNESS

#CREATE FAKE DATA
graph3.df = data.frame(
  community = c("Ru", "Pl", "HP", "Fr", "La", "HA", "Hl", "RAF", "Lu", "WH", "DH", "PAF", "Mc", "AFM", "Sp", "Ea", "NR"), #THE COMMUNITY CODES USED IN THE PAPER
  community_labels = c("RU", "PL", "HP", "FR", "LA", "HA", "HO", "RA", "LU", "WH", "DH", "PA", "MO", "AF", "SP", "ER", "NR"), #NEW, SHORTER, ALL CAPS CODES
  LDMC = sort(sample(181:363, 17, replace = T)) + sample(-15:15, 17, replace = T), #SIMILAR DATA WITH SOME RANDOM VARIATION
  Slope = sort(rnorm(17, 0.0035, 0.0005), decreasing = TRUE) + rnorm(17, 0, 0.0006)
)

#OUR PLOT
ggplot(data = graph3.df, #SET DATA GLOBALLY
       aes(x = LDMC, y = Slope)) + #SET THE X AND Y AESTHETICS GLOBALLY
  geom_smooth(method = "lm", #ADD LINEAR MODEL TREND LINE FIRST (SO LABELS PLOT OVER TOP OF IT FOR READABILITY). 
              se = TRUE, #ADD UNCERTAINTY BAND
              color = "black", #MAKE LINE SOLID BLACK
              linewidth = 1.75, #AND THICKER
              fill = "gray80") + #FILL THE UNCERTAINTY BAND LIGHTLY TO DE-EMPHASIZE IT
  geom_label_repel( #THIS IS FROM GGREPEL, BUT geom_label IN GGPLOT2 COULD BE SUBSTITUTED.
    aes(label = community_labels), #MAKE THE LABELS THE REVISED COMMUNITY CODES
            max.time = 5, #SETTING A HIGHER VALUE WILL PRODUCE MORE CONSISTENT RESULTS
            size = 5, #MAKE THE LABELS LARGE
            fontface = "bold", #BOLDED
            family = "Arial", #AND SANS SERIF
            color = "gray40", #COLOR THEM MEDIUM GRAY TO DE-EMPHASIZE THEM BUT KEEP THEM READABLE.
            force_pull = 300, #DO NOT LET THEM REPEL FROM THEIR SPOTS TOO MUCH
            label.r = unit(0.25, "cm"), #ROUND THE LABEL STROKES TO MAKE THEM OVAL-LIKE
            label.size = unit(0.75, "cm"), #WIDEN THE STROKE
            segment.color = "transparent") + #HIDE THE SEGMENTS PRODUCED WHEN POINTS ARE JITTERED FURTHER AWAY (VISUAL NOISE)
  annotate("text", x = 200, y = 0.002, #ADD AN R2 VALUE AS AN ANNOTATION
           label = expression("R^2 == 0.33"), 
           parse = TRUE, #NEEDED TO GET THE SUPERSCRIPT TO REPRESENT CORRECTLY
           size = 7) + #MAKE VERY READABLE SIZE
  annotate("text", x = 200, y = 0.0015,  #SAME AS ABOVE WITH A P VALUE
           label = expression(italic("p") * " = 0.016"), 
           parse = TRUE,
           size = 7) +
  scale_x_continuous("Leaf dry matter content\n(LDMC; mg dry mass / g fresh mass)", #MORE INFORMATIVE TITLE, PARSED ONTO TWO LINES
                     limits = c(180, 355), #EXPAND LIMITS
                     breaks = seq(from = 180, to = 355, by = 25)) + #ENSURE EQUIDISTANT BREAKS THAT ANCHOR THE ENTIRE AXIS AND HAVE ROUND-ISH FEELING STEP LENGTHS
  scale_y_continuous("Estimated slope (NDVI / year)",  #MORE INFORMATIVE TITLE
                     limits = c(0,0.005), #EXPAND AXIS LIMITS
                     breaks = seq(from = 0, to = 0.005, length.out = 6)) + #INCLUDE 0 AND ANCHOR BOTH ENDS OF THE AXIS WITH LABELS
  theme(
    panel.background = element_rect(fill = "white"), #ELIMINATE GRAY BACKGROUND
    axis.line = element_line(color = "black", linewidth = 1.2), #INTENSIFY THE AXIS LINES
    axis.title = element_text(size = 20, 
                                face = "bold", 
                              color = "black"), #MAKE THE AXIS TITLES EASIER TO READ BY MAKING THEM BIGGER AND BOLD. 
    axis.text = element_text(size = 18, color = "black"), #MAKE AXIS LABELS LARGER.
    axis.ticks.length = unit(0.3, "cm"), #MAKE AXIS TICKS AN APPROPRIATE SIZE.
    panel.grid.major = element_blank(), #ELIMINATE ALL GRID LINES
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 0.5, unit = "cm")), #INCREASE ROOM BETWEEN AXIS TITLES AND LABELS
    axis.title.x = element_text(margin = margin(t = 0.5, unit = "cm"))
  )
#SAVE OUR PLOT
ggsave("scatterplot.svg", height = 7.0, width = 9.5, dpi = "print")
