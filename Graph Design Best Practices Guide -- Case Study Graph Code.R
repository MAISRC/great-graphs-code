library(tidyverse)
library(viridis)
library(svglite)

set.seed(123) #CONTROL RANDOMNESS


# GRAPH 1 (BAR GRAPH) -----------------------------------------------------------------


#CREATE FAKE DATA
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
      "Shedded coarse stems",
      "Sticks"
    ),
    each = 4
  ),
  species = rep(c("PRAW", "CSWA", "FISP", "CHSP"), times = 17), #OUR SPECIES
  prop.nest.mass = rbeta( #OUR RANDOM PROP. NEST MATERIAL DATA
    n = 4 * 17,
    shape1 = 1,
    shape2 = 20
  )
)

#OUR PLOT
ggplot(graph1.df, #PROVIDE OUR DATA SET GLOBALLY TO ALL ELEMENTS
       aes(y = materials, x = prop.nest.mass, fill = species)) + #MAP OUR X, Y, AND FILL COLOR AESTHETICALLY GLOBALLY TO APPLY TO ALL ELEMENTS. FLIP THE X AND Y AESTHETICS FOR EASE OF READABILITY.
  geom_bar(stat = "identity", #BARS REPRESENT MEAN NEST MASS PROPORTION
           position = position_dodge(width = 0.75), #THIS DODGES ONE SPECIES' BARS AWAY FROM THE NEXT SPECIES' BARS.
           width = 0.5) + #MAKE THE BARS THEMSELVES THINNER SO THERE IS WHITE SPACE BTW. NEIGHBORING BARS.
  scale_y_discrete("Material type", #USE LINE BREAKS (\n) TO BREAK ONTO MULTIPLE LINES 
                   limits = rev(levels(factor(graph1.df$materials)))) + #SORT THE AXIS ITEMS ALPHABETICALLY
  scale_x_continuous(
    "Percent of total nest mass", 
    expand = c(0, 0), #REMOVE THE GAP BETWEEN THE X AXIS LINE AND THE BOTTOMS OF THE BARS.
    limits = c(0, 0.33), #ELIMINATE AXIS TRUNCATION--WE HAVE TO GO OUT A LITTLE FURTHER THAN THE BREAKS TO PREVENT THE LABEL FROM BEING CUT OFF.
    breaks = c(0, 0.08, 0.16, 0.24, 0.32), #PROVIDE EVEN BREAKS THAT MAKE IT TO THE NEW LIMIT.
    labels = c(0, 8, 16, 24, 32)
  ) +
  scale_fill_discrete(
    "Species", #USE A LINE BREAK TO SEPARATE THE LEGEND TITLE FROM THE KEYS
    type = c(viridis(4, end = 0.9)), #SWITCH TO THE VIRIDIS COLORBLIND-FRIENDLY PALETTE, BUT AVOID THE YELLOW COLOR AT THE END THAT LACKS CONTRAST WITH A WHITE BACKGROUND.
    labels = c( #USE ACTUAL SPECIES COMMON NAMES, ELIMINATING THE NEED TO EYE-DART TO THE CAPTION.
      "Prairie\nwarbler",
      "Chestnut-sided\nwarbler",
      "Field\nsparrow",
      "Chipping\nsparrow"
    ),
    limits = rev(levels(factor(graph1.df$species))) #SORT THE LEGEND KEYS TO MATCH THE ORDER IN THE FIGURE.
  ) +
  theme(
    panel.background = element_rect(fill = "white"), #ELIMINATE GRAY BACKGROUND
    axis.line = element_line(color = "black", linewidth = 1.2), #INTENSIFY THE AXES LINES
    axis.title = element_text(size = 22, face = "bold", color = "black"), #MAKE THE AXES TITLES EASIER TO READ
    axis.text = element_text(size = 20, color = "black"), #ENLARGE AXIS LABELS.
    legend.text = element_text(size = 20, color = "black"), #SAME
    axis.ticks.length = unit(0.3, "cm"), #ENLARGE THE TICKS A LITTLE SO IT'S EASIER TO SEE THEM AND TO LINK MATERIAL TYPES BETTER WITH THEIR BARS.
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm")), #SPACE THE Y AXIS TITLE AWAY FROM ITS LABELS.
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    legend.title = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), size = 20, face = "bold", color = "black"), 
    legend.key.height = unit(0.25, "cm"), #EXPAND THE LEGEND KEYS TO ACCOMMODATE MULTI-LINE LABELS.
    legend.key.width = unit(1.25, "cm"),
    panel.grid.major.x = element_line(color = "gray60"), #ADD VERTICAL GRID LINES AT KEY POINTS BUT ZEBRA-STRIPE THEM AND USE GRAYS TO DE-EMPHASIZE THEM.
    panel.grid.minor.x = element_line(color = "gray80"),
    legend.key.spacing.y = unit(0.3, "cm"), #SPACE THE LEGEND KEYS APART FROM EACH OTHER
    legend.justification = "center", #CENTER ALIGN THE LEGEND
    legend.box = "horizontal", #PLACE IT HORIZONTAL AT THE TOP
    legend.position = "top",
    legend.margin = margin(0, 7, 0, 0, unit = "cm") #BUMP THE LEGEND TO THE LEFT BY INCREASING THE RIGHT MARGIN.
  )

ggsave(filename = "barplot.svg", height = 8.4, width = 9.8, dpi = "print") #SAVE IT PROGRAMMATICALLY AS A VECTOR-GRAPHICS FILE TYPE, THEN INSPECT AND ITERATE FOR PROPER SIZING.

# GRAPH 2 (BOXPLOT) -----------------------------------------------------------------


#CREATE FAKE DATA
graph2.df = data.frame(
  ecosystem = rep(c("Open", "Closed"), each = 60),
  perc.endemic.richness = c(rnorm(60, 46, 11), rnorm(60, 54, 13))
)

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
