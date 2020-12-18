# install.packages("palmerpenguins")
library(palmerpenguins) #two datasets abt penguins
library(tidyverse) #tidyverse libraries
library(visdat)
library(dplyr)
library(ggplot2)
library(recipes)
library(tidytext)
library(corrr)
theme_set(theme_minimal())

data(package = 'palmerpenguins')
head(penguins)
head(penguins_raw)


penguins %>%
  count(species)

penguins %>%
  group_by(species) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))


citation("palmerpenguins")

visdat::vis_dat(penguins)


penguins %>%
  dplyr::select(where(is.factor)) %>%
  glimpse()


# Count penguins for each species / island
penguins %>%
  count(species, island, .drop = FALSE)



ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"),
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()


# Count penguins for each species / sex
penguins %>%
  count(species, sex, .drop = FALSE)

ggplot(penguins, aes(x = sex, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"),
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  glimpse()

# Scatterplot example 1: penguin flipper length versus body mass
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 2) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

# Scatterplot example 2: penguin bill length versus bill depth
ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 2)  +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

ggplot(penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange","cyan4"),
                     na.translate = FALSE) +
  facet_wrap(~species)

penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species)) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))

# Jitter plot example: bill length by species
ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_jitter(aes(color = species),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))


# Histogram example: flipper length by species
ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))



mass_flipper <- ggplot(data = penguins,
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

mass_flipper

flipper_bill <- ggplot(data = penguins,
                       aes(x = flipper_length_mm,
                           y = bill_length_mm)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Flipper and bill length",
       subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)",
       y = "Bill length (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

flipper_bill


bill_len_dep <- ggplot(data = penguins,
                       aes(x = bill_length_mm,
                           y = bill_depth_mm,
                           group = species)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 3,
             alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = species)) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin bill dimensions",
       subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

bill_len_dep


bill_no_species <- ggplot(data = penguins,
                          aes(x = bill_length_mm,
                              y = bill_depth_mm)) +
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin bill dimensions (omit species)",
       subtitle = "Palmer Station LTER",
       x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  theme(plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  geom_smooth(method = "lm", se = FALSE, color = "gray50")

bill_no_species

flipper_hist <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species),
                 alpha = 0.5,
                 position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(x = "Flipper length (mm)",
       y = "Frequency",
       title = "Penguin flipper lengths")

flipper_hist

flipper_box <- ggplot(data = penguins, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(x = "Species",
       y = "Flipper length (mm)")

flipper_box

mass_hist <- ggplot(data = penguins, aes(x = body_mass_g)) +
  geom_histogram(aes(fill = species),
                 alpha = 0.5,
                 position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(x = "Body mass (g)",
       y = "Frequency",
       title = "Penguin body mass")

mass_hist

ggplot(penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = sex)) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
  labs(title = "Penguin flipper and body mass",
       subtitle = "Dimensions for male and female Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin sex") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  facet_wrap(~species)


penguins_corr <- penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  correlate() %>%
  rearrange()
penguins_corr

enguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species),
                  columns = c("flipper_length_mm", "body_mass_g",
                              "bill_length_mm", "bill_depth_mm")) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))


penguin_recipe <-
  recipe(~., data = penguins) %>%
  update_role(species, island, sex, year, new_role = "id") %>%
  step_naomit(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()

penguin_pca <-
  penguin_recipe %>%
  tidy(id = "pca")

penguin_pca


penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  tidyr::drop_na() %>%
  scale() %>%
  prcomp() %>%
  .$rotation


penguin_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance") %>%
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "#b6dfe2") +
  xlim(c(0, 5)) +
  ylab("% of total variance")


penguin_pca %>%
  mutate(terms = tidytext::reorder_within(terms,
                                          abs(value),
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )

# get pca loadings into wider format
pca_wider <- penguin_pca %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)


# define arrow style
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")


pca_plot <-
  juice(penguin_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = species, shape = species),
             alpha = 0.8,
             size = 2) +
  scale_colour_manual(values = c("darkorange","purple","cyan4"))

pca_plot +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_text(data = pca_wider,
            aes(x = PC1, y = PC2, label = terms),
            hjust = 0,
            vjust = 1,
            size = 5,
            color = '#0A537D')



penguins %>%
  group_by(species) %>%
  summarize(across(c(flipper_length_mm, body_mass_g),
                   mean,
                   na.rm = TRUE))

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  scale_colour_manual(values = c("darkorange","purple","cyan4"))


pca_plot %+%
  aes(PC2, PC3) +
  geom_segment(data = pca_wider,
               aes(xend = PC2, yend = PC3),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_text(data = pca_wider,
            aes(x = PC2, y = PC3, label = terms),
            hjust = 0,
            vjust = 1,
            size = 5,
            color = '#0A537D')


penguins %>%
  group_by(species) %>%
  summarize(across(c(bill_depth_mm, bill_length_mm),
                   mean,
                   na.rm = TRUE))

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, colour = species)) +
  geom_point() +
  scale_colour_manual(values = c("darkorange","purple","cyan4"))


# https://allisonhorst.github.io/palmerpenguins/articles/user_contributions.html





























































