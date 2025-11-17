source("themes.R")
source("data.R")

library(tidyverse)

k3 <- read.csv("Data/dat-3.csv")

all_data <- bind_cols(k3, collins)

mean_mat <- all_data %>%
  group_by(label) %>%
  summarize(across(
    c(
      "FirstPerson",
      "InnerThinking",
      "ThinkPositive",
      "ThinkNegative",
      "ThinkAhead",
      "ThinkBack",
      "Reasoning",
      "Share_SocTies",
      "Direct_Activity",
      "Interacting",
      "Notifying",
      "LinearGuidance",
      "WordPicture",
      "SpaceInterval",
      "Motion",
      "PastEvents",
      "TimeInterval",
      "ShiftingEvents",
      "Text_Coverage"
    ),
    mean
  )) %>%
  column_to_rownames("label") %>%
  as.matrix()

tbl <- table(all_data$Genre, all_data$label)
tbl_prop <- prop.table(tbl, margin = 2)
df_heat_prop <- as.data.frame(tbl_prop)
colnames(df_heat_prop) <- c("genre", "cluster", "proportion")
df_heat_prop <- df_heat_prop %>%
  mutate(
    genre_names = case_when(
      genre == 1 ~ "Press: Reporting",
      genre == 2 ~ "Press: Editorial",
      genre == 3 ~ "Press: Reviews",
      genre == 4 ~ "Religion",
      genre == 5 ~ "Skills and Hobbies",
      genre == 6 ~ "Popular Lore",
      genre == 7 ~ "Biography, Memoirs, Belles Lettres, etc.",
      genre == 8 ~ "Official Communications",
      genre == 9 ~ "Learned (scholarly journals)",
      genre == 10 ~ "General Fiction",
      genre == 11 ~ "Mystery and Detective Fiction",
      genre == 12 ~ "Science Fiction",
      genre == 13 ~ "Adventure and Western Fiction",
      genre == 14 ~ "Romance and Love Story",
      genre == 15 ~ "Humor"
    ),
    genre_names = factor(
      genre_names,
      levels = c(
        "Press: Reporting",
        "Press: Editorial",
        "Press: Reviews",
        "Religion",
        "Skills and Hobbies",
        "Popular Lore",
        "Biography, Memoirs, Belles Lettres, etc.",
        "Official Communications",
        "Learned (scholarly journals)",
        "General Fiction",
        "Mystery and Detective Fiction",
        "Science Fiction",
        "Adventure and Western Fiction",
        "Romance and Love Story",
        "Humor"
      )
    ),
    category = case_when(
      genre %in% c(1, 2, 3) ~ "Press",
      genre %in% c(4, 5, 6) ~ "Non-press Nonfiction",
      genre == 7 ~ "Biography",
      genre %in% c(8, 9) ~ "Scholarship and Official Documents",
      genre %in% c(10, 11, 12, 13, 14, 15) ~ "Fiction"
    ),
    category = factor(
      category,
      levels = c(
        "Press",
        "Non-press Nonfiction",
        "Biography",
        "Scholarship and Official Documents",
        "Fiction"
      )
    )
  )


# Heatmap by genre
p1 <- ggplot(
  df_heat_prop,
  aes(x = cluster, y = genre_names, fill = proportion)
) +
  geom_tile() +
  geom_text(aes(label = round(proportion, 2)), color = "black", size = 16) + # larger numbers on tiles
  scale_fill_gradient2_paper() +
  theme_paper(base_size = 64, text_size = 48) +
  labs(x = "Cluster", y = "Genre", fill = "Proportion") +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

# # Summarize over categories
# df_heat_prop_cat <- df_heat_prop %>%
#   group_by(category, cluster) %>%
#   summarize(proportion = sum(proportion)) %>%
#   ungroup()

# # Heatmap by category
# p2 <- ggplot(
#   df_heat_prop_cat,
#   aes(x = cluster, y = category, fill = proportion)
# ) +
#   geom_tile() +
#   geom_text(aes(label = round(proportion, 2)), color = "black", size = 3.5) + # larger numbers on tiles
#   scale_fill_gradient(low = "white", high = "red") +
#   theme_minimal(base_size = 14) +
#   labs(x = "Cluster", y = "Category", fill = "Proportion")

k5 <- read.csv("Data/dat-5.csv")
all_data2 <- bind_cols(k5, collins)

all_data2$category <- case_when(
  all_data2$Genre %in% c(1, 2, 3) ~ "Press",
  all_data2$Genre %in% c(4, 5, 6) ~ "Non-press Nonfiction",
  all_data2$Genre == 7 ~ "Biography",
  all_data2$Genre %in% c(8, 9) ~ "Scholarship and Official Documents",
  all_data2$Genre %in% c(10, 11, 12, 13, 14, 15) ~ "Fiction"
)

tbl <- table(all_data2$category, all_data2$label)
tbl_prop <- prop.table(tbl, margin = 2)
df_heat_prop <- as.data.frame(tbl_prop)
colnames(df_heat_prop) <- c("category", "cluster", "proportion")

df_heat_prop_cat <- df_heat_prop %>%
  group_by(category, cluster) %>%
  summarize(proportion = sum(proportion)) %>%
  ungroup()

# Heatmap by category
p3 <- ggplot(
  df_heat_prop_cat,
  aes(x = cluster, y = category, fill = proportion)
) +
  geom_tile() +
  geom_text(aes(label = round(proportion, 2)), color = "black", size = 16) +
  scale_fill_gradient2_paper() +
  theme_paper(base_size = 64, text_size = 48) +
  labs(x = "Cluster", y = "Category", fill = "Proportion") +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

# library(gridExtra)
# cluster_heatmaps <- grid.arrange(p1, p2, ncol = 2)
# ggsave(
#   "Figures/cluster_genre_heatmaps.png",
#   cluster_heatmaps,
#   width = 12,
#   height = 5
# )

# var_means <- all_data %>%
#   select(
#     label,
#     FirstPerson,
#     InnerThinking,
#     ThinkPositive,
#     ThinkNegative,
#     ThinkAhead,
#     ThinkBack,
#     Reasoning,
#     Share_SocTies,
#     Direct_Activity,
#     Interacting,
#     Notifying,
#     LinearGuidance,
#     WordPicture,
#     SpaceInterval,
#     Motion,
#     PastEvents,
#     TimeInterval,
#     ShiftingEvents,
#     Text_Coverage
#   ) %>%
#   group_by(label) %>%
#   summarize(across(everything(), mean)) %>%
#   pivot_longer(-label, names_to = "feature", values_to = "mean_value")

# var_means_plot <- ggplot(
#   var_means,
#   aes(x = label, y = mean_value, fill = label)
# ) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~feature, scales = "free_y") +
#   labs(
#     title = "Mean Feature Values by Cluster",
#     x = "Cluster",
#     y = "Mean Value"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")
