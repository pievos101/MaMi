# Load libraries
library(ggplot2)
library(ggrepel)

# --- 1. Compact, visible clusters
set.seed(123)

# Class 1 cluster (left but not too far)
class1 <- data.frame(
  x = rnorm(8, 4.2, 0.2),
  y = rnorm(8, 4.2, 0.2),
  class = factor(1)
)

# Class 2 cluster (centered)
class2 <- data.frame(
  x = rnorm(10, 5, 0.2),
  y = rnorm(10, 5, 0.2),
  class = factor(2)
)

# Noisy class 1 point very close to query
noisy_point <- data.frame(x = 5.01, y = 5.01, class = factor(1))

# Combine dataset
data <- rbind(class1, class2, noisy_point)

# Define query point
query_point <- data.frame(x = 5, y = 5)

# --- 2. Compute distances & get k-NNs
k <- 3
data$distance <- sqrt((data$x - query_point$x)^2 + (data$y - query_point$y)^2)
nearest_neighbors <- data[order(data$distance), ][1:k, ]
radius <- max(nearest_neighbors$distance)

# --- 3. Predictions
nearest_neighbors$weight <- 1 / (nearest_neighbors$distance + 1e-5)
vote_weights <- aggregate(weight ~ class, data = nearest_neighbors, sum)
pred_weighted <- vote_weights$class[which.max(vote_weights$weight)]
pred_non_weighted <- names(sort(table(nearest_neighbors$class), decreasing = TRUE))[1]

# --- 4. Plot
ggplot() +
  # All data points
  geom_point(data = data, aes(x = x, y = y, color = class), size = 3, alpha = 0.6) +

  # k-nearest neighbors - light blue highlight
  geom_point(data = nearest_neighbors, aes(x = x, y = y), shape = 21, size = 6,
             stroke = 1, color = "deepskyblue", fill = "lightblue") +

  # Noisy point - light red
  geom_point(data = noisy_point, aes(x = x, y = y), shape = 8, size = 6,
             color = "firebrick", fill = "mistyrose") +
  geom_text_repel(data = noisy_point, aes(x = x, y = y, label = "Noisy class 1 point"),
                  color = "firebrick", size = 4, nudge_y = 0.25) +

  # Query point - black triangle
  geom_point(data = query_point, aes(x = x, y = y), shape = 17, color = "black", size = 6) +
  geom_text_repel(data = query_point, aes(x = x, y = y, label = "Query Point"),
                  size = 4, nudge_y = -0.3) +

  # Dashed k-NN boundary circle
  annotate("path",
           x = query_point$x + radius * cos(seq(0, 2 * pi, length.out = 100)),
           y = query_point$y + radius * sin(seq(0, 2 * pi, length.out = 100)),
           color = "black", linetype = "dashed") +

  # Dotted lines from query to neighbors
  geom_segment(data = nearest_neighbors,
               aes(x = query_point$x, y = query_point$y, xend = x, yend = y),
               color = "gray30", linetype = "dotted") +

  # Colors and theme
  scale_color_manual(values = c("firebrick3", "darkgreen")) +
  labs(
    title = "kNN Classification: Weighted vs Non-weighted with Noisy Neighbor",
    subtitle = paste("k =", k, "| Non-weighted:", pred_non_weighted,
                     "✔️ | Weighted:", pred_weighted, "❌"),
    x = "x", y = "y", color = "Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")