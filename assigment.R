
# Load packages -----------------------------------------------------------

library(tidyverse)

# Read data ---------------------------------------------------------------

iris
iris <- as_tibble(iris)
iris

# Question 1 --------------------------------------------------------------

new_iris <- rename(iris, Sepal_Length = Sepal.Length, Sepal_Width = Sepal.Width, Petal_Length = Petal.Length, Petal_Width = Petal.Width)
new_iris

# Question 2 --------------------------------------------------------------

iris_cm <- mutate(new_iris, Sepal_Length = Sepal_Length * 10, Sepal_Width = Sepal_Width * 10, Petal_Length = Petal_Length * 10, Petal_Width = Petal_Width * 10)
iris_cm

# Question 3 --------------------------------------------------------------

iris_area <- mutate(iris_cm, Sepal_Area = Sepal_Length * Sepal_Width, Petal_Area = Petal_Length * Petal_Width)
iris_onlyarea <- select(iris_area, Sepal_Area, Petal_Area, Species)
iris_onlyarea

# Question 4 --------------------------------------------------------------

summarize(
  iris_cm,
  sampl_size = n(),
  max = max(Sepal_Length),
  min = min(Sepal_Length),
  range = max - min,
  med = median(Sepal_Length),
  q1 = quantile(Sepal_Length, probs = 0.25),
  q3 = quantile(Sepal_Length, probs = 0.75),
  iqr = IQR(Sepal_Length)
)

# Question 5 --------------------------------------------------------------

iris_sum <-
  iris_cm %>%
  group_by(Species) %>%
  summarize(
    sampl_size = n(),
    mean_w = mean(Petal_Width),
    str_dev = sd(Petal_Width),
    var = var(Petal_Width),
    sem = mean(Petal_Width) / sqrt(n()),
    ci_upper = mean_w + 2 * sem,
    ci_lower = mean_w - 2 * sem
  )  

iris_sum
iris_cm
# Question 6 --------------------------------------------------------------

ggplot(data = iris_cm) +
  geom_jitter(mapping = aes(x = Species, y = Petal_Width))

# Question 7 --------------------------------------------------------------

ggplot(data = iris_cm) +
  geom_jitter(mapping = aes(x = Species, y = Petal_Width)) +
  geom_crossbar(
    data = iris_sum,
    mapping = aes(x = Species, y = mean_w, ymax = ci_upper, ymin = ci_lower),
    color = "blue"
  )

# Question 8 --------------------------------------------------------------

ggplot(data = iris_cm) +
  geom_point(mapping = aes(x = Petal_Length, y = Petal_Width, color = Species))
