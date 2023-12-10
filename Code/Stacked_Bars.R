th <- 
  
  theme_light() +
  
  theme(plot.title = element_text(hjust = 0.5, size = 7.5),
            legend.key.size = unit(0.25, "cm"),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.position = "bottom")

# Urban vs Test Scores

P1 <- ggplot(data = Data, 
             aes(x = Urban, 
                 fill = TestRange)) +
  geom_bar(position = "fill") +
  ylab("") +
  scale_x_discrete(labels = c(
    "Rural", 
    "Urban"), 
    breaks = c("0", "1"), 
    name = NULL) +
  scale_fill_manual(values = c("0" = "orange3", 
                               "1" = "gold3", 
                               "2" = "yellow3"),
                    labels = c("0" = "< 40%", 
                               "1" = "< 80%", 
                               "2" = "80% +"),
                    name = "Test Score") +
  labs(title = "Urban Status") +
  th

###

P2 <- ggplot(data = Data, 
             aes(x = InternetCat, 
                 fill = TestRange)) +
  geom_bar(position = "fill") +
  ylab("") +
  scale_x_discrete(labels = c(
    "No Internet", 
    "Internet"), 
    breaks = c("0", "1"), 
    name = NULL) +
  scale_fill_manual(values = c("0" = "orange3", 
                               "1" = "gold3", 
                               "2" = "yellow3"),
                    labels = c("0" = "< 40%", 
                               "1" = "< 80%", 
                               "2" = "80% +"),
                    name = "Test Score") +
  labs(title = "Internet Status") +
  th
###

P3 <- ggplot(data = Data, 
             aes(x = Ma, 
                 fill = TestRange)) +
  geom_bar(position = "fill") +
  ylab("") +
  scale_x_discrete(labels = c(
    "No School", 
    "Primary",
    "Middle",
    "High",
    "Tertiary"), 
    breaks = c("0", "1", "2", "3", "4"), 
    name = NULL) +
  scale_fill_manual(values = c("0" = "orange3", 
                               "1" = "gold3", 
                               "2" = "yellow3"),
                    labels = c("0" = "< 40%", 
                               "1" = "< 80%", 
                               "2" = "80% +"),
                    name = "Test Score") +
  labs(title = "Mother Education") +
  th

###

P4 <- ggplot(data = Data, 
             aes(x = Trav, 
                 fill = TestRange)) +
  geom_bar(position = "fill") +
  ylab("") +
  scale_x_discrete(labels = c(
    "< 1hr", 
    "< 2hrs",
    "< 3hrs",
    "3hrs +"), 
    breaks = c("1", "2", "3", "4"), 
    name = NULL) +
  scale_fill_manual(values = c("0" = "orange3", 
                               "1" = "gold3", 
                               "2" = "yellow3"),
                    labels = c("0" = "< 40%", 
                               "1" = "< 80%", 
                               "2" = "80% +"),
                    name = "Test Score") +
  labs(title = "Traveltime") +
  th 

### 

P5 <- ggplot(data = Data, 
             aes(x = Gen, 
                 fill = TestRange)) +
  geom_bar(position = "fill") +
  ylab("") +
  scale_x_discrete(labels = c(
    "Male", 
    "Female"), 
    breaks = c("0", "1"), 
    name = NULL) +
  scale_fill_manual(values = c("0" = "orange3", 
                               "1" = "gold3", 
                               "2" = "yellow3"),
                    labels = c("0" = "< 40%", 
                               "1" = "< 80%", 
                               "2" = "80% +"),
                    name = "Test Score") +
  labs(title = "Gender") +
  th 

###

P6 <- ggplot(data = Data, 
             aes(x = Da, 
                 fill = TestRange)) +
  geom_bar(position = "fill") +
  ylab("") +
  scale_x_discrete(labels = c(
    "No School", 
    "Primary",
    "Middle",
    "High",
    "Tertiary"), 
    breaks = c("0", "1", "2", "3", "4"), 
    name = NULL) +
  scale_fill_manual(values = c("0" = "orange3", 
                               "1" = "gold3", 
                               "2" = "yellow3"),
                    labels = c("0" = "< 40%", 
                               "1" = "< 80%", 
                               "2" = "80% +"),
                    name = "Test Score") +
  labs(title = "Father Education") +
  th