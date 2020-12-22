library(haven)
library(dplyr)
data <- read_sav("input/data.sav")

data <- data %>% mutate_all(as_factor)

media <- data %>% select(a1, age, a4, a5, a6, a7, a8, f6, h2, h3, j1.4, k1, k2a, k2b.1,
                         k2b.2, k2b.3, k2b.4, k2b.5, k2b.6, k2b.7, k2b.8, k2b.9, k2b.10, k2b.11, k2b.12, k2b.13, k2b.14, k2b.15,
                         k2b.16, k2b.17, k2b.18, k2b.19, k2b.20, k2b.96, k2b.97, k2b.95, k2b.99, k2c, p3, p4, p5, p7, t1, t1a,
                         t1.b, t1c, w1, w2, w3)

media_2 <- media[which(media$k2a != 0 & !is.na(media$k2a)),]

# replace the media with categories. 1 for Blue -1 for Green and 0 for neutral

# Taiwan Television
media_2$k2b.1 <- as.character(media_2$k2b.1)
media_2$k2b.1[is.na(media_2$k2b.1)] <- "0"
media_2$k2b.1[media_2$k2b.1 == "Taiwan Television(TTV)"] <- "1"

# China Television (CTV)
media_2$k2b.2 <- as.character(media_2$k2b.2)
media_2$k2b.2[is.na(media_2$k2b.2)] <- "0"
media_2$k2b.2[media_2$k2b.2 == "China Television (CTV)"] <- "1"

# Chinese Television System (CTS)
media_2$k2b.3 <- as.character(media_2$k2b.3)
media_2$k2b.3[is.na(media_2$k2b.3)] <- "0"
media_2$k2b.3[media_2$k2b.3 == "Chinese Television System (CTS)"] <- "1"

# Formosa TV (FTV)
media_2$k2b.4 <- as.character(media_2$k2b.4)
media_2$k2b.4[is.na(media_2$k2b.4)] <- "0"
media_2$k2b.4[media_2$k2b.4 == "Formosa TV (FTV)"] <- "-1"

# TVBS
media_2$k2b.5 <- as.character(media_2$k2b.5)
media_2$k2b.5[is.na(media_2$k2b.5)] <- "0"
media_2$k2b.5[media_2$k2b.5 == "TVBS"] <- "1"

# Sanlih E-Television (SET)
media_2$k2b.6 <- as.character(media_2$k2b.6)
media_2$k2b.6[is.na(media_2$k2b.6)] <-"0"
media_2$k2b.6[media_2$k2b.6 == "Sanlih E-Television (SET)"] <- "-1"

# Eastern Broadcasting Company News (EBC News)
media_2$k2b.7 <- as.character(media_2$k2b.7)
media_2$k2b.7[is.na(media_2$k2b.7)] <- "0"
media_2$k2b.7[media_2$k2b.7 == "Eastern Broadcasting Company News (EBC News)"] <- "-1"

# Chung Tien Television (CTi TV)
media_2$k2b.8 <- as.character(media_2$k2b.8)
media_2$k2b.8[is.na(media_2$k2b.8)] <- "0"
media_2$k2b.8[media_2$k2b.8 == "Chung Tien Television (CTi TV)"] <- "1"

# Era Television
media_2$k2b.9 <- as.character(media_2$k2b.9)
media_2$k2b.9[is.na(media_2$k2b.9)] <- "0"
media_2$k2b.9[media_2$k2b.9 == "Era Television"] <- "-1"

# Gala Television (GTV)
media_2$k2b.10 <- as.character(media_2$k2b.10)
media_2$k2b.10[is.na(media_2$k2b.10)] <- "0"
media_2$k2b.10[media_2$k2b.10 == "Gala Television (GTV)"] <- "1"

# Unique Satellite TV (USTV)
media_2$k2b.11 <- as.character(media_2$k2b.11)
media_2$k2b.11[is.na(media_2$k2b.11)] <-"0"
media_2$k2b.11[media_2$k2b.11 == "Unique Satellite TV (USTV)"] <- "1"

# DaAi TV 
media_2$k2b.12 <- as.character(media_2$k2b.12)
media_2$k2b.12[is.na(media_2$k2b.12)] <- "0"
media_2$k2b.12[media_2$k2b.12 == "DaAi TV"] <- "0"

# Public Television Service (PTS)
media_2$k2b.13 <- as.character(media_2$k2b.13)
media_2$k2b.13[is.na(media_2$k2b.13)] <- "0"
media_2$k2b.13[media_2$k2b.13 == "Public Television Service (PTS)"] <- "0"

# Hakka Television Station (Hakka TV) 
media_2$k2b.14 <- as.character(media_2$k2b.14)
media_2$k2b.14[is.na(media_2$k2b.14)] <-"0"
media_2$k2b.14[media_2$k2b.14 == "Hakka Television Station (Hakka TV)" ] <- "0"

# Taiwan Indigenous Television (TITV)
media_2$k2b.15 <- as.character(media_2$k2b.15)
media_2$k2b.15[is.na(media_2$k2b.15)] <- "0"
media_2$k2b.15[media_2$k2b.15 == "Taiwan Indigenous Television (TITV)"] <- "1"

# Chinese Satellite Television (CSTV) 
media_2$k2b.16 <- as.character(media_2$k2b.16)
media_2$k2b.16[is.na(media_2$k2b.16)] <- "0"
media_2$k2b.16[media_2$k2b.16 == "Chinese Satellite Television (CSTV)"] <- "0"
# Chinese Satellite Television
media_2$k2b.17 <- as.character(media_2$k2b.17)
media_2$k2b.17[is.na(media_2$k2b.17)] <- "0"
media_2$k2b.17[media_2$k2b.17 == "Chinese Satellite Television"] <- "0"
# Beautiful Life Television (BLTV)
media_2$k2b.18 <- as.character(media_2$k2b.18)
media_2$k2b.18[is.na(media_2$k2b.18)] <- "0"
media_2$k2b.18[media_2$k2b.18 == "Beautiful Life Television (BLTV)"] <- "0"
# Next TV
media_2$k2b.19 <- as.character(media_2$k2b.19)
media_2$k2b.19[is.na(media_2$k2b.19)] <- "0"
media_2$k2b.19[media_2$k2b.19 == "Next TV"] <- "-1"
# All of the above
media_2$k2b.20 <- as.character(media_2$k2b.20)
media_2$k2b.20[is.na(media_2$k2b.20)] <- "0"
media_2$k2b.20[media_2$k2b.20 == "All of the above"] <- "0"
# 

sum <- as.data.frame(as.numeric(media_2$k2b.1) + as.numeric(media_2$k2b.2) + as.numeric(media_2$k2b.3) + 
  as.numeric(media_2$k2b.4) + as.numeric(media_2$k2b.5) + as.numeric(media_2$k2b.6) + as.numeric(media_2$k2b.7) + 
  as.numeric(media_2$k2b.8) + as.numeric(media_2$k2b.9) + as.numeric(media_2$k2b.10) + as.numeric(media_2$k2b.11) + 
  as.numeric(media_2$k2b.12) + as.numeric(media_2$k2b.13) + as.numeric(media_2$k2b.14) + as.numeric(media_2$k2b.15) + 
  as.numeric(media_2$k2b.16) + as.numeric(media_2$k2b.17) + as.numeric(media_2$k2b.18) + as.numeric(media_2$k2b.19) + as.numeric(media_2$k2b.20))

colnames(sum) <- "sum"

media_2 <- cbind(media_2, sum)
media_2 <- media_2 %>%
  mutate(media_lean = case_when(sum > 0 ~ 'Blue', 
                                sum < 0 ~ 'Green',
                                sum == 0 ~ 'Neutral'))

# combine "want to vote" with "leaning"
media_2$p4 <- as.character(media_2$p4)
media_2$p5 <- as.character(media_2$p5)
media_2$p4[media_2$p4 == "You would cast a null ballot"] <- NA
media_2$p4[media_2$p4 == "Don’t know"] <- NA
media_2$p4 <- ifelse(is.na(media_2$p4), media_2$p5, media_2$p4)

# parties people support
## delete refuse to answer
media_2$t1.b <- as.character(media_2$t1.b)
media_2$t1.b[media_2$t1.b == "don’t know"] <- "Neutral"
media_2$t1.b[media_2$t1.b == "NP"] <- "Neutral"
media_2$t1.b[media_2$t1.b == "Other party"] <- "Neutral"
media_2$t1.b[media_2$t1.b == "PFP"] <- "Neutral"
media_2$t1.b[media_2$t1.b == "TSU"] <- "Neutral"
media_2$t1.b[is.na(media_2$t1.b)] <- "Neutral"

media_3 <- media_2 %>% select(a1, age, a4, a5, a6, a7, a8, f6, h2, h3, j1.4, k1, media_lean, k2c, p4, t1.b, w1, w2, w3)

names <- c("Gender", "Age", "Residence", "Background_father", "Background_mother", "Marital_Status", "Education", "Frequency",
           "News_Source", "News_Trust", "Recieve_info", "2016_attention", "Media_Lean_Obj", "Media_Lean_Sub", "Candidates", "Party", 
           "Employment", "Occupation", "Income")
colnames(media_3) <- names

green_party <- media_3[which(media_3$Party=='DPP'),]
blue_party <- media_3[which(media_3$Party =='KMT'),]
neutral_party <- media_3[which(media_3$Party =='Neutral'),]

# write into csv
write.csv(green_party, "input/green_party.csv")
write.csv(blue_party, "input/blue_party.csv")
write.csv(neutral_party, "input/neutral.csv")
