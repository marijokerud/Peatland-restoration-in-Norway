

artslinjerH <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "Hildremsvatnet", col_names = TRUE)
fungroupH <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "Funksjonelle_grupper", col_names = TRUE)


freqH <- artslinjerH %>% 
  left_join(fungroupH, by="Art")




arter.sjekk <- artslinjerH %>% 
  select(Art) %>% 
  distinct() %>% 
  left_join(fungroupH)
