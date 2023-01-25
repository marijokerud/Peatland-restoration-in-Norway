

artslinjerH <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "Hildremsvatnet", col_names = TRUE)
fungroupH <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "test", col_names = TRUE)

fungroupH <- fungroupH %>% 
  as_tibble()

freqH <- artslinjerH %>% 
  left_join(fungroupK, by="Art")




arter.sjekk <- artslinjerH %>% 
  select(Art) %>% 
  distinct() %>% 
  left_join(fungroupH)
