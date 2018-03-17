###
# Code: Warehouse mangement- Teacher's Desk
# Author: Sarathkumar Nachiappan Nallusamy
# Date: April 2017
###

library(dplyr)

set.seed(432)

#Read Product and Warehouse datasets
parent.address <- 'paste_path_here'

warehouse.address <- paste0(parent.address,'warehouse.csv')
product.address <- paste0(parent.address, 'products.csv')

warehouse <- tbl_df(read.csv(warehouse.address, stringsAsFactors = F))
product <- tbl_df(read.csv(product.address, stringsAsFactors = F))

warehouse
product

#cleaning warehouse
names(warehouse) <- tolower(names(warehouse))
warehouse <- warehouse %>% 
  mutate(id = gsub('[[:space:]]', x = id, replacement = ''),
         level.off.first = ifelse(level.off.first == 'First', 1, 
                                  ifelse(level.off.first == 'Second', 2, 3)),
         name = paste(aisle.no, bay.no, level.off.first, sep = ''))

warehouse

#Clean product
names(product) <- tolower(names(product))

product

#random tables
warehouse <- warehouse %>%
  mutate(active = sample(0:1, nrow(warehouse), replace = T),
         capacity = ifelse(is.na(capacity), 2, capacity))


product <- product %>%
  mutate(name = substr(name, 1, 8),
         quantity = sample(1:6, nrow(product), replace = T),
         weight = sample(1:3, nrow(product), replace = T),
         refilling.freq = sample(1:4, nrow(product), replace = T),
         active = sample(0:1, nrow(product), replace = T))
product


#Check if location allocation is possible
available.cap <- warehouse %>%
  filter(active == 1) %>%
  summarize(available.capacity = sum(capacity))
available.cap

prod.active.quant <- product %>%
  filter(active == 1) %>%
  summarize(product.active.quant = sum(quantity))
prod.active.quant

if(available.cap[[1]] < prod.active.quant[[1]]){
  print('There is NOT enough capacity')
}else{
  print('There is enough capacity')
}

#Product Score Calculation
#Weight: 1 to 3, 3 is the heaviest
#refilling: 1 to 4, 4 is the most frequent
#Score = 2 * weight + refilling -> Range: [2,10]
product <- product %>%
  mutate(score = 2*weight + refilling.freq)

#Algorithm
warehouse.active <- warehouse %>%
  filter(active == 1) %>%
  arrange(desc(active), score, aisle.no, name) %>%
  mutate(cap.cumsum = cumsum(capacity))
warehouse.active

product.active <- product %>%
  filter(active == 1) %>%
  arrange(desc(score), desc(quantity))
product.active

w.p <- tbl_df(data.frame(id =1, temp = 1, p.id = 1))
w.p

for(i in 1:nrow(product.active)){
  p <- product.active[1,]
  p
  q <- p$quantity
  q
  p.id <- p$id
  p.id
  w.temp <- warehouse.active %>%
    mutate(temp = cap.cumsum - q) %>%
    filter(temp <= 0) %>%
    select(id, temp)
  w.temp
  if(nrow(w.temp) == 0){
    w.temp <- (warehouse.active %>%
      mutate(temp = cap.cumsum - q) %>%
      select(id, temp))[1,]
  }
  w.temp
  
  if(max(w.temp$temp) < 0){
    w.temp <- rbind(w.temp,
                    (warehouse.active %>%
                       mutate(temp = cap.cumsum - q) %>%
                       filter(temp > 0))[1,] %>%
                      select(id,temp))
  }
  w.temp
  w.temp2 <- w.temp %>%
    mutate(p.id = p.id)
  w.temp2
  
  w.p <- rbind(w.p, w.temp2)
  w.p
  
  warehouse.active <- warehouse.active %>%
    mutate(active = ifelse((id %in% w.temp2$id), 0, active)) %>%
    arrange(desc(active), score, aisle.no, name) %>%
    mutate(cap.cumsum = cumsum(capacity))
  warehouse.active
  
  product.active <- product.active %>%
    mutate(active = ifelse(id == p.id, 0, active)) %>%
    filter(active == 1) %>%
    arrange(desc(score), desc(quantity))
  product.active
}

w.p
w.p <- w.p[-1,-2]
w.p

final <- w.p %>%
  inner_join(product, by = c('p.id' = 'id')) %>%
  inner_join(warehouse.active, by = c('id' = 'id')) %>%
  select(id, p.id, name.x, quantity,weight, refilling.freq,
         score.x, capacity, score.y)
final
print(final, n =30)
