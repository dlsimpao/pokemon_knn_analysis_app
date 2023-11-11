names(stat_info)

#Physical Sweeper
ps <- stat_info[[4]] + stat_info[[8]]
#Special Sweeper
ss <- stat_info[[6]] + stat_info[[8]]
#Wall
w <- stat_info[[3]] + stat_info[[5]] + stat_info[[7]]
#Physical Tank
pt <- stat_info[[4]] + stat_info[[5]]
#Special Tank
st <- stat_info[[6]] + stat_info[[7]] 


stat_role <- data.frame(name = stat_info[1],
                        `Physical Sweeper` = ps,
                        `Special Sweeper` = ss,
                         #Wall = w,
                         `Phyiscal Tank` = pt,
                         `Special Tank` = st)


set.seed(151)
role <- colnames(stat_role)[max.col(stat_role[2:5], ties.method = "first")+1]

stat_role %>% 
  cbind(role)

stat_role %>% filter(name == 'bulbasaur') %>% pull(role)
