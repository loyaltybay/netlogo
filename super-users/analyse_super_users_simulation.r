library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

d = read_csv('super-users-simulation experiment-table.csv', skip = 6, col_types = 'nnnnnnnnn')


names(d) = str_replace_all(names(d), "-", ".")
names(d) = str_replace_all(names(d), "\\[|\\]", "")
names(d) = str_replace_all(names(d), "\\s+", ".")

print(names(d))

d$n.super.users = str_c(d$n.super.users, " super users")
d$average.node.degree = str_c("normal n links", d$average.node.degree)
d$super.user.node.degree = str_c("super user n links ", d$super.user.node.degree)
d$message.spread.chance = str_c("p = ", d$message.spread.chance)

std.error = function(x) sd(x, na.rm = T)/sqrt(length(x))

summ = d %>% group_by(n.super.users, average.node.degree, super.user.node.degree, message.spread.chance, step) %>% summarise(avg.percent.heard.message = mean(percent.heard.message), 
                                                      ymin = avg.percent.heard.message - 1.96 * std.error (percent.heard.message),
                                                      ymax = avg.percent.heard.message + 1.96 * std.error (percent.heard.message)) %>% identity()



g = ggplot(summ) + 
    geom_line(aes(x = step, y = avg.percent.heard.message, group = n.super.users, colour = factor(n.super.users)), alpha = 0.7) +
    # geom_linerange(aes(x = step, y = avg.percent.heard.message, ymin = ymin, ymax = ymax, group = n.super.users), alpha = 0.1) +
    facet_grid(super.user.node.degree ~ average.node.degree )+
    labs(title = str_c("Time vs message spread (", unique(summ$message.spread.chance)), ")", x = 'time')
    

ggsave(g, file = 'message-spreading.png')


