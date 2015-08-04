library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

d = read_csv('super-users-simulation experiment-table.csv', skip = 6, col_types = 'iiniiiin')
names(d) = str_replace_all(names(d), "-", ".")
names(d) = str_replace_all(names(d), "\\[|\\]", "")
names(d) = str_replace_all(names(d), "\\s+", ".")

print(names(d))
g = ggplot(d) + 
    geom_line(aes(x = step, y = percent.infected, group = run.number, colour = percent.infected)) + 
    ylim(0, 100) +
    facet_wrap(~n.super.users)
ggsave(g, file = 'speed-of-infection.png')





