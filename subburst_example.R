library(sunburstR)

a = salarios %>% filter(subsidio == max(subsidio)) %>% select(indenizacoes, direitos_pessoais, direitos_eventuais)
group = colnames(a)
a = t(a) %>% as_data_frame() 
a$group = group
a = a%>% dplyr::rename(value = V1)
a$group <- as.factor(a$group)

a = a %>% select(group, value)


df <- data.frame(
  group = c("foo", "bar", "xyz","foo-foo2", "bar-bar2"),
  value = c(11775., 21321., 15506., 12717., 21281.)
)


sunburst(df,
         sortFunction = htmlwidgets::JS('function(x) {return x;}'))
new_order <- c(3,2,1)
sunburst(df[new_order,],
         sortFunction = htmlwidgets::JS('function(x) {return x;}'))

