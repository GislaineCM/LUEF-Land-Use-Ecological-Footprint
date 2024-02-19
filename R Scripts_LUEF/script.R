#setwd("F:\\Artigo_Rafael_2022\script")

tab <- read.csv('tabela_2.csv', sep=';')


library(ggplot2)


tab_2 <- tab |> 
  dplyr::group_by(Year,Region) |> 
  dplyr::mutate(
    percetagem= (Area/sum(Area))*100
  ) |> 
  dplyr::glimpse()


ggplot(aes(x=Year,y=percetagem,fill=LU_br), data=tab_2)+
  geom_col()+
  coord_polar(theta = 'y')+
  facet_wrap(~regiao)+
  labs(x='Ano',y='', fill='Uso e Ocupação')+
  theme_bw()


Br <- tab |> 
  dplyr::select(Year, LU, LU_br, Area, Ind) |> 
  dplyr::group_by(Year, LU) |> 
  dplyr::mutate(
    Br_per=(Area/sum(Area))*100,
    total=sum(Area),
    ind = mean(Ind)
    
  )



Br |> 
  ggplot(aes(x=Year,y=Br_per,fill=LU_br))+
  geom_col()+
  coord_polar(theta = 'y')+
  labs(x='Ano',y='', fill='Uso e Ocupação')+
  theme_bw()




tab_2 |> 
  ggplot(aes(x=LU_br,y=Area/1000000,fill=as.factor(Year)))+
  geom_col(position = 'dodge')+
  facet_wrap(~regiao,scales = 'free')+
  labs(x='Uso e Ocupação do Solo', 
       y=expression('Área ('~m^2~''[X]~''~10^6~')'),fill='Ano')+
  theme(axis.text.x = element_text(angle=90,hjust = 1))


ggplot(tab_2,aes(x=LU_br,y=Ind,fill=as.factor(Year)))+
  geom_col(position = 'dodge')+
  facet_wrap(~regiao,scales = 'free')+
  labs(x='Uso e Ocupação do Solo', 
       y=expression('Ind ('~m^2~''[X]~''~hab^-1~')'),fill='Ano')+
  theme(axis.text.x = element_text(angle=90,hjust = 1))






##################


tab |> 
  dplyr::filter(LU!='Other') |> 
  ggplot(aes(x=Year,y=Ind))+
  geom_point()+
  geom_smooth(method = lm, formula = y~poly(x,2))+
  facet_wrap(~regiao+LU_br, scales = 'free')+
  ylab(expression('Ind ('~m^2*hab^-1~')'))+
  xlab('Ano')



###############

tab |> 
  dplyr::filter(regiao=='Centro Oeste') |> 
  ggplot(aes(x=LU,y=Ind,fill=as.factor(Year)))+
  geom_col(position = 'dodge')+
  labs(x='Land Use', 
       y=expression('Ind ('~m^2~''[X]~''~hab^-1~')'),fill='Year',
       title='Midwest')+
  theme(axis.text.x = element_text(angle=90,hjust = 1))


Br |> 
  ggplot(aes(x=LU_br,y=ind,fill=as.factor(Year)))+
  geom_col(position = 'dodge')+
  labs(x='Uso e Ocupação', 
       y=expression('Ind ('~m^2~''[X]~''~hab^-1~')'),fill='Ano')+
  theme(axis.text.x = element_text(angle=90,hjust = 1))

