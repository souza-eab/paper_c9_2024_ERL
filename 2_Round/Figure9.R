



ebt9<- mut_OK9 %>%
  filter(`NIVEL 2` != "Remoção por Mudança de Uso da Terra")%>% 
  group_by(`Nome_Município`, CODIBGE) %>% #P1
  #group_by(`Nome_Município`, CODIBGE) %>% #P2
  summarise('1990'=sum(`1990`),'1991'=sum(`1991`),'1992'=sum(`1992`),
            '1993'=sum(`1993`),'1994'=sum(`1994`),'1995'=sum(`1995`),
            '1996'=sum(`1996`),'1997'=sum(`1997`),'1998'=sum(`1998`),
            '1999'=sum(`1999`),'2000'=sum(`2000`),'2001'=sum(`2001`),
            '2002'=sum(`2002`),'2003'=sum(`2003`),'2004'=sum(`2004`),
            '2005'=sum(`2005`),'2006'=sum(`2006`),'2007'=sum(`2007`),
            '2008'=sum(`2008`),'2009'=sum(`2009`),'2010'=sum(`2010`),
            '2011'=sum(`2011`),'2012'=sum(`2012`),'2013'=sum(`2013`),
            '2014'=sum(`2014`),'2015'=sum(`2015`),'2016'=sum(`2016`),
            '2017'=sum(`2017`),'2018'=sum(`2018`),'2019'=sum(`2019`))
dff9 <- as.data.frame(ebt9)



dff9$`1990` <- as.numeric(dff9$`1990`/1000000) 
dff9$`1991` <- as.numeric(dff9$`1991`/1000000) 
dff9$`1992` <- as.numeric(dff9$`1992`/1000000) 
dff9$`1993` <- as.numeric(dff9$`1993`/1000000) 
dff9$`1994` <- as.numeric(dff9$`1994`/1000000) 
dff9$`1995` <- as.numeric(dff9$`1995`/1000000) 
dff9$`1996` <- as.numeric(dff9$`1996`/1000000) 
dff9$`1997` <- as.numeric(dff9$`1997`/1000000) 
dff9$`1998` <- as.numeric(dff9$`1998`/1000000) 
dff9$`1999` <- as.numeric(dff9$`1999`/1000000) 
dff9$`2000` <- as.numeric(dff9$`2000`/1000000)   
dff9$`2001` <- as.numeric(dff9$`2001`/1000000)
dff9$`2002` <- as.numeric(dff9$`2002`/1000000)
dff9$`2003` <- as.numeric(dff9$`2003`/1000000)
dff9$`2004` <- as.numeric(dff9$`2004`/1000000)
dff9$`2005` <- as.numeric(dff9$`2005`/1000000)
dff9$`2006` <- as.numeric(dff9$`2006`/1000000)
dff9$`2007` <- as.numeric(dff9$`2007`/1000000)
dff9$`2008` <- as.numeric(dff9$`2008`/1000000)
dff9$`2009` <- as.numeric(dff9$`2009`/1000000)
dff9$`2010` <- as.numeric(dff9$`2010`/1000000)
dff9$`2011` <- as.numeric(dff9$`2011`/1000000)
dff9$`2012` <- as.numeric(dff9$`2012`/1000000)
dff9$`2013` <- as.numeric(dff9$`2013`/1000000)
dff9$`2014` <- as.numeric(dff9$`2014`/1000000)
dff9$`2015` <- as.numeric(dff9$`2015`/1000000)
dff9$`2016` <- as.numeric(dff9$`2016`/1000000)
dff9$`2017` <- as.numeric(dff9$`2017`/1000000)
dff9$`2018` <- as.numeric(dff9$`2018`/1000000)
dff9$`2019` <- as.numeric(dff9$`2019`/1000000)
#dff9$`2020` <- as.numeric(dff9$`2020`/1000000)

colnames(dff9)

df <- reshape (dff9, varying = list(colnames(dff9[3:32])),#i+1
               times = names(dff9[3:32]), #i+1
               timevar = "ANO",
               direction = "long")

colnames(df)

colnames(df)[4]<-c("VALOR")#i+1

colnames(df)

df<-df[,-c(5)]#i+1


dataset_final = left_join(all_mun, df, by=c("code_muni"="CODIBGE"))
dataset_finalNET = dataset_final %>% tidyr::drop_na(VALOR) %>%
  filter (ANO == "2019")

### Rank maps municipally
ranks = dataset_finalNET %>%
  mutate(ranks = order(order(VALOR, decreasing = T)))%>%
  top_n(10)
ranks = ranks %>%
  mutate(ranks = order(order(VALOR, decreasing = F)))

ranks1 = dataset_finalNET %>%
  mutate(ranks = order(order(VALOR, decreasing = T)))%>%
  top_n(-10)
ranks1 = ranks1 %>%
  mutate(ranks = order(order(VALOR, decreasing = F)))


ranksF<- rbind(ranks, ranks1)


top10a <- dataset_final1 %>% 
  filter(`Nome_Município` == "Atalaia do Norte"|`Nome_Município` == "Lábrea"|
           `Nome_Município` == "Barcelos"|`Nome_Município` == "Santa Isabel do Rio Negro"|
           `Nome_Município` == "São Gabriel da Cachoeira"|`Nome_Município` == "Almeirim"|
           `Nome_Município` == "Altamira"|`Nome_Município` == "Itaituba"|
           `Nome_Município` == "Oriximiná"|`Nome_Município` == "São Félix do Xingu")



ranksF <- ranksF %>% 
  mutate(`rank` = recode(`Nome_Município`,
                            `Altamira`= "9", 
                            `São Félix do Xingu` = "4",
                            `Lábrea` = "3",
                            `Porto Velho` = "1",
                            `Pacajá` = "2",
                            `Novo Progresso` = "5",
                            `Apuí`= "7",
                            `Colniza` = "6",
                            `Portel` = "7",
                            `Senador José Porfírio` = "10",
                            `Novo Repartimento` = "8",
                            
                            # Remoções
                            `Japurá` = "11", 
                            `Novo Airão` = "12", 
                            `Tapauá` ="13",
                            `Almeirim`="14",
                            `Jutaí`="15",
                            `Barcelos`="16",
                            `Santa Isabel do Rio Negro` = "17",
                            `Atalaia do Norte` = "18",	
                            `Oriximiná` = "19",
                            `São Gabriel da Cachoeira` = "20"))

### Concatenate two string columns
ranksF$mun_and_state = paste(ranksF$rank," - ", ranksF$name_muni,", ",ranksF$abbrev_state,sep = "")


ranksFF = ranksF %>%
  mutate(ranksF = order(order(VALOR, decreasing = T)))%>%
  top_n(20)


###Plot
p1<- ranksF %>% 
  #mutate(mun_and_state = fct_reorder(mun_and_state, VALOR)) %>%
  ggplot(aes(x=reorder(mun_and_state,+VALOR), y = VALOR, fill = VALOR))+
  scale_y_continuous(limits=c(-20, 20), breaks = c(-20,-15,-10,-5,0,5,10,15,20))+
  #geom_col(aes(fill = `NIVEL 2`)) +
  #scale_color_gradient2(low="blue",high="red",midpoint=0,limits=c(-30,30))+
  geom_bar(position = position_stack(reverse = TRUE), stat="identity",  na.rm = T, width = 0.5)+
  scale_fill_distiller(palette = "RdYlGn",name="Mt Co2eq")+
  #scale_fill_gradientn(colours = terrain.colors(7))+
  #ylim(-30, 30)+
  # inverter eixos
  coord_flip()+
  #scale_y_comma(position = "right") +
  labs(fill = " ")+
  #guides(fill=guide_legend(nrow = 3, byrow = T))+        
  theme_bw()+
  theme_classic()+
  #ylab("Millions of tonnes of CO2e (GWP-AR5)") +
  ylab(expression(bold("Millions of tonnes of CO"["2"]*"e (GWP-AR5)"))) +
  xlab(" ")+
  #theme(legend.position=c(.2,.6), legend.box = "horizontal",legend.justification = "center")+
  #theme(legend.key = element_blank())+
  #theme(legend.key.height = unit(0.1, "mm"))+
  theme(legend.background = element_blank())+
  theme(legend.title = element_text(color = "black", family = "fonte.tt", size=9))+
  theme(axis.title = element_text(color = "black",family = "fonte.tt", size=9))+
  theme(legend.text =  element_text(color = "black",family = "fonte.tt", size=9))+ # Aqui e a letra da legenda
  theme(axis.title.x = element_text(color = "black",family = "fonte.tt", size=9, face = "bold"))+
  theme(axis.title.y = element_text(color = "black",family = "fonte.tt", size=10, face = "bold"))+ #Aqui é a legenda do eixo y 
  theme(axis.text.x = element_text(color = "black",family = "fonte.tt",size=8))+ #Aqui é a legenda do eixo x
  theme(axis.text.y = element_text(color = "black",family = "fonte.tt",size=8)) + 
  theme(legend.position="none")
plot(p1)


# Plot 2
a<- ggplot() +
  geom_sf(data=dataset_finalNET, aes(fill=VALOR), size=.125, color=alpha("gray",0.1))+
  scale_fill_distiller(palette = "RdYlGn", name=expression(bold("Mt CO"["2"]*"e")))+
  theme_minimal()+
  #theme(legend.position=c(.88,.85), legend.box = "horizontal",legend.justification = "center", legend.direction = "horizontal")+
  theme(legend.key = element_blank())+
  ylab("") + xlab(" ")+
  theme(legend.direction = "vertical")+
  geom_sf_text(data=ranksFF, aes(label=ranksF), size=3,family="fonte.tt")+
  theme(legend.background = element_blank())+
  theme(legend.title = element_text(color = "black", family = "fonte.tt", size=9))+
  theme(axis.title = element_text(color = "black",family = "fonte.tt", size=9))+
  theme(legend.text =  element_text(color = "black",family = "fonte.tt", size=9))+ # Aqui e a letra da legenda
  theme(axis.title.x = element_text(color = "black",family = "fonte.tt", size=9, face = "bold"))+
  theme(axis.title.y = element_text(color = "black",family = "fonte.tt", size=10, face = "bold"))+ #Aqui é a legenda do eixo y 
  theme(axis.text.x = element_text(color = "black",family = "fonte.tt",size=9))+ #Aqui é a legenda do eixo x
  theme(axis.text.y = element_text(color = "black",family = "fonte.tt",size=9))+
  # Adiciona o Norte Geográfico
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale()+
  theme(legend.position = "right")





ggsave("output/Figure10_AAAAAb.jpeg", plot = a,
       width = 18,
       height = 18,
       units = c("cm"),
       dpi = 330)



combined_plot5 <- arrangeGrob(a, p1, ncol = 1, heights = unit(c(2, 1), "null"), widths = unit(1, "null"))

ggsave("output/Figure10-OK.jpeg", plot = combined_plot5, scale = 1,
       width = 18,
       height = 18,
       units = c("cm"),
       dpi = 330)


combined_plot5 <- arrangeGrob(a, p1, ncol = 1, heights = unit(c(2, 1), "null"), widths = unit(1, "null"))

ggsave("output/Figure9-AOK.jpeg", plot = combined_plot5, scale = 1,
       width = 18,
       height = 18,
       units = c("cm"),
       dpi = 330)



plot(a)

windowsFonts(fonte.tt= windowsFont("TT Arial"))
