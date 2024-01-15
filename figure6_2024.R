
ebt9<- mut_OK9 %>%
  ilter(`NIVEL 2` != "Remoção por Mudança de Uso da Terra")%>% 
  group_by(`NIVEL 2`, `NIVEL 3`) %>% 
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

df <- df %>% 
  mutate(`NIVEL 2` = recode(`NIVEL 2`,
                            `Remoção por Vegetação Secundária`= "Removals by secondary vegetation", 
                            `Remoção em Áreas Protegidas`= "Removals in protected areas",
                            `Alterações de Uso do Solo` = "Net emissions by land use change",
                            `Resíduos Florestais` = "Emissions from the burning of vegetation residues"))



df <- df %>% 
  mutate(BIOMA= recode(`NIVEL 3`,
                       `Amazônia` = "Amazon",
                       `Caatinga` = "Caatinga",
                       `Cerrado`= "Cerrado", 
                       `Mata Atlântica`= "Atlantic Forest",
                       `Pampa` = "Pampa",
                       `Pantanal` = "Pantanal"))

#############################

plot2 <- df %>%
  filter (ANO == "2019") %>%
  filter (BIOMA == "Caatinga" |
            BIOMA == "Cerrado" |
            BIOMA =="Atlantic Forest"| 
            BIOMA == "Pampa"|
            BIOMA == "Pantanal")%>%
  ggplot(aes(x=factor(BIOMA, levels = c('Caatinga','Cerrado','Atlantic Forest', 'Pampa', 'Pantanal')), y=VALOR, fill= factor(`NIVEL 2`, levels = c('Emissions from the burning of vegetation residues',
                                                                                                                                                   'Net emissions by land use change',
                                                                                                                                                   'Removals by secondary vegetation',
                                                                                                                                                   'Removals in protected areas'))))+
  geom_bar(position="stack", stat="identity", width = 0.35, na.rm = T)+
  geom_hline(yintercept = 0, show.legend =F, colour = "black",type = "l")+ #,lty=1, lwd=1)+
  #guides(fill=guide_legend(title="Transições de Remoções", color = "black", family = "fonte.tt", face = "bold"))+
  ylab(" ") +
  xlab(" ")+
  scale_y_continuous(limits=c(-100, 100), breaks = c(-100,-80,-60,-40,-20,0,20,40,60,80,100))+
  scale_fill_manual(values=c("#fcd5b5","#c0504d","#9bbb59","#4f6228"))+
  scale_color_manual(labels = c('Emissions from the burning of vegetation residues',
                                'Net emissions by land use change',
                                'Removals in protected areas',
                                'Removals by secondary vegetation'))+
  labs(fill = " ")+
  guides(fill=guide_legend(nrow = 3, byrow = T))+        
  #theme_bw()+
  theme_classic()+
  theme(legend.position=c(.5,.1), legend.box = "horizontal",legend.justification = "center")+
  theme(legend.key = element_blank())+
  theme(legend.key.height = unit(0.1, "mm"))+
  theme(legend.background = element_blank())+
  theme(legend.title = element_text(color = "black", family = "fonte.tt", size=9))+
  theme(axis.title = element_text(color = "black",family = "fonte.tt", size=9))+
  theme(legend.text =  element_text(color = "black",family = "fonte.tt", size=9))+ # Aqui e a letra da legenda
  theme(axis.title.x = element_text(color = "black",family = "fonte.tt", size=9, face = "bold"))+
  theme(axis.title.y = element_text(color = "black",family = "fonte.tt", size=10, face = "bold"))+ #Aqui é a legenda do eixo y 
  theme(axis.text.x = element_text(color = "black",family = "fonte.tt",size=9, angle = 0))+ #Aqui é a legenda do eixo x
  theme(axis.text.y = element_text(color = "black",family = "fonte.tt",size=9))#Aqui é a legenda do eixo y
plot(plot2)


#########################################################

plot11 <- df %>%
  filter (ANO == "2019") %>%
  filter (BIOMA == "Amazon")%>%
  ggplot(aes(x=BIOMA, y=VALOR, fill= factor(`NIVEL 2`, levels = c('Emissions from the burning of vegetation residues',
                                                                  'Net emissions by land use change',
                                                                  'Removals by secondary vegetation',
                                                                  'Removals in protected areas'))))+
  geom_bar(position="stack", stat="identity", width = 0.75, na.rm = T)+
  geom_hline(yintercept = 0, show.legend =F, colour = "black",type = "l")+ #,lty=1, lwd=1)+
  #guides(fill=guide_legend(title="Transições de Remoções", color = "black", family = "fonte.tt", face = "bold"))+
  ylab(expression(bold("Millions of tonnes of CO"["2"]*"e (GWP-AR5)"))) +
  #ylab(bquote(bold("Millions of tonnes of CO"[bold(2)]*"e (GWP-AR5)")))+
  #ylab(bquote("Millions of tonnes of " * CO[2] * e ~ (GWP-AR5))) +
  #theme(axis.title.y = element_text(face = "bold"))+
  xlab(" ")+
  scale_y_continuous(limits=c(-1000, 1000), breaks = c( -1000, -800,-600,-400,-200,0,200,400,600,800,1000))+
  scale_fill_manual(values=c("#fcd5b5","#c0504d","#9bbb59","#4f6228"))+
  scale_color_manual(labels = c('Emissions from the burning of vegetation residues',
                                'Net emissions by land use change',
                                'Removals in protected areas',
                                'Removals by secondary vegetation'))+
  labs(fill = " ")+
  guides(fill=guide_legend(nrow = 3, byrow = T))+        
  #theme_bw()+
  theme_classic()+
  theme(legend.position=c(.5,.1), legend.box = "horizontal",legend.justification = "center")+
  theme(legend.key = element_blank())+
  theme(legend.key.height = unit(0.1, "mm"))+
  theme(legend.background = element_blank())+
  theme(legend.title = element_text(color = "black", family = "fonte.tt", size=9))+
  theme(axis.title = element_text(color = "black",family = "fonte.tt", size=9))+
  theme(legend.text =  element_text(color = "black",family = "fonte.tt", size=9))+ # Aqui e a letra da legenda
  theme(axis.title.x = element_text(color = "black",family = "fonte.tt", size=9, face = "bold"))+
  theme(axis.title.y = element_text(color = "black",family = "fonte.tt", size=10, face = "bold"))+ #Aqui é a legenda do eixo y 
  theme(axis.text.x = element_text(color = "black",family = "fonte.tt",size=9, angle = 0))+ #Aqui é a legenda do eixo x
  theme(axis.text.y = element_text(color = "black",family = "fonte.tt",size=9))#Aqui é a legenda do eixo y
plot(plot11)


p<- ggpubr::ggarrange(plot11, plot2,
                      ncol=2,
                      widths= c(.2,1),
                      heights = c(1,.2),# list of plots
                      #labels = "auto", # labels
                      labels = c("       (a)", "      (b)"),  # Adicionar rótulos (a) e (b)
                      label.x = 0,
                      label.y = 1.0,
                      common.legend = T,# COMMON LEGEND
                      legend = "bottom", # legend position
                      #align = "hv",
                      #align = "hv",# Align them both, horizontal and vertical
                      nrow = 1)  # number of rows

plot(p)



ggsave("Figure634_2024_12-12.jpeg", plot = p, 
       width = 8,
       height = 6,
       #units = c("cm"),
       dpi = 600)


