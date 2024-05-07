# SEEG - Script for the plots of emissions and removals GHG_BR-----------------
-------------------------------------------------------
  Title:"Script for the plots of emissions and removals GHG_BR"

# Created by: 'Felipe Lenti, Barbara Zimbres (barbara.zimbres@ipam.org.br), Joao Siqueira e Edriano Souza'
# For clarification or an issue/bug report, please write to barbara.zimbres@ipam.org.br and/or edriano.souza@ipam.org.br
# Key activities in sections
-------------------------------------------------------
  
  #
  ##Startt
gc()
memory.limit(9999999999) # or your memory

## Setting your project.R  -------------
# !!! Eg. ~/3._Plots
# //! Create Folder: 'data'; 'R/script'; 'Results'


### Required packages  -------------------------------------------------------
# e.g.
#install.packages("pacman") #// or
## install.packages("usethis")
#install.packages(c("usethis", "geojsonR", "jsonlite", "googledrive", "openxlsx", "ggplot2", "tidyverse", "tidyr", "dplyr", "rlang"))
library(pacman)
pacman::p_load(usethis, googledrive,readxl,openxlsx, 
               ggplot2, gridExtra, grid, tidyverse, tidyr, dplyr, geobr,
               sf,magrittr,gghighlight,ggpubr, ggspatial)


## Set your directory with data uf_csv--------------------------------------------
setwd('C:/Users/edriano.souza/GitHub/2023_Paper_BABI/data')
files_mut<-dir(pattern = '\\.csv$')
for(i in files_mut) {assign(unlist(strsplit(i, "[.]"))[1], read.csv2(i,  sep = ",", h=T, encoding = "UTF-8")) }


### Join tables UF n= 27 states------------------------------------------
mut<-rbind(`TABELAO_MUT_MUN-10-05_AC`, `TABELAO_MUT_MUN-10-05_AL`, `TABELAO_MUT_MUN-10-05_AM`,
           `TABELAO_MUT_MUN-10-05_AP`, `TABELAO_MUT_MUN-10-05_BA`, `TABELAO_MUT_MUN-10-05_CE`,
           `TABELAO_MUT_MUN-10-05_DF`, `TABELAO_MUT_MUN-10-05_ES`, `TABELAO_MUT_MUN-10-05_GO`,
           `TABELAO_MUT_MUN-10-05_MA`, `TABELAO_MUT_MUN-10-05_MG`, `TABELAO_MUT_MUN-10-05_MS`,
           `TABELAO_MUT_MUN-10-05_MT`, `TABELAO_MUT_MUN-10-05_PB`, `TABELAO_MUT_MUN-10-05_PA`,
           `TABELAO_MUT_MUN-10-05_PE`, `TABELAO_MUT_MUN-10-05_PI`, `TABELAO_MUT_MUN-10-05_PR`,
           `TABELAO_MUT_MUN-10-05_RJ`, `TABELAO_MUT_MUN-10-05_RN`, `TABELAO_MUT_MUN-10-05_RO`,
           `TABELAO_MUT_MUN-10-05_RR`, `TABELAO_MUT_MUN-10-05_RS`, `TABELAO_MUT_MUN-10-05_SC`,
           `TABELAO_MUT_MUN-10-05_SE`, `TABELAO_MUT_MUN-10-05_SP`, `TABELAO_MUT_MUN-10-05_TO` 
)






rm(i, files_mut,`TABELAO_MUT_MUN-10-05_AC`, `TABELAO_MUT_MUN-10-05_AL`, `TABELAO_MUT_MUN-10-05_AM`,
   `TABELAO_MUT_MUN-10-05_AP`, `TABELAO_MUT_MUN-10-05_BA`, `TABELAO_MUT_MUN-10-05_CE`,
   `TABELAO_MUT_MUN-10-05_DF`, `TABELAO_MUT_MUN-10-05_ES`, `TABELAO_MUT_MUN-10-05_GO`,
   `TABELAO_MUT_MUN-10-05_MA`, `TABELAO_MUT_MUN-10-05_MG`, `TABELAO_MUT_MUN-10-05_MS`,
   `TABELAO_MUT_MUN-10-05_MT`, `TABELAO_MUT_MUN-10-05_PB`, `TABELAO_MUT_MUN-10-05_PA`,
   `TABELAO_MUT_MUN-10-05_PE`, `TABELAO_MUT_MUN-10-05_PI`, `TABELAO_MUT_MUN-10-05_PR`,
   `TABELAO_MUT_MUN-10-05_RJ`, `TABELAO_MUT_MUN-10-05_RN`, `TABELAO_MUT_MUN-10-05_RO`,
   `TABELAO_MUT_MUN-10-05_RR`, `TABELAO_MUT_MUN-10-05_RS`, `TABELAO_MUT_MUN-10-05_SC`,
   `TABELAO_MUT_MUN-10-05_SE`, `TABELAO_MUT_MUN-10-05_SP`, `TABELAO_MUT_MUN-10-05_TO`)

### Duplicate ---------------------------------------------------------------
mut9 <- mut


## Head (colnames)
colnames(mut9)
mut9[,15:64] <- as.numeric(unlist(mut9[,15:64])) #column with a estimates yearly in the type: numeric 


### Return Output your directory eg.--------------------------------------------
setwd('C:/Users/edriano.souza/GitHub/2024_ERL_Paper_BABI/Plots/')

## Head (colnames)
colnames(mut9)

### Recode ---------------------------------------------------------------
newNames9 <- c("NIVEL 1",
               "NIVEL 2",
               "NIVEL 3",
               "NIVEL 4",
               "NIVEL 5",
               "NIVEL 6",
               "TIPO DE EMISSÃO",
               "GÁS",
               "TERRITÓRIO",
               "BIOMA",
               "CODIBGE",
               "Nome_Município",
               ##"CODBIOMASESTADOS",
               ##"ESTADOS",
               "ATIVIDADE ECONÔMICA",
               "PRODUTO",
               "1970",
               "1971",
               "1972",
               "1973",
               "1974",
               "1975",
               "1976",
               "1977",
               "1978",
               "1979",
               "1980",
               "1981",
               "1982",
               "1983",
               "1984",
               "1985",
               "1986",
               "1987",
               "1988",
               "1989",
               "1990",
               "1991",
               "1992",
               "1993",
               "1994",
               "1995",
               "1996",
               "1997",
               "1998",
               "1999",
               "2000",
               "2001",
               "2002",
               "2003",
               "2004",
               "2005",
               "2006",
               "2007",
               "2008",
               "2009",
               "2010",
               "2011",
               "2012",
               "2013",
               "2014",
               "2015",
               "2016",
               "2017",
               "2018",
               "2019")

colnames(mut9)<-newNames9


# Select gas between ------------------------------------------------------


mut_OK9<- mut9[mut9$GÁS=="CO2e (t) GWP-AR5",]

head(mut_OK9,5)

str(mut_OK9)


mut_OK9[,15:64] <- as.numeric(unlist(mut_OK9[,15:64])) #column with a estimates yearly in the type: numeric 


#################

# Gross removals ----------------------------------------------------------

ebt9<- mut_OK9 %>%
  filter(`NIVEL 2`=="Remoção por Vegetação Secundária"|
           `NIVEL 2`=="Remoção em Áreas Protegidas") %>%
  #group_by(`NIVEL 2`, `Nome_Município`, CODIBGE) %>% #P1
  group_by(`Nome_Município`, CODIBGE) %>% #P2
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



df <- reshape (dff9, varying = list(colnames(dff9[4:33])),#i+1
               times = names(dff9[4:33]), #i+1
               timevar = "ANO",
               direction = "long")

colnames(df)

colnames(df)[5]<-c("VALOR")#i+1

colnames(df)

df<-df[,-c(6)]#i+1


###

##############################
all_mun <- read_municipality(year=2020)


dataset_finalG = left_join(all_mun, df, by=c("code_muni"="CODIBGE"))


dataset_finalGROSS = dataset_finalG %>% tidyr::drop_na(VALOR) %>%
  filter (ANO == "2019")

### Rank maps municipally
ranks = 
  dataset_finalGROSS  %>%
  mutate(ranks = order(order(VALOR, decreasing = T)))%>%
  top_n(10)
ranks = ranks %>%
  mutate(ranks = order(order(VALOR, decreasing = F)))

top10E <- dataset_final1 %>% 
  filter(`NIVEL 2` == "Remoção por Mudança de Uso da Terra")%>% 
  # selecionar o top 10
  top_n(10, VALOR)

top10W <- dataset_final1 %>%
  filter(`NIVEL 2` == "Remoção por Vegetação Secundária")%>% 
  # selecionar o top 10
  top_n(10, VALOR)    

top10Z <- dataset_final1 %>%
  filter(`NIVEL 2` == "Remoção em Áreas Protegidas")%>% 
  # selecionar o top 10
  top_n(10, VALOR)    


ranks = top10E %>%
  mutate(ranks = order(order(VALOR, decreasing = T)))

########################

#Deu certo


top10 <- dataset_finalGROSS %>%
  #filter(`NIVEL 2` == "Resíduos Florestais")%>% 
  # selecionar o top 20
  top_n(-10, VALOR)    


ranks = top10 %>%
  mutate(ranks = order(order(VALOR, decreasing = F)))



top10GROSS <- dataset_finalGROSS %>% 
  filter(`Nome_Município` == "Atalaia do Norte"|`Nome_Município` == "Lábrea"|
           `Nome_Município` == "Barcelos"|`Nome_Município` == "Santa Isabel do Rio Negro"|
           `Nome_Município` == "São Gabriel da Cachoeira"|`Nome_Município` == "Almeirim"|
           `Nome_Município` == "Altamira"|`Nome_Município` == "Itaituba"|
           `Nome_Município` == "Oriximiná"|`Nome_Município` == "São Félix do Xingu")



top10GROSS <- top10GROSS %>% 
  mutate(`rank` = recode(`Nome_Município`,
                         `Atalaia do Norte` = "5", 
                          `Lábrea` ="9",
                          `Barcelos`="8",
                          `Santa Isabel do Rio Negro`="7",
                          `São Gabriel da Cachoeira`="3",
                          `Almeirim`="4",
                          `Altamira`="1",
                          `Itaituba`="10",
                          `Oriximiná`="2",
                          `São Félix do Xingu` = "6"))


top10a <- top10GROSS

### Concatenate two string columns

top10a$mun_and_state = paste(top10a$rank," - ", top10a$name_muni,", ",top10a$abbrev_state,sep = "")



top10a <- top10a %>% 
  mutate(`NIVEL 2` = recode(`NIVEL 2`,
                            #`Remoção por Mudança de Uso da Terra` = "By other kinds of land use change",
                            `Remoção por Vegetação Secundária`= "By secondary vegetation", 
                            `Remoção em Áreas Protegidas`= "By primary vegetation in protected areas"))
#`Alterações de Uso do Solo` = "Emissions by land use change",
#`Resíduos Florestais` = "Emissions by the burning of vegetation residuals"))

### Plot1

top10a$VALOR=as.numeric(levels(top10a$VALOR))[top10a$VALOR]
p1<- top10a %>% 
  mutate(mun_and_state = fct_reorder(mun_and_state, -VALOR)) %>%
  ggplot(aes(x=reorder(mun_and_state,+VALOR), y = VALOR, fill = factor(`NIVEL 2`,levels = c('By primary vegetation in protected areas',
                                                                                            'By other kinds of land use change',
                                                                                            'By secondary vegetation'))))+
  #geom_col(aes(fill = `NIVEL 2`)) +
  geom_bar(position = position_stack(reverse = TRUE), stat="identity",  na.rm = T, width = 0.5)+
  scale_fill_manual(values=c("#4f6228" ,"#9bbb59"))+
  scale_color_manual(labels = c('By primary vegetation in protected areas', 'By other kinds of land use change',
                                'By secondary vegetation'))+
  ylim(-30, 0)+
  # inverter eixos
  coord_flip()+
  #scale_y_comma(position = "right") +
  #theme(legend.position="none")+
  labs(fill = " ")+
  guides(fill=guide_legend(nrow = 3, byrow = T))+        
  #theme_bw()+
  theme_classic()+
  #ylab("Millions of tonnes of CO2e (GWP-AR5)") +
  ylab(expression(bold("Millions of tonnes of CO"["2"]*"e (GWP-AR5)"))) +
  xlab(" ")+
  theme(legend.position=c(.3,.9), legend.box = "horizontal",legend.justification = "center")+
  theme(legend.key = element_blank())+
  theme(legend.key.height = unit(0.1, "mm"))+
  theme(legend.background = element_blank())+
  theme(legend.title = element_text(color = "black", family = "fonte.tt", size=9))+
  theme(axis.title = element_text(color = "black",family = "fonte.tt", size=9))+
  theme(legend.text =  element_text(color = "black",family = "fonte.tt", size=9))+ # Aqui e a letra da legenda
  theme(axis.title.x = element_text(color = "black",family = "fonte.tt", size=9, face = "bold"))+
  theme(axis.title.y = element_text(color = "black",family = "fonte.tt", size=10, face = "bold"))+ #Aqui é a legenda do eixo y 
  theme(axis.text.x = element_text(color = "black",family = "fonte.tt",size=9))+ #Aqui é a legenda do eixo x
  theme(axis.text.y = element_text(color = "black",family = "fonte.tt",size=9))#Aqui é a legenda do eixo y
plot(p1)




ggsave("output/Figure8_B_OK.jpeg", plot = p1,dpi = 330)





####################################################
#GROSS REMOVALS



###########################################################################################

# Change data rownames as a real column called 'carName'
data <- top10 %>%
  rownames_to_column(code_muni)


windowsFonts(fonte.tt= windowsFont("TT Times New Roman"))
a3<- ggplot() +
  geom_sf(data = dataset_finalGROSS, fill = "transparent", color=alpha("black",.25))+
  geom_sf(data=dataset_finalGROSS, aes(fill=VALOR), size=.125, color=alpha("Green",0.01))+
  scale_fill_distiller(palette = "Greens",type="seq", name=expression(bold("Mt CO"["2"]*"e")), breaks=seq(0,-40,-5))+
  #scale_fill_distiller(palette = "Oranges",type="seq",trans = "reverse", )+
  theme_minimal()+
  theme(legend.position=c(.88,.85), legend.box = "horizontal",legend.justification = "center", legend.direction = "horizontal")+
  theme(legend.key = element_blank())+
  ylab("") + 
  xlab(" ")+
  #ylab(expression(bold("Millions of tonnes of CO"["2"]*"e (GWP-AR5)"))) +
  #geom_sf(data = apendice_c_geo, fill = NA, color=alpha("black",1))+
  #geom_sf(data = coord_pontos2, aes(size = VALOR2), color=alpha("orange",0.5)) +  ### Adicional 
  #scale_fill_distiller(palette = "Oranges",type="seq",trans = "reverse", name="Mt Co2eq")+
  #scale_size_continuous(name="Mt Co2eq",breaks=seq(0,40,5))+
  theme(legend.direction = "vertical")+
  #geom_sf(data = coord_pontos, aes(size = VALOR2, color=`NIVEL 2`)) + 
  geom_sf_text(data=ranks, aes(label=ranks), size=3,family="fonte.tt")+
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
plot(a3)

max(as.numeric(coord_pontos$VALOR))
plot(a)



ggsave("output/Figure9_AA.jpeg", plot = a3,scale = 1,
       width = 18,
       height = 18,
       units = c("cm"),
       dpi = 330)





library(ggplot2)
library(gridExtra)

combined_plot5 <- arrangeGrob(a3, p1, ncol = 1, heights = unit(c(2, 1), "null"), widths = unit(1, "null"))

ggsave("output/Figure9-1AOK.jpeg", plot = combined_plot5, scale = 1,
       width = 18,
       height = 18,
       units = c("cm"),
       dpi = 330)



p<- ggpubr::ggarrange(a3, p1,
                      ncol=1,
                      widths= c(1,.1),
                      heights = c(2,1),# list of plots
                      #labels = "AUTO", # labels
                      #common.legend = T,# COMMON LEGEND
                      #legend = "top", # legend position
                      align = "v",
                      #align = "hv",# Align them both, horizontal and vertical
                      nrow = 2)  # number of rows






ggsave("Figure8.jpeg", plot = p, scale = 1,
       width = 18,
       height = 18,
       units = c("cm"),
       dpi = 330)