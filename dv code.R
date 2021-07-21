
library(plotly)

##load data

data1 <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen1 050.csv")
data2 <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen2 050.csv")
data3 <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen3 050.csv")
data4 <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen4 050.csv")
data5 <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen5 050.csv")
#data5 <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen6 050.csv")
data6 <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen6 050.csv")

data2km <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen2 050 km.csv")
data2ss <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen 2 050 ss km.csv")
data2int <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen 2 050 int km.csv")
data2ssint <- read.csv("//gobo/USER40/u/u1474467/Documents/PHD/Bias Paper/double violins/scen 2 050 ss int km.csv") # change when correct simulation is run

data1 <- data1[order(data1$method),]
data2 <- data2[order(data2$method),]
data3 <- data3[order(data3$method),]
data4 <- data4[order(data4$method),]
data5 <- data5[order(data5$method),]
data6 <- data6[order(data6$method),]

data2km <- data2km[order(data2km$method),]
data2ss <- data2ss[order(data2ss$method),]
data2int <- data2int[order(data2int$method),]
data2ssint <- data2ssint[order(data2ssint$method),]

data2ss<-data2ss[!(data2ss$vdiff_ > 2 | data2ss$vdiff_ < -2),]
data2ssint<-data2ssint[!(data2ssint$vdiff_ > 2 | data2ssint$vdiff_ < -2),]

bw <- 0.05
true1 <- 0

## adapt below code to produce a graph for each set of data.

fig1a <- data1 %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))


fig1a <- fig1a %>%
  add_trace(
    x = ~method[data1$arm == 'i'],
    y = ~vdiff_[data1$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',
    scalegroup = 'Yes',
    name = 'Intervention',
    side = 'negative',  points=F,
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("#F8FECA") , bandwidth = bw
  ) 

fig1a <- fig1a %>%
  add_trace(
    x = ~method[data1$arm == 'c'],
    y = ~vdiff_[data1$arm == 'c'], type = 'violin',
    legendgroup = 'No',
    scalegroup = 'No',
    name = 'Control',
    side = 'positive',  points=F,
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("#FECACA") , bandwidth = bw
  ) 

fig1a <- fig1a %>%
  layout(
    xaxis = list(
      title = "",  
      range=c(-0.5,4.5), categoryorder="array" , showticklabels=F, title="Scenario 1",
      categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all" ) 
    ),
    yaxis = list(
      title = "Difference (LY)",
      zeroline = F, 
      range =c(-1.5,1.5) , titlefont=list(size=20)
    ),
    annotations = list(
      x = 2 , 
      y = -1.3 , 
      text = "Scenario 1",
      showarrow = FALSE,
      xanchor = "middle", font=list(size=20)
    )
    ,
    violingap = 0,
    violingroupgap = 0,
    violinmode = 'overlay' 
  )




#fig1a


#############

fig2a <- data2 %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))


fig2a <- fig2a %>%
  add_trace(
    x = ~method[data2$arm == 'i'],    y = ~vdiff_[data2$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#F8FECA") , bandwidth = bw
  ) 

fig2a <- fig2a %>%
  add_trace(
    x = ~method[data2$arm == 'c'],    y = ~vdiff_[data2$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#FECACA") , bandwidth = bw
  ) 

fig2a <- fig2a %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,4.5), categoryorder="array", showticklabels=F , 
    categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all" )     ),
    yaxis = list(      title = "",      zeroline = F,      range =c(-1.5,1.5)    ),     annotations = list(
      x = 2 , 
      y = -1.3 , 
      text = "Scenario 2",
      showarrow = FALSE,
      xanchor = "middle", font=list(size=20)
    )
    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay' , showlegend=F
  )

#fig2a

##########

fig3a <- data3 %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))


fig3a <- fig3a %>%
  add_trace(
    x = ~method[data3$arm == 'i'],    y = ~vdiff_[data3$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#F8FECA"), bandwidth = bw
  ) 

fig3a <- fig3a %>%
  add_trace(
    x = ~method[data3$arm == 'c'],    y = ~vdiff_[data3$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#FECACA"), bandwidth = bw
  ) 

fig3a <- fig3a %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,4.5), categoryorder="array", showticklabels=F ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all" )     ),
    yaxis = list(      title = "Difference (LY)",      zeroline = F,      range =c(-1.5,1.5) , titlefont=list(size=20)   ),     
    annotations = list(      x = 2 ,       y = -1.3 ,       text = "Scenario 3",      showarrow = FALSE,      xanchor = "middle", font=list(size=20)  )    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay', showlegend=F
  )

#####


fig4a <- data4 %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))


fig4a <- fig4a %>%
  add_trace(
    x = ~method[data4$arm == 'i'],    y = ~vdiff_[data4$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#F8FECA"), bandwidth = bw
  ) 

fig4a <- fig4a %>%
  add_trace(
    x = ~method[data4$arm == 'c'],    y = ~vdiff_[data4$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#FECACA"), bandwidth = bw
  ) 

fig4a <- fig4a %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,4.5), categoryorder="array", showticklabels=F ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all" )     ),
    yaxis = list(      title = "",      zeroline = F,      range =c(-1.5,1.5)    ),
    annotations = list(      x = 2 ,       y = -1.3 ,       text = "Scenario 4",      showarrow = FALSE,      xanchor = "middle", font=list(size=20)  )    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay', showlegend=F
  )


##################

fig5a <- data5 %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))


fig5a <- fig5a %>%
  add_trace(
    x = ~method[data5$arm == 'i'],    y = ~vdiff_[data5$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#F8FECA"), bandwidth = bw
  ) 

fig5a <- fig5a %>%
  add_trace(
    x = ~method[data5$arm == 'c'],    y = ~vdiff_[data5$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#FECACA"), bandwidth = bw
  ) 

fig5a <- fig5a %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,4.5), categoryorder="array", tickfont=list(size=17) ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all" )     ),
    yaxis = list(      title = "Difference (LY)",      zeroline = F,      range =c(-1.5,1.5) , titlefont=list(size=20)   ),
    annotations = list(      x = 2 ,       y = -1.3 ,       text = "Scenario 5",      showarrow = FALSE,      xanchor = "middle", font=list(size=20)  )    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay', showlegend=F
  )


###################


fig6a <- data6 %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))


fig6a <- fig6a %>%
  add_trace(
    x = ~method[data6$arm == 'i'],    y = ~vdiff_[data6$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#F8FECA"), bandwidth = bw
  ) 

fig6a <- fig6a %>%
  add_trace(
    x = ~method[data6$arm == 'c'],    y = ~vdiff_[data6$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),
    meanline = list(      visible = T    ),
    color = I("#FECACA"), bandwidth = bw
  ) 

fig6a <- fig6a %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,4.5), categoryorder="array", tickfont=list(size=17) ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all" )     ),
    yaxis = list(      title = "",      zeroline = F,      range =c(-1.5,1.5)    ),
    annotations = list(      x = 2 ,       y = -1.3 ,       text = "Scenario 6",      showarrow = FALSE,      xanchor = "middle", font=list(size=20)  )    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay', showlegend=F
  )





fig_1 <- subplot(fig1a,fig2a,fig3a,fig4a,fig5a,fig6a,shareX=F, nrows=3, titleX = T, titleY=T)
fig_1



##### additional plot exploring sample size and interaction effects #####

# using fig2_a as above (2a) to be the bench mark.


fig2_a <- data2km %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))

fig2_a <- fig2_a %>%
  add_trace(
    x = ~method[data2km$arm == 'i'],    y = ~vdiff_[data2km$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#F8FECA"),  bandwidth = bw
  ) 

fig2_a <- fig2_a %>%
  add_trace(
    x = ~method[data2km$arm == 'c'],    y = ~vdiff_[data2km$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#FECACA"), bandwidth = bw
  ) 

fig2_a <- fig2_a %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,5.5), categoryorder="array", showticklabels=T , tickfont=list(size=18) ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all","Complete Kaplan-Meier" )     ),
    yaxis = list(      title = "Difference (LY)",      zeroline = F,      range =c(-1.5,1.5), titlefont=list(size=20)    ),     annotations = list(
      x = 2.5 ,       y = -1.3 ,       text = "Scenario 2 - Original",      showarrow = FALSE,
      xanchor = "middle", font=list(size=20)
    )
    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay' , showlegend=F
  )



fig2b <- data2ss %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))

fig2b <- fig2b %>%
  add_trace(
    x = ~method[data2ss$arm == 'i'],    y = ~vdiff_[data2ss$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#F8FECA") , bandwidth = bw
  ) 

fig2b <- fig2b %>%
  add_trace(
    x = ~method[data2ss$arm == 'c'],    y = ~vdiff_[data2ss$arm == 'c'], type = 'violin', 
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#FECACA"), bandwidth = bw
  ) 

fig2b <- fig2b %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,5.5), categoryorder="array", showticklabels=T , tickfont=list(size=18) ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all","Complete Kaplan-Meier" )     ),
    yaxis = list(      title = "Difference (LY)",      zeroline = F,      range =c(-1.5,1.5), titlefont=list(size=20)    ),     annotations = list(
      x = 2.5 ,       y = -1.3 ,       text = "Scenario 2 - Increased sample size",      showarrow = FALSE,
      xanchor = "middle", font=list(size=20)
    )
    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay' , showlegend=F
  )

fig2c <- data2int %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))

fig2c <- fig2c %>%
  add_trace(
    x = ~method[data2int$arm == 'i'],    y = ~vdiff_[data2int$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F,
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#F8FECA") , bandwidth = bw
  ) 

fig2c <- fig2c %>%
  add_trace(
    x = ~method[data2int$arm == 'c'],    y = ~vdiff_[data2int$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#FECACA"), bandwidth = bw
  ) 

fig2c <- fig2c %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,5.5), categoryorder="array", showticklabels=T , tickfont=list(size=18) ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all","Complete Kaplan-Meier" )     ),
    yaxis = list(      title = "Difference (LY)",      zeroline = F,      range =c(-1.5,1.5), titlefont=list(size=20)    ),     annotations = list(
      x = 2.5 ,       y = -1.3 ,       text = "Scenario 2 - Separate models if sig. interaction",      showarrow = FALSE,
      xanchor = "middle", font=list(size=20)
    )
    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay' , showlegend=F
  )

fig2d <- data2ssint %>% plot_ly( y=~true1, x=~method, name='True RMST',mode='lines', type='scatter', line = list(color = 'black', dash='solid', width = 1))

fig2d <- fig2d %>%
  add_trace(
    x = ~method[data2ssint$arm == 'i'],    y = ~vdiff_[data2ssint$arm == 'i'], type = 'violin',
    legendgroup = 'Yes',    scalegroup = 'Yes',    name = 'Intervention',    side = 'negative', points=F, 
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#F8FECA") , bandwidth = bw
  ) 

fig2d <- fig2d %>%
  add_trace(
    x = ~method[data2ssint$arm == 'c'],    y = ~vdiff_[data2ssint$arm == 'c'], type = 'violin',
    legendgroup = 'No',    scalegroup = 'No',    name = 'Control',    side = 'positive', points=F,
    box = list(      visible = T    ),    meanline = list(      visible = T    ),    color = I("#FECACA") , bandwidth = bw
  ) 

fig2d <- fig2d %>%
  layout(
    xaxis = list(      title = "",        range=c(-0.5,5.5), categoryorder="array", tickfont=list(size=18) ,
                       categoryarray=list("AIC - Independent","BIC - Independent","AIC - Combined","BIC - Combined","Average of all","Complete Kaplan-Meier" )     ),
    yaxis = list(      title = "Difference (LY)",      zeroline = F,      range =c(-1.5,1.5), titlefont=list(size=20)    ),     annotations = list(
      x = 2.5 ,       y = -1.3 ,       text = "Scenario 2 - Increased sample size and separate models if sig. interaction",      showarrow = FALSE,
      xanchor = "middle", font=list(size=20) 
    )
    ,
    violingap = 0,    violingroupgap = 0,    violinmode = 'overlay' , showlegend=F
  )

fig_2 <- subplot(fig2_a,fig2b,fig2c,fig2d, shareX=F, nrows=4, titleX = T, titleY=T)
fig_2
