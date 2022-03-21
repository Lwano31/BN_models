
library(bnlearn)
library(forecast)
library(gmodels)
library(statip)
library(caret)

flash_data = read.csv('C:\\Users\\929538\\Documents\\SCHOOL STUFF\\Masters-WITS\\Research Report\\Data\\Probability\\flashes.csv')
flash_data$Flash_Number=as.factor(flash_data$Flash_Number)
flash_data$Year=as.factor(flash_data$Year)
flash_data$Month=as.factor(flash_data$Month)
flash_data$Day=as.factor(flash_data$Day)
flash_data$freedom=as.factor(flash_data$freedom)
flash_data$num_dfrs=as.factor(flash_data$num_dfrs)
flash_data$PSP=as.factor(flash_data$PSP)
flash_data$Strike.point=as.factor(flash_data$Strike.point)

training1=flash_data[,c("Strike.point","Year","Month","Day","Time","Lat","Long","Peak_kA","chi_square","ell_smaj","ell_smin","ell_angle","freedom","num_dfrs","PSP")]
training2=flash_data[,c("Strike.point","Lat","Long","ell_smaj","ell_smin","ell_angle","chi_square","num_dfrs","freedom","Peak_kA","PSP")]
training3=flash_data[,c("Lat","Long","chi_square","num_dfrs","freedom","Peak_kA","PSP","Strike.point")]
training4=flash_data[,c("Lat","Long","ell_smaj","ell_smin","ell_angle","PSP","Strike.point")]
training5=flash_data[,c("Lat","Long","ell_smaj","ell_smin","ell_angle","PSP","Time","Strike.point")]

net = tabu(training1, score = 'bic-cg')
net2 = tabu(training2, score = 'bic-cg')
net3 = tabu(training3, score = 'bic-cg')
net4 = tabu(training4, score = 'bic-cg')
net5 = tabu(training5, score = 'bic-cg')

P=bn.cv(data=training1,net,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P2=bn.cv(data=training2,net2,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P3=bn.cv(data=training3,net3,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P4=bn.cv(data=training4,net4,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P5=bn.cv(data=training5,net5,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
plot(P,P2,P3,P4,P5,xlab = c('Model 1','Model 2','Model 3','Model 4','Model 5'),connect = FALSE)

score(net,training1,type = 'bic-cg')
score(net2,training2,type = 'bic-cg')
score(net3,training3,type = 'bic-cg')
score(net4,training4,type = 'bic-cg')
score(net5,training5,type = 'bic-cg')

pred = predict(bn.fit(net,training1), "PSP", training1,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred),reference = as.factor(training1$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred),reference = as.factor(training1$PSP),mode = 'everything',positive = '1')

pred2 = predict(bn.fit(net2,training2), "PSP", training2,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred2),reference = as.factor(training2$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred2),reference = as.factor(training2$PSP),mode = 'everything',positive = '1')

pred3 = predict(bn.fit(net3,training3), "PSP", training3,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred3),reference = as.factor(training3$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred3),reference = as.factor(training3$PSP),mode = 'everything',positive = '1')

pred4 = predict(bn.fit(net4,training4), "PSP", training4,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred4),reference = as.factor(training4$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred4),reference = as.factor(training4$PSP),mode = 'everything',positive = '1')

pred5 = predict(bn.fit(net5,training5),"PSP",training5,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred5),reference = as.factor(training5$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred5),reference = as.factor(training5$PSP),mode = 'everything',positive = '1')



graph1=graphviz.plot(net,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net, "PSP")),
                                                                      arcs = incoming.arcs(net, "PSP"),
                                                                      col = "darkblue", fill = "olivedrab2", lwd = 3))
nodeRenderInfo(graph1)$fill['Strike.point']='plum'
edgeRenderInfo(graph1)$col[c('PSP~Strike.point')]='turquoise3'
edgeRenderInfo(graph1)$lty[c('PSP~Strike.point')]='dashed'
edgeRenderInfo(graph1)$lwd[c('PSP~Strike.point')]=2
renderGraph(graph1)

graphviz.plot(net2,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c('PSP', parents(net2, "PSP")),
                                                                       arcs = incoming.arcs(net2, "PSP"),
                                                                       col = "darkblue", fill = "tomato", lwd = 3))

graphviz.plot(net3,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net3, "PSP")),
                                                                       arcs = incoming.arcs(net3, "PSP"),
                                                                       col = "darkblue", fill = "tomato", lwd = 3))

graphviz.plot(net4,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net4, "PSP")),
                                                                       arcs = incoming.arcs(net4, "PSP"),
                                                                       col = "darkblue", fill = "tomato", lwd = 3))

graphviz.plot(net5,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net5, "PSP")),
                                                                       arcs = incoming.arcs(net5, "PSP"),
                                                                       col = "darkblue", fill = "tomato", lwd = 3))




w1=matrix(c("Month", "PSP","num_dfrs","PSP","Day","PSP","PSP",'Time',"PSP","Strike.point",'Year','PSP'), ncol = 2, byrow = TRUE)
w2=matrix(c("num_dfrs","PSP",'freedom','PSP',"PSP","Strike.point"),ncol = 2,byrow = TRUE)
w3=matrix(c("num_dfrs","PSP",'freedom',"PSP","PSP","Strike.point"),ncol = 2,byrow = TRUE)
w4=matrix(c("PSP","Strike.point"),ncol = 2,byrow = TRUE)
w5=matrix(c("PSP",'Time',"PSP","Strike.point"),ncol = 2,byrow = TRUE)

net_1 = tabu(training1, score = 'bic-cg',whitelist = w1)
net_2 = tabu(training2, score = 'bic-cg',whitelist = w2)
net_3 = tabu(training3, score = 'bic-cg',whitelist = w3)
net_4 = tabu(training4, score = 'bic-cg',whitelist = w4)
net_5 = tabu(training5, score = 'bic-cg',whitelist = w5)

P_1=bn.cv(data=training1,net_1,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P_2=bn.cv(data=training2,net_2,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P_3=bn.cv(data=training3,net_3,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P_4=bn.cv(data=training4,net_4,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
P_5=bn.cv(data=training5,net_5,runs=50,loss = 'pred-lw-cg',loss.args = list(target = 'PSP'))
plot(P,P_1,P_2,P_3,P_5,xlab = c('Model 1','Model 2','Model 3','Model 4','Model 5'),connect = FALSE)

score(net_1,training1,type = 'bic-cg')
score(net_2,training2,type = 'bic-cg')
score(net_3,training3,type = 'bic-cg')
score(net_4,training4,type = 'bic-cg')
score(net_5,training5,type = 'bic-cg')

pred_1 = predict(bn.fit(net_1,training1), "PSP", training1,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred_1),reference = as.factor(training1$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred_1),reference = as.factor(training1$PSP),mode = 'everything',positive = '1')

pred_2 = predict(bn.fit(net_2,training2), "PSP", training2,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred_2),reference = as.factor(training2$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred_2),reference = as.factor(training2$PSP),mode = 'everything',positive = '1')

pred_3 = predict(bn.fit(net_3,training3), "PSP", training3,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred_3),reference = as.factor(training3$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred_3),reference = as.factor(training3$PSP),mode = 'everything',positive = '1')

pred_4 = predict(bn.fit(net_4,training4), "PSP", training4,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred_4),reference = as.factor(training4$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred_4),reference = as.factor(training4$PSP),mode = 'everything',positive = '1')

pred_5 = predict(bn.fit(net_5,training5),"PSP",training5,method = 'bayes-lw')
confusionMatrix(data = as.factor(pred_5),reference = as.factor(training5$PSP),mode = 'everything',positive = '0')
confusionMatrix(data = as.factor(pred_5),reference = as.factor(training5$PSP),mode = 'everything',positive = '1')

graph2=graphviz.plot(net_1,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net_1, "PSP")),
                                                                      arcs = incoming.arcs(net_1, "PSP"),
                                                                      col = "darkblue", fill = "olivedrab2", lwd = 3))
nodeRenderInfo(graph2)$fill['Strike.point']='plum'
edgeRenderInfo(graph2)$col[c('PSP~Strike.point')]='turquoise3'
edgeRenderInfo(graph2)$lty[c('PSP~Strike.point')]='dashed'
edgeRenderInfo(graph2)$lwd[c('PSP~Strike.point')]=2
renderGraph(graph2)

graph3=graphviz.plot(net_2,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c('PSP', parents(net_2, "PSP")),
                                                                       arcs = incoming.arcs(net_2, "PSP"),
                                                                       col = "darkblue", fill = "olivedrab2", lwd = 3))
nodeRenderInfo(graph3)$fill['Strike.point']='plum'
edgeRenderInfo(graph3)$col[c('PSP~Strike.point')]='turquoise3'
edgeRenderInfo(graph3)$lty[c('PSP~Strike.point')]='dashed'
edgeRenderInfo(graph3)$lwd[c('PSP~Strike.point')]=2
renderGraph(graph3)

graph4=graphviz.plot(net_3,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net_3, "PSP")),
                                                                       arcs = incoming.arcs(net_3, "PSP"),
                                                                       col = "darkblue", fill = "olivedrab2", lwd = 3))
nodeRenderInfo(graph4)$fill['Strike.point']='plum'
edgeRenderInfo(graph4)$col[c('PSP~Strike.point')]='turquoise3'
edgeRenderInfo(graph4)$lty[c('PSP~Strike.point')]='dashed'
edgeRenderInfo(graph4)$lwd[c('PSP~Strike.point')]=2
renderGraph(graph4)

graphviz.plot(net_4,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net_4, "PSP")),
                                                                       arcs = incoming.arcs(net_4, "PSP"),
                                                                       col = "darkblue", fill = "tomato", lwd = 3))

graph5=graphviz.plot(net_5,shape = 'rectangle',layout = 'fdp',highlight = list(nodes = c("PSP", parents(net_5, "PSP")),
                                                                       arcs = incoming.arcs(net_5, "PSP"),
                                                                       col = "darkblue", fill = "olivedrab2", lwd = 3))
nodeRenderInfo(graph5)$fill['Strike.point']='plum'
edgeRenderInfo(graph5)$col[c('PSP~Strike.point')]='turquoise3'
edgeRenderInfo(graph5)$lty[c('PSP~Strike.point')]='dashed'
edgeRenderInfo(graph5)$lwd[c('PSP~Strike.point')]=2
renderGraph(graph5)




