low4 <- low3
low4$group <- "low"

high4 <- high3
high4$group <- "high"

mix <- rbind(low4, high4)
mix$group <- as.factor(mix$group)

onewaytests::bf.test(data = mix,
                     IB_S_Pos ~ group)
onewaytests::bf.test(data = mix,
                     IB_N_Pos ~ group)
onewaytests::bf.test(data = mix,
                     IB_S_Neg ~ group)
onewaytests::bf.test(data = mix,
                     IB_N_Neg ~ group)

onewaytests::bf.test(data = mix,
                     MB_Pos ~ group)
onewaytests::bf.test(data = mix,
                     MB_Neg ~ group)



var(low4$IB_S_Pos)
var(high4$IB_S_Pos)

var(low4$IB_N_Pos)
var(high4$IB_N_Pos)

var(low4$IB_S_Neg)
var(high4$IB_S_Neg)

var(low4$IB_S_Neg)
var(high4$IB_S_Neg)

var(low4$MB_Pos)
var(high4$MB_Pos)

var(low4$MB_Neg)
var(high4$MB_Neg)


sd(low4$IB_S_Pos)
sd(high4$IB_S_Pos)

sd(low4$IB_N_Pos)
sd(high4$IB_N_Pos)

sd(low4$IB_S_Neg)
sd(high4$IB_S_Neg)

sd(low4$IB_S_Neg)
sd(high4$IB_S_Neg)

sd(low4$MB_Pos)
sd(high4$MB_Pos)

sd(low4$MB_Neg)
sd(high4$MB_Neg)



library("igraph")

graph1 <- qgraph(cor_auto(low3), graph = "glasso", tuning = .5, sampleSize = nrow(low3))
g <- as.igraph(graph1, attributes = TRUE)

out <- data.frame(a = NULL,
                  b = NULL,
                  c = NULL,
                  d = NULL,
                  e = NULL,
                  f = NULL)
for(i in 1:1000) {
sgc <- spinglass.community(g)
out[i,1:6] <- sgc$membership[1:6]
}
out$test <- ifelse(out$V1 == 1 & out$V2 == 1 & out$V3 == 2 &
                   out$V4 == 2 & out$V5 == 1 & out$V6 == 2, 1,
            ifelse(out$V1 == 2 & out$V2 == 2 & out$V3 == 1 &
                   out$V4 == 1 & out$V5 == 2 & out$V6 == 1, 1,  0))

sum(out$test)/1000 # this is the main output


graph2 <- qgraph(cor_auto(high3), graph = "glasso", tuning = .5, sampleSize = nrow(high3))
g2 <- as.igraph(graph2, attributes = TRUE)

out2 <- data.frame(a = NULL,
                  b = NULL,
                  c = NULL,
                  d = NULL,
                  e = NULL,
                  f = NULL)
for(i in 1:1000) {
  sgc2 <- spinglass.community(g2)
  out2[i,1:6] <- sgc2$membership[1:6]
}
out2$test <- ifelse(out2$V1 == 1 & out2$V2 == 1 & out2$V3 == 2 &
                     out2$V4 == 2 & out2$V5 == 1 & out2$V6 == 2, 1,
                   ifelse(out2$V1 == 2 & out2$V2 == 2 & out2$V3 == 1 &
                            out2$V4 == 1 & out2$V5 == 2 & out2$V6 == 1, 1,  0))

sum(out2$test)/1000 # this is the main output


g3 <- as.igraph(mgm_figure, attributes = TRUE)

out3 <- data.frame(MH = NULL,
                   IB_S_Pos = NULL,
                   IB_N_Pos = NULL,
                   IB_S_Neg = NULL,
                   IB_N_Neg = NULL,
                   MB_Pos = NULL,
                   MB_Neg = NULL)
for(i in 1:50) {
  sgc3 <- spinglass.community(g3)
  out3[i,1:7] <- sgc3$membership[1:7]
}
out3 <- rename(out3,
               MH = V1,
               IB_S_Pos = V2,
               IB_N_Pos = V3,
               IB_S_Neg = V4,
               IB_N_Neg = V5,
               MB_Pos = V6,
               MB_Neg = V7)
out3$test <- ifelse(out3$IB_S_Pos == 1 & out3$IB_N_Pos == 1 & out3$IB_S_Neg == 2 &
                      out3$IB_N_Neg == 2 & out3$MB_Pos == 1 & out3$MB_Neg == 2, 1,
                    ifelse(out3$IB_S_Pos == 2 & out3$IB_N_Pos == 2 & out3$IB_S_Neg == 1 &
                             out3$IB_N_Neg == 1 & out3$MB_Pos == 2 & out3$MB_Neg == 1, 1,  0))

sum(out3$test)/nrow(out3) # proportion of runs splitting into positive and negative

out3$MHPos <- ifelse(out3$MH == out3$IB_S_Pos, 1, 0)
sum(out3$MHPos)/nrow(out3) # number of runs that mental health falls into the positive group.


#######################################


library("devtools")
devtools::install_github('hfgolino/EGA')
library("EGAnet")
ega<-EGA(low3, plot.EGA = TRUE, model = "glasso")
