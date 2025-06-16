

teams <- read.csv("C:\\Users\\mayka\\OneDrive - University of California\\Workshops\\GSERM\\GSERM St. Gallen 2023\\MMCPA II\\R\\R data and code\\teams.csv", header = TRUE)

process(y = "perform", x = "dysfunc", m = "negtone", model = 4, boot = 10000, 
        total = 1, data = teams, seed = 524542)

anova(model1, model2)

process(data = teams, y = "perform", x = "negtone", w = "negexp", plot = 1, jn = 1, model = 1, moments = 1)

model1 <- lm(perform~negtone+negexp, data = teams)
summary(model1)
model2 <- lm(perform~negtone*negexp, data = teams)
summary(model2)

summary(teams)

process(data = teams, y = "perform", x = "negtone", w = "negexp", plot = 1, jn = 1, model = 1, center = 1)

process (data=teams,x="dysfunc",m="negtone",y="perform",w="negexp",plot=1,
         model=14,seed=61326)

process (data=teams,x="dysfunc",m="negtone",y="perform",w="negexp",plot=1,
         model=15,seed=61326, intprobe = 1)


mentor <- read.csv("C:\\Users\\mayka\\OneDrive - University of California\\Workshops\\GSERM\\GSERM St. Gallen 2023\\MMCPA II\\R\\R data and code\\mentor.csv", header = TRUE)

savedmentor <- process(data = mentor, x = "mentor", y = "conflict", m = c("workload","resource"), w = "wforient", model = 7, plot = 1, jn = 1, save = 2)

workloadplotdat <- savedmentor[34:42, 1:3]
workloadplotdat <- data.frame(workloadplotdat)
names(workloadplotdat) <- c("X", "W", "Y")

wmarker<-c(15,15,15,16,16,16,17,17,17)
with(workloadplotdat, plot(y=Y,x=X,cex=1.2,pch=wmarker,  xlab="Extent of formal mentoring (X)",
     ylab="Access to job-related resources",col=c("purple","purple","purple",1,1,1,4,4,4)))  
legend.txt<-c("Family-focused","Blended identity", "Work-focused")
legend("topleft", legend = legend.txt,cex=1,lty=c(1,3,6),lwd=c(2,3,2),  
       pch=c(15,16,17),col=c("purple","black","blue"))  
with(workloadplotdat, lines(X[W==2.3],Y[W==2.3],lwd=2,col="purple"))  
with(workloadplotdat, lines(X[W==3.1],Y[W==3.1],lwd=3,lty=3,col="black"))  
with(workloadplotdat, lines(X[W==3.8],Y[W==3.8],lwd=2,lty=6,col="blue"))

# Johnson-Neyman Plot for Workload
workloadjnplotdat <- savedmentor[12:33, 1:7]
workloadjnplotdat <- data.frame(workloadplotdat)
names(workloadjnplotdat) <- c("wforient", "effect", "se", "t", "p", "LLCI", "ULCI")

library(ggplot2)
library(jtools)
ggplot(data = workloadjnplotdat, aes(y = effect, x = wforient))+
geom_smooth(method = "lm", color = "black")+
  geom_smooth(mapping = aes(y = LLCI, x = wforient), method = "loess", lty = "dashed", data = workloadjnplotdat)+
  geom_smooth(mapping = aes(y = ULCI, x = wforient), method = "loess", lty = "dashed", data = workloadjnplotdat)+
  geom_vline(xintercept = savedmentor[10,1])+
  geom_hline(yintercept = 0, lty = "dashed")+
  theme_apa()

# Conditional Indirect Effects
workloadCIEplotdat <- savedmentor[c(91:93,95:97), 1:5]
workloadCIEplotdat <- data.frame(workloadCIEplotdat)
names(workloadCIEplotdat) <- c("wforient", "effect", "BootSE", "BootLLCI", "BootULCI")
workloadCIEplotdat$mediator <- c(rep("workload", 3), rep("resource", 3))

ggplot(data = workloadCIEplotdat, aes(y = effect, x = wforient, color = mediator))+
  geom_point(aes(shape = mediator))+
  geom_line(aes(lty = mediator)) +
  geom_hline(yintercept = 0, lty = "dashed")+
  geom_errorbar(aes(ymin = BootLLCI, ymax = BootULCI), width = .2)+
  xlab("Work-family orientation")+
  ylab("Conditional Indirect Effect")+
  theme_apa()

# Serial Moderated Mediation
binladen <- read.csv("C:\\Users\\mayka\\OneDrive - University of California\\Workshops\\GSERM\\GSERM St. Gallen 2023\\MMCPA II\\R\\R data and code\\binladen.csv", header = TRUE)

process(data = binladen, x = "binladen", y = "mcivil", m = c("stereo", "rthreat"), 
        w = "age", cov = c("sex", "ideo"), model = 85, plot = 1, jn = 1, seed = 63234)

process(data = binladen, x = "binladen", y = "mcivil", m = c("stereo", "rthreat"), 
        w = "age", cov = c("age", "sex", "ideo"), model = 83, plot = 1, jn = 1, seed = 63234)

# Multicategorical Variables
protest <- read.csv("C:\\Users\\mayka\\OneDrive - University of California\\Workshops\\GSERM\\GSERM St. Gallen 2023\\MMCPA II\\R\\R data and code\\lawyer2.csv", header = TRUE)
summary(lm(eval~factor(protest), data = protest))


model1 <- lm(eval~approp, data = protest)
summary(model1)
model2 <- lm(eval~approp+factor(protest), data = protest)
summary(model2)
anova(model1, model2)

process(data = protest, x = "protest", y = "eval", w = "sexism", model = 1, jn = 1, mcx = 1)

process(data = protest, w = "protest", y = "eval", x = "sexism", model = 1, mcw = 3, plot = 1)

# Within-Subjects Mediation
library(foreign)
science <- read.spss("C:\\Users\\mayka\\OneDrive - University of California\\Workshops\\APS 2019\\Participant Folder\\CompSci_ws.sav", to.data.frame = TRUE)
summary(lm(int_G-int_I~1, data = science))
mean(science$int_G - science$int_I)

library(foreign)
fluency <- read.spss("C:\\Users\\mayka\\OneDrive - University of California\\Workshops\\APS 2019\\Participant Folder\\CompSci_ws.sav", to.data.frame = TRUE)

injuries <- read.csv("C:\\Users\\mayka\\OneDrive - University of California\\Workshops\\GSERM\\GSERM St. Gallen 2023\\MMCPA II\\R\\R data and code\\injuries.csv", header = TRUE)
injuriesout <- process(data = injuries, x = "exhaust", y = "safety", w = "tenure", model = 1, plot = 1, save = 2, jn = 1)
