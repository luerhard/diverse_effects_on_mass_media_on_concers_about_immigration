library(stm)
rm(list=ls())

load("/home/lukas/git/masterarbeit/data/rdata/processed.RData")
load("/home/lukas/git/masterarbeit/data/rdata/heldoutdata.RData")

path = "/mnt/Data/models/ma/stmobjects/"
heldout_path = "/mnt/Data/models/ma/heldoutobjects/"

model_names = list.files(path, pattern="*.modelR")

read_model_data <- function(modelname) {
    
    load(paste0(path, modelname))
    bound <- max(model$convergence$bound) + lfactorial(model$settings$dim$K)
    coherence = semanticCoherence(model, data$documents, M=7)
    excl = exclusivity(model, M=25)
    resid = checkResiduals(model, data$documents, tol=0.01)
    rm(model)
    load(paste0(heldout_path, modelname))
    heldout = eval.heldout(model, heldout$missing)
    rm(model)
    gc()
    
    return(list("bound" = bound,"coherence"=coherence, "exclusivity"=excl, "residuals"=resid, "likelihood"=heldout))
}

df = data.frame()

library(foreach)
library(doMC)
registerDoMC(4)

df <- foreach(model=model_names, .combine=rbind) %dopar% {
    
    result <- regmatches(model, regexec("model_k_([0-9]+).modelR", model))
    k <- as.integer(result[[1]][2])
    print(paste("Running", k, "-", model))  
    model_data <- read_model_data(model)
    
    c(k, model_data$bound, mean(model_data$exclusivity), mean(model_data$coherence), model_data$residuals$dispersion, model_data$likelihood$expected.heldout)
}


colnames(df) <- c("K", "bound", "exclusivity", "coherence", "residuals","likelihood")

df <- as.data.frame(df)

ordered_df = df[order(df$K),]

save(ordered_df, file="/home/lukas/git/masterarbeit/data/rdata/eval_models_result.RData")

load("/home/lukas/git/masterarbeit/data/rdata/eval_models_result.RData")

par(mfrow=c(2,2), family = "Times New Roman", oma = c(0, 0, 2, 0))

x_lim = c(20,180)

plot(
    x = ordered_df$K,
    y = ordered_df$likelihood,
    type = "o",
    main = "Held-out Likelihood",
    xlab = "K",
    ylab = "Likelihood",
    xlim = x_lim
)

plot(
    x = ordered_df$K, 
    y = ordered_df$exclusivity, 
    type = "o", 
    xlab = "K", 
    ylab = "mean(Exklusivität)",
    main = "Exklusivität",
    xlim = x_lim
)

plot(
    x = ordered_df$K, 
    y = ordered_df$coherence, 
    type = "o", 
    xlab = "K", 
    ylab = "mean(Kohärenz)",
    main = "semantische Kohärenz",
    xlim = x_lim
)

plot(
    x = ordered_df$K, 
    y = ordered_df$residuals, 
    type = "o", 
    xlab = "K", 
    ylab = "est. sample dispersion",
    main = "Residuen",
    xlim = x_lim
)

mtext("Diagnosewerte nach Anzahl der Topics", outer=TRUE, cex = 1.5)

ordered_df$K[-remove_entries][which.max(ordered_df$exclusivity[-remove_entries])]
ordered_df$K[-remove_entries][which.max(ordered_df$coherence[-remove_entries])]

ordered_df[which.max(ordered_df$likelihood),]

par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(1:10,  main="Plot 1")
plot(1:100,  main="Plot 2")
mtext("Title for Two Plots", outer = TRUE, cex = 1.5)