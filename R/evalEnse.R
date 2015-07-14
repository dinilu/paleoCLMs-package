evalEnse <-
    function(obse, pred, tS, binary=TRUE){
        #    obse=testCommM[[1]]
        #    pred=modPred
        #    tS=iTrain[[1]]
        #    binary=FALSE
        #    rm(obse, pred, tS, binary)
        #    rm(obseTest, predTest, jaccard, meanJacc, sdJacc, jacObse, jacPred, jacDis, jacCal, richObse,richPred, richCor, richMef, res)
        
        obseTest <- obse[!tS,]
        predTest <- pred[!tS,]
        
        jaccard <- NULL
        for(i in 1:nrow(obseTest)){
            #    print(i)
            jaccard[i] <- vegdist(rbind(obseTest[i,], predTest[i,]), method="jaccard", binary=binary)
        }
        meanJacc <- mean(jaccard, na.rm=T)
        sdJacc <- var(jaccard, na.rm=T)
        
        jacObse <- vegdist(obseTest, method="jaccard", binary=binary)
        jacPred <- vegdist(predTest, method="jaccard", binary=binary)
        # Correlation
        jacDis <- cor(jacObse, jacPred, method="p")
        # Nash-Sutcliffe model efficiency coefficient
        jacCal <- 1 - ((sum((jacObse-jacPred)^2))/(sum((jacObse-mean(jacObse))^2)))
        
        richObse <- rowSums(obseTest)
        richPred <- rowSums(predTest)
        # Correlation
        richCor <- cor(richObse, richPred, method="p")
        # Nash-Sutcliffe model efficiency coefficient
        richMef <- 1 - ((sum((richObse-richPred)^2))/(sum((richObse-mean(richObse))^2)))
        
        #  betaTest <- beta.multi(obseTest)
        #  betaPred <- beta.multi(predTest)
        #  betaDif <- mapply(FUN=function(x, y){x - y}, x=betaTest, y=betaPred)
        
        #  res <- c(meanJacc, sdJacc, corRich, betaDif)
        #  names(res) <- c("jacc.MEAN","jacc.SD","spRich.COR","beta.SIM.DIF","beta.SNE.DIF","beta.SOR.DIF")
        res <- c(meanJacc, sdJacc, jacCal, jacDis, richCor, richMef)
        names(res) <- c("jacc.MEAN","jacc.SD","jacc.CAL","jacc.DIS","spRich.COR","spRich.Mef")
        
        return(res)
    }
