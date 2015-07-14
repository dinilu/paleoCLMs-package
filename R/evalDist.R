evalDist <-
    function(obse, pred, tS=NULL, obseTrain=NULL, predTrain=NULL, output=c("test", "train")){
        # obse=testDataset
        # pred=testPredict
        # tS=NULL
        # obseTrain=trainDataset
        # predTrain=trainPredict
        # output="train"
        
        
        res <- vector(mode="list", length=8)
        names(res) <- c("taxon","prev","auc","trmin","tss","sens","spec","brier")
        
        for(i in 1:ncol(obse)){
            #    print(i)
            if(is.null(tS)){
                pObse <- which(obse[, i] == 1)
                aObse <- which(obse[, i] == 0)
                pTrainObse <- which(obseTrain[, i] == 1)
                aTrainObse <- which(obseTrain[, i] == 0)
            }else{
                pObse <- which(obse[, i] == 1 & !tS)
                aObse <- which(obse[, i] == 0 & !tS)
                pTrainObse <- which(obse[, i] == 1 & tS)
                aTrainObse <- which(obse[, i] == 0 & tS)
            }

            pTest <- pred[pObse, i]
            aTest <- pred[aObse, i]
            pTrain <- predTrain[pTrainObse, i]
            aTrain <- predTrain[aTrainObse, i]
            
            la <- length(pTest)
            lb <- length(aTest)
            lc <- length(pTrain)
            ld <- length(aTrain)
            
            le <- min(la,lb,lc,ld)
            
            res[["taxon"]][i] <- colnames(obse)[i]
            
            if(le != 0){
                # Brier scores [0-1] 0 best possible prediction
                testBrier <- sum((pred[,i]-obse[,i])^2)/length(obse[,i])
                trainBrier <- sum((predTrain[,i]-obseTrain[,i])^2)/length(obseTrain[,i]) 
                testEval <- evaluate(p=pTest, a=aTest)
                trainEval <- evaluate(p=pTrain, a=aTrain)
                
                trmin <- trainEval@t[which.min(abs(trainEval@TPR - trainEval@TNR))]
                
                if(output == "test"){
                    res[["auc"]][i] <- testEval@auc
                    res[["trmin"]][i] <- trmin
                    ind <- which.min(abs(testEval@t - trmin))
                    res[["prev"]][i] <- testEval@prevalence[ind]
                    res[["sens"]][i] <- testEval@TPR[ind]
                    res[["spec"]][i] <- testEval@TNR[ind]
                    res[["tss"]][i] <- testEval@TPR[ind] + testEval@TNR[ind] - 1
                    res[["brier"]][i] <- testBrier
                }
                if(output == "train"){
                    res[["auc"]][i] <- trainEval@auc
                    res[["trmin"]][i] <- trmin
                    ind <- which.min(abs(trainEval@t - trmin))
                    res[["prev"]][i] <- trainEval@prevalence[ind]
                    res[["sens"]][i] <- trainEval@TPR[ind]
                    res[["spec"]][i] <- trainEval@TNR[ind]
                    res[["tss"]][i] <- trainEval@TPR[ind] + trainEval@TNR[ind] - 1
                    res[["brier"]][i] <- trainBrier
                }
            }else{
                res[["auc"]][i] <- NA
                res[["trmin"]][i] <- NA
                res[["prev"]][i] <- NA
                res[["sens"]][i] <- NA
                res[["spec"]][i] <- NA
                res[["tss"]][i] <- NA
                res[["brier"]][i] <- NA
            }
        }
        res <- data.frame(res)
        
        return(res)
    }
