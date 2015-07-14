\name{evalDist}
\alias{evalDist}
\title{Evaluate species distribution predictions from a community matrix}
\description{This function evaluate species distribution predictions from a Community Level Model (CLM). It evaluates the training and the testing datasets, and need both to correctly select a threshold. It take several ways to provide training/testing datasets: a logical vector to split training/testing datasets (tS) or specify two extra community matrix for the training data (predTrain and obseTrain) in that case obse and train should be the matrices for testing. It also give an option to specify which results are desired those on the training or those on the testing datasets. Finally, it allow extra parameters to the predict function.
}
\usage{
evalDist(obse, pred, tS = NULL, obseTrain = NULL, predTrain = NULL, output = c("test", "train"))
}
\arguments{
  \item{obse}{Matrix. Community matrix (species by sites) with observed values.}
  \item{pred}{Matrix. Community matrix (species by sites) with predicted values.}
  \item{tS}{Optional logical vector, indicating those sites that will be used for training (TRUE) and for testing (FALSE).}
  \item{obseTrain}{Optional Matrix. If tS is provided obseTrain is ignored. If tS is missing obseTrain should be provided and will be the community matrix of observed values used for train the model.}
  \item{predTrain}{Optional Matrix. If tS is provided predTrain is ignored. If tS is missing predTrain should be provided and will be the community matrix of predicted values on the climate data used to train the model.}
  \item{output}{Logical value, indicating if the output should report the testing or the training results.}
}
\details{All the community matrices (obse, pred, obseTrain and predTrain) should have the same number of species. The number of sites should be the same for the training and testing datasets (nrow(obse) == nrow(pred) and nrow(obseTrain) == nrow(predTrain)).}
\value{Dataframe. It contain 8 columns: taxon, prev, auc, trmin, tss, sens, spec, and brier. It contain as many rows as species in the community matrices. taxon indicate the name of the species to be evaluated, prev indicate the prevalence of this species in the corresponding dataset (training or testing), auc show the Area Under the receiver corresponding Curve (AUC), trmin is the threshold that minimize the difference between True Positive Ratio (TPR) and True Negative Ratio (TNR) in the training dataset, tss is the True Skill Statistic (TSS), sens is the Sensitivity (SENS), spec is the Specificity (SPEC), brier is the Brier Score.
}
\author{Diego Nieto Lugilde}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (obse, pred, tS = NULL, obseTrain = NULL, predTrain = NULL, 
    output = c("test", "train")) 
{
    require(dismo)
    res <- vector(mode = "list", length = 7)
    names(res) <- c("taxon", "prev", "auc", "trmin", "tss", "sens", 
        "spec")
    for (i in 1:ncol(obse)) {
        if (is.null(tS)) {
            pObse <- which(obse[, i] == 1)
            aObse <- which(obse[, i] == 0)
            pTrainObse <- which(obseTrain[, i] == 1)
            aTrainObse <- which(obseTrain[, i] == 0)
            pTest <- pred[pObse, i]
            aTest <- pred[aObse, i]
            pTrain <- predTrain[pTrainObse, i]
            aTrain <- predTrain[aTrainObse, i]
        }
        else {
            pObse <- which(obse[, i] == 1 & !tS)
            aObse <- which(obse[, i] == 0 & !tS)
            pTrainObse <- which(obse[, i] == 1 & tS)
            aTrainObse <- which(obse[, i] == 0 & tS)
            pTest <- pred[pObse, i]
            aTest <- pred[aObse, i]
            pTrain <- pred[pTrainObse, i]
            aTrain <- pred[aTrainObse, i]
        }
        la <- length(pTest)
        lb <- length(aTest)
        lc <- length(pTrain)
        ld <- length(aTrain)
        le <- min(la, lb, lc, ld)
        res[["taxon"]][i] <- colnames(obse)[i]
        if (le != 0) {
            testEval <- evaluate(p = pTest, a = aTest)
            trainEval <- evaluate(p = pTrain, a = aTrain)
            if (output == "test") {
                res[["auc"]][i] <- testEval@auc
                res[["trmin"]][i] <- trmin <- trainEval@t[which.min(abs(trainEval@TPR - 
                  trainEval@TNR))]
                ind <- which.min(abs(testEval@t - trmin))
                res[["prev"]][i] <- testEval@prevalence[ind]
                res[["sens"]][i] <- testEval@TPR[ind]
                res[["spec"]][i] <- testEval@TNR[ind]
                res[["tss"]][i] <- testEval@TPR[ind] + testEval@TNR[ind] - 
                  1
            }
            if (output == "train") {
                res[["auc"]][i] <- trainEval@auc
                res[["trmin"]][i] <- trmin <- trainEval@t[which.min(abs(trainEval@TPR - 
                  trainEval@TNR))]
                ind <- which.min(abs(trainEval@t - trmin))
                res[["prev"]][i] <- trainEval@prevalence[ind]
                res[["sens"]][i] <- trainEval@TPR[ind]
                res[["spec"]][i] <- trainEval@TNR[ind]
                res[["tss"]][i] <- trainEval@TPR[ind] + trainEval@TNR[ind] - 
                  1
            }
        }
        else {
            res[["auc"]][i] <- NA
            res[["trmin"]][i] <- NA
            res[["prev"]][i] <- NA
            res[["sens"]][i] <- NA
            res[["spec"]][i] <- NA
            res[["tss"]][i] <- NA
        }
    }
    res <- data.frame(res)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
