library(data.table)
rankall <- function(outcome, num = "best") { 
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ##Changing long column names to shorter ones
        colnames(data)[11] <- "heart_attack"
        colnames(data)[17] <- "heart_failure"
        colnames(data)[23] <- "pneumonia"
        ##taking the space off in outcome variable
        spl_out <- strsplit(outcome, " ")
        spl_out <- unlist(spl_out)
        outcome_s <- paste(spl_out, collapse="_")
        
        ##Check that outcome is valid
        st <- split(data, data$State)
        st_list <- names(st)
        ot_list <- c("heart_attack", "heart_failure", "pneumonia")
        if (!any(outcome_s == ot_list)){
                stop("invalid outcome")
        }
        ##End of test
        
        frame_st <- data.frame(Reduce(rbind, st))
        keeps <- c(outcome_s, "Hospital.Name", "State")
        ot_hp <- frame_st[, keeps, drop=FALSE]
        ot_hp[,1] <- as.numeric(ot_hp[,1])
        good <- complete.cases(ot_hp)
        ot_hp <- ot_hp[good,]
        ##All good until here
        
        ##result <- data.frame()
        ##names(result) <- c("hospital", "state")
        count <- 0
        x <- character(0)
        y <- character(0)
        for (state in st_list) {
                count <- count + 1
                ot_hp_s <- ot_hp[ot_hp$State == state,]
                if (num == "best"){
                        num <- 1
                }else if (num == "worst"){
                        num <- length(st[[count]][[1]])
                }else if (num > length(st[[count]][[1]])){
                        x[count] <- NA
                        y[count] <- state
                        next
                }else{
                        names(ot_hp_s) <- c("Outcome","Hospital","State")
                        ot_hp_table <- data.table(ot_hp_s) 
                        setkey(ot_hp_table, State, Outcome, Hospital)
                        ot_hp_table[, Rank := 1:.N, by = State]
                        setkey(ot_hp_table, State, Rank)
                        x[count] <- ot_hp_table[[2]][[num]] 
                        y[count] <- state 
                }
                     
        }
        data.frame(hospital = x, state = y)        
}

