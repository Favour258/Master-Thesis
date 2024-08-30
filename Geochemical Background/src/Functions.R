#Define calculated distribution function (CDF)

Caldist <- function(data) {
    data <- na.omit (data)
    
    min_val <- min(data)
    median_val <- median(data)
    reduceddata <- data[data>=min_val&data<=median_val]
    dist_val <- median_val - reduceddata
    mirrorvalues <- median_val + dist_val
    backgroundvalue <- mean(mirrorvalues) + 2*sd(mirrorvalues)
    
    
    return(backgroundvalue)
}

#Define Mean Absolut Deviation (MAD) 
MAD <- function (data) {
    data <- na.omit (data) 
    real_median <- median (data)
    abs_deviation <- abs(data - real_median)
    median_abs <- median(abs_deviation)
    madd <- 1.48*median_abs
    backgroundvalue <- madd + 2*real_median
    
    return(backgroundvalue)
    
}

#Using the 2 sigma iteration method####
iterative_2sigma <- function(data) {
    
    data <- na.omit(data)
    
    repeat {
        
        mean_val <- mean(data)
        sd_val <- sd(data)
        
        # Define the 2-sigma range
        lower_limit <- mean_val - 2 * sd_val
        upper_limit <- mean_val + 2 * sd_val
        
        # Filter data to keep only values within the 2-sigma range
        filtered_data <- data[data >= lower_limit & data <= upper_limit]
        
        # Check if the length of the filtered data is equal to the original data length
        if (length(filtered_data) == length(data)) {
            break
        }
        
        # Update the data with the filtered data for the next iteration
        data <- filtered_data
    }
    
    return(paste("Backgroundvalue:", mean(data)+2*sd(data)))
}

################################################################################
#Heavymetals 
Pb_bc <- Caldist(Background$Pb)
Zn_bc <- Caldist(Background$Zn)
Mn_bc <- Caldist(Background$Mn)
Fe_bc <- Caldist(Background$Fe)
Cd_bc <- Caldist(Background$Cd)
Cu_bc <- Caldist(Background$Cu)

Pb_mad <- MAD(Background$Pb)
Zn_mad <- MAD(Background$Zn)
Cd_mad <- MAD(Background$Cd)
Cu_mad <- MAD(Background$Cu)

iterative_2sigma(Background$Pb)
iterative_2sigma(Background$Zn)
iterative_2sigma(Background$Cu)
iterative_2sigma(Background$Fe) 

#Heavymetals
As_bc <- Caldist(BackgroundpXRF$As)
Ni_bc <- Caldist(BackgroundpXRF$Ni)
Cr_bc <- Caldist(BackgroundpXRF$Cr)

As_mad <- MAD(BackgroundpXRF$As)
Cr_mad <- MAD(BackgroundpXRF$Cr)
Ni_mad <- MAD(BackgroundpXRF$Ni)

iterative_2sigma(BackgroundpXRF$As)
iterative_2sigma(BackgroundpXRF$Ni)
iterative_2sigma(BackgroundpXRF$Cr)


