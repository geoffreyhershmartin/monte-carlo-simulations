# 1 Next event is an arrival
  # a There is no other customer at the gas station apart from the one that has triggered the next event
  # b There is another customer
# 2 Next event is a departure
  # a There is no other customer at the gas station apart from the one that has triggered the next event
  # b There is another customer

# 1a Queue length stays at zero -> set next departure time from exponential distribution
# 1b Increment queue length

# 2a Queue length stays at zero -> set next departure time to Infinity
# 2b Decrement queue length -> set next departure time from exponential departure

# Set parameters

lambda <- 0.25 # Since 15/60 = 0.25, set lambda =0.25
es <- 3  #expected service time 
tfinal <- 1e6 # a large value 

# Determine arrival time
nCust <- rpois(1,lambda * tfinal) #number of customers

Arr <- sort(runif(nCust, 0, tfinal)) # determine arriving time 

# Initiliztions 
qLength <- 0 # Initially there is nobody
nextCustToArr <- 1 # labeling customers 
nextArr <- Arr[1] # arrival time of the 1st customer
nextDept <- Inf # this indicates that there is no customer waiting 
t <- 0.0 # t  is the next time something happens.

wq <- 0 # initializing the cumulative waiting time summber over all customers 
tqChange <- 0 # initializing time of the prvious change in the queue length

#  Simulate the queueing process.
while(nextCustToArr <= nCust){
  if (nextArr < nextDept){ 
    t <- nextArr # set the arrival time to time  
    if (is.infinite(nextDept)){ # Case 1a: there is no other customer at the gas station && next event is an arrival
      
      nextDept <- t + rexp(1, 1/es) # new arrival immediately after procedes to the pump 
      
    }else {# cae 1b: there is another customer && next event is an arrival
      wq <- wq + qLength * (t - tqChange)  # cumulating the waiting time 
      tqChange <- t #assign the time in 
      qLength <- qLength +1 # Increment the queue length
    }
    nextCustToArr <- nextCustToArr +1 # increment customer
    nextArr <- Arr[nextCustToArr] # get the time when the next customer comes 
    
  }else{
    
    t <- nextDept # assign the departure time to time
    if (qLength == 0){ # case 2a: next event is a departure && there is no other customer at the gas station
      nextDept <- Inf  # set the next departure to infinity
      
    }else { #2b # next event is a departure && there is another customer
      wq <- wq + qLength * (t - tqChange)  # cumulate waiting time
      tqChange <- t # store the time of the change in queue length
      qLength <- qLength -1  # decrement queue length
      nextDept <- nextDept + rexp(1, 1/es) # set the departure tiem 
    }
  }
}

mwq <- wq/nCust # calculating the average waiting time 
cat("Average Waiting Time:", mwq, "\n") # print 