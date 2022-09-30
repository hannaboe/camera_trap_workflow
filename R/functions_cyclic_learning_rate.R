##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## FUNCTIONS FOR CYCLING LEARNING RATE

## The functions are from a blogpost: http://thecooldata.com/2019/01/learning-rate-finder-with-cifar10-keras-r/
## And have been translated from python to R
## The original is on https://github.com/bckenstler/CLR/blob/master/clr_callback.py


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


Cyclic_LR <- function(iteration=1:32000, base_lr=1e-5, max_lr=1e-3, step_size=2000, mode='triangular', gamma=1, scale_fn=NULL, scale_mode='cycle'){ # translated from python to R, original at: https://github.com/bckenstler/CLR/blob/master/clr_callback.py # This callback implements a cyclical learning rate policy (CLR). # The method cycles the learning rate between two boundaries with # some constant frequency, as detailed in this paper (https://arxiv.org/abs/1506.01186). # The amplitude of the cycle can be scaled on a per-iteration or per-cycle basis. # This class has three built-in policies, as put forth in the paper. # - "triangular": A basic triangular cycle w/ no amplitude scaling. # - "triangular2": A basic triangular cycle that scales initial amplitude by half each cycle. # - "exp_range": A cycle that scales initial amplitude by gamma**(cycle iterations) at each cycle iteration. # - "sinus": A sinusoidal form cycle # # Example # > clr <- Cyclic_LR(base_lr=0.001, max_lr=0.006, step_size=2000, mode='triangular', num_iterations=20000) # > plot(clr, cex=0.2)
  
  # Class also supports custom scaling functions with function output max value of 1:
  # > clr_fn <- function(x) 1/x # > clr <- Cyclic_LR(base_lr=0.001, max_lr=0.006, step_size=400, # scale_fn=clr_fn, scale_mode='cycle', num_iterations=20000) # > plot(clr, cex=0.2)
  
  # # Arguments
  #   iteration:
  #       if is a number:
  #           id of the iteration where: max iteration = epochs * (samples/batch)
  #       if "iteration" is a vector i.e.: iteration=1:10000:
  #           returns the whole sequence of lr as a vector
  #   base_lr: initial learning rate which is the
  #       lower boundary in the cycle.
  #   max_lr: upper boundary in the cycle. Functionally,
  #       it defines the cycle amplitude (max_lr - base_lr).
  #       The lr at any cycle is the sum of base_lr
  #       and some scaling of the amplitude; therefore 
  #       max_lr may not actually be reached depending on
  #       scaling function.
  #   step_size: number of training iterations per
  #       half cycle. Authors suggest setting step_size
  #       2-8 x training iterations in epoch.
  #   mode: one of {triangular, triangular2, exp_range, sinus}.
  #       Default 'triangular'.
  #       Values correspond to policies detailed above.
  #       If scale_fn is not None, this argument is ignored.
  #   gamma: constant in 'exp_range' scaling function:
  #       gamma**(cycle iterations)
  #   scale_fn: Custom scaling policy defined by a single
  #       argument lambda function, where 
  #       0 <= scale_fn(x) <= 1 for all x >= 0.
  #       mode paramater is ignored 
  #   scale_mode: {'cycle', 'iterations'}.
  #       Defines whether scale_fn is evaluated on 
  #       cycle number or cycle iterations (training
  #       iterations since start of cycle). Default is 'cycle'.
  
  ########
  if(is.null(scale_fn)==TRUE){
    if(mode=='triangular'){scale_fn <- function(x) 1; scale_mode <- 'cycle';}
    if(mode=='triangular2'){scale_fn <- function(x) 1/(2^(x-1)); scale_mode <- 'cycle';}
    if(mode=='exp_range'){scale_fn <- function(x) gamma^(x); scale_mode <- 'iterations';}
    if(mode=='sinus'){scale_fn <- function(x) 0.5*(1+sin(x*pi/2)); scale_mode <- 'cycle';}
  }
  lr <- list()
  if(is.vector(iteration)==TRUE){
    for(iter in iteration){
      cycle <- floor(1 + (iter / (2*step_size)))
      x2 <- abs(iter/step_size-2 * cycle+1)
      if(scale_mode=='cycle') x <- cycle
      if(scale_mode=='iterations') x <- iter
      lr[[iter]] <- base_lr + (max_lr-base_lr) * max(0,(1-x2)) * scale_fn(x)
    }
  }
  lr <- do.call("rbind",lr)
  return(as.vector(lr))
}
####################


## R6 function 
LogMetrics <- R6::R6Class("LogMetrics",
                          inherit = KerasCallback,
                          public = list(
                            loss = NULL,
                            acc = NULL,
                            on_batch_end = function(batch, logs=list()) {
                              self$loss <- c(self$loss, logs[["loss"]])
                              self$acc <- c(self$acc, logs[["acc"]])
                            }
                          ),
                          lock_objects = FALSE)

## function to set iteration to zero and clear history (at the begininng of training)
callback_lr_init <- function(logs){
  iter <<- 0
  lr_hist <<- c()
  iter_hist <<- c()
}

## function to set the learning rate at the beginning of each batch
callback_lr_set <- function(batch, logs){
  iter <<- iter + 1
  LR <- l_rate[iter] # if number of iterations > l_rate values, make LR constant to last value
  if(is.na(LR)) LR <- l_rate[length(l_rate)]
  k_set_value(model$optimizer$lr, LR)
}

## function to log the learning rate and iteration number
callback_lr_log <- function(batch, logs){
  lr_hist <<- c(lr_hist, k_get_value(model$optimizer$lr))
  iter_hist <<- c(iter_hist, k_get_value(model$optimizer$iterations))
}
