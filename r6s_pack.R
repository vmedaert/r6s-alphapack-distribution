library("purrr")

start <- 0.025
win_inc <- 0.025
loss_inc <- 0.015
win_chance <- 0.5
loss_chance <- 1 - win_chance
sample_size <- 100000
plot_range <- 1:30

rpack <- function(n=1){
  if (n > 1){
    return(replicate(n, rpack()))
  }
  attempts <- 1
  chance <- start
  won <- rbernoulli(1, win_chance)
  while (!won || !rbernoulli(1, chance)){
    attempts <- attempts + 1
    if (won){
      chance <- chance + win_inc
    }else{
      chance <- chance + loss_inc
    }
    won <- rbernoulli(1, win_chance)
  }
  return(attempts)
}

dpack <- function(x){
  if (length(x) > 1){
    return(sapply(x, dpack))
  }
  chance <- start
  output <- 1
  if (x < 1){
    return(0)
  }
  while (x > 1){
    output <- output * (1 - chance * win_chance)
    chance <- chance + (win_chance * win_inc + loss_chance * loss_inc)
    x <- x - 1
  }
  return(output * chance * win_chance)
}

ppack <- function(q, lower.tail=TRUE){
  if (lower.tail){
    return(sum(dpack(1:q)))
  }else{
    return(1 - ppack(q))
  }
}

dplot <- barplot(dpack(plot_range), names.arg=plot_range, main="attempts needed for alpha pack")

sample_chance <- flatten_dbl(as.data.frame(table(rpack(sample_size)))[2])/sample_size
sample_plot <- barplot(sample_chance[plot_range], names.arg=plot_range, main="attempts needed for alpha pack (sampled)")