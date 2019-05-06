library(lattice)
rm(list=ls())
set.seed(10)

work_dir <- "D:/School Stuff/Math/Stats 141A/Data Files/HW 3"
setwd(work_dir)
property_file = read.csv("D:/School Stuff/Math/Stats 141A/Data Files/HW 3/properties.csv")
color_file = read.csv("D:/School Stuff/Math/Stats 141A/Data Files/HW 3/color_combos.csv")
property = property_file$Index
rent = property_file$Rent

########
#1.1

simulate_monopoly = function(n, d, property_vector, rent_vector)
{
  cc = c("GO", "J", rep(0, 14))
  ch = c("GO", "J", "C1", "E3", "H2", "R1", "RR", "RR", "UT", "B3", rep(0, 6))
  
  shuffled_cc = sample(cc)
  shuffled_ch = sample(ch)
  
  die1 = sample(1:d, n, replace = TRUE)
  die2 = sample(1:d, n, replace = TRUE)
  die_roll = die1 + die2
  doubles = (die1 == die2)
  
  initial_position = c(0)
  moves = c(die1 + die2)
  position_vector = cumsum(moves) %% 40
  
  for(i in 3:n)
  {
    if(all(doubles[(i-2):i]))
    {
      position_vector[i] = 10
      doubles[i] = FALSE
    }
    else
    { 
      position_vector[i] = position_vector[i-1] + die_roll[i]
    }
    position_vector = position_vector %% 40
  }
  
  for(i in 1:n)
  {
    if(position_vector[i] == 2) #CC1
    {
      if(shuffled_cc[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 38) %% 40 
      }
      if(shuffled_cc[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] + 8) %% 40
      }
      shuffled_cc = c(shuffled_cc[-1], shuffled_cc[1])
    }
    
    if(position_vector[i] == 7) #CH1
    {
      if(shuffled_ch[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 33) %% 40
      }
      if(shuffled_ch[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] + 3) %% 40
      }
      if(shuffled_ch[1] == "C1")
      {
        position_vector[i:n] = (position_vector[i:n] + 4) %% 40
      }
      if(shuffled_ch[1] == "E3")
      {
        position_vector[i:n] = (position_vector[i:n] + 17) %% 40
      }
      if(shuffled_ch[1] == "H2")
      {
        position_vector[i:n] = (position_vector[i:n] + 32) %% 40
      }
      if(shuffled_ch[1] == "R1")
      {
        position_vector[i:n] = (position_vector[i:n] + 38) %% 40
      }
      if(shuffled_ch[1] == "RR")
      {
        position_vector[i:n] = (position_vector[i:n] + 8) %% 40
      }
      if(shuffled_ch[1] == "UT")
      {
        position_vector[i:n] = (position_vector[i:n] + 5) %% 40
      }
      if(shuffled_ch[1] == "B3")
      {
        position_vector[i:n] = (position_vector[i:n] - 3) %% 40
      }
      shuffled_ch = c(shuffled_ch[-1], shuffled_ch[1])
    }
    
    if(position_vector[i] == 17) #CC2
    {
      if(shuffled_cc[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 23) %% 40
      }
      if(shuffled_cc[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 7) %% 40
      }
      shuffled_cc = c(shuffled_cc[-1], shuffled_cc[1])
    }
    
    if(position_vector[i] == 22) #CH2
    {
      if(shuffled_ch[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 18) %% 40
      }
      if(shuffled_ch[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 12) %% 40
      }
      if(shuffled_ch[1] == "C1")
      {
        position_vector[i:n] = (position_vector[i:n] + 29) %% 40
      }
      if(shuffled_ch[1] == "E3")
      {
        position_vector[i:n] = (position_vector[i:n] + 2) %% 40
      }
      if(shuffled_ch[1] == "H2")
      {
        position_vector[i:n] = (position_vector[i:n] + 17) %% 40
      }
      if(shuffled_ch[1] == "R1")
      {
        position_vector[i:n] = (position_vector[i:n] + 23) %% 40
      }
      if(shuffled_ch[1] == "RR")
      {
        position_vector[i:n] = (position_vector[i:n] + 3) %% 40
      }
      if(shuffled_ch[1] == "UT")
      {
        position_vector[i:n] = (position_vector[i:n] + 6) %% 40
      }
      if(shuffled_ch[1] == "B3")
      {
        position_vector[i:n] = (position_vector[i:n] - 3) %% 40
      }
      shuffled_ch = c(shuffled_ch[-1], shuffled_ch[1])
    }
    
    if(position_vector[i] == 30) #G2J
    {
      position_vector[i:n] = (position_vector[i:n] - 20) %% 40
    }
    
    if(position_vector[i] == 33) #CC3
    {
      if(shuffled_cc[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 7) %% 40
      }
      if(shuffled_cc[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 23) %% 40
      }
      shuffled_cc = c(shuffled_cc[-1], shuffled_cc[1])
    }
    
    if(position_vector[i] == 36) #CH3
    {
      if(shuffled_ch[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 4) %% 40
      }
      if(shuffled_ch[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 26) %% 40
      }
      if(shuffled_ch[1] == "C1")
      {
        position_vector[i:n] = (position_vector[i:n] + 15) %% 40
      }
      if(shuffled_ch[1] == "E3")
      {
        position_vector[i:n] = (position_vector[i:n] + 28) %% 40
      }
      if(shuffled_ch[1] == "H2")
      {
        position_vector[i:n] = (position_vector[i:n] + 3) %% 40
      }
      if(shuffled_ch[1] == "R1")
      {
        position_vector[i:n] = (position_vector[i:n] + 9) %% 40
      }
      if(shuffled_ch[1] == "RR")
      {
        position_vector[i:n] = (position_vector[i:n] + 9) %% 40
      }
      if(shuffled_ch[1] == "UT")
      {
        position_vector[i:n] = (position_vector[i:n] + 16) %% 40
      }
      if(shuffled_ch[1] == "B3")
      {
        position_vector[i:n] = (position_vector[i:n] - 3) %% 40
      }
      shuffled_ch = c(shuffled_ch[-1], shuffled_ch[1])
    }
  }
  final_vector = c(initial_position, position_vector)
  final_vector
}

##########
#1.2

estimate_monopoly = function(n, d)
{
  simulation = simulate_monopoly(n, d)
  freq_table = table(simulation) / n
}

three_die = estimate_monopoly(10000, 3)
four_die = estimate_monopoly(1000, 4)
five_die = estimate_monopoly(1000,5)
six_die = estimate_monopoly(1000, 6)

top_three = head(sort(three_die, decreasing = TRUE), 3)
top_four = head(sort(four_die, decreasing = TRUE), 3)
top_five = head(sort(five_die, decreasing = TRUE), 3)
top_six = head(sort(six_die, decreasing = TRUE), 3)

par(mfrow = c(2, 2))
plot(three_die, xlab = "Position", ylab = "Frequency", main = "Three Sided Die Frequency Distribution ")
plot(four_die, xlab = "Position", ylab = "Frequency", main = "Four Sided Die Frequency Distribution ")
plot(five_die, xlab = "Position", ylab = "Frequency", main = "Five Sided Die Frequency Distribution ")
plot(six_die, xlab = "Position", ylab = "Frequency", main = "Six Sided Die Frequency Distribution ")
dev.off()

##########
#1.3

sd_sim = replicate(1000, estimate_monopoly(10000, 6))
sd = sd(sd_sim[11,])

###########
#2.1

simulate_monopoly2 = function(n, d, property, rent)
{
  cc = c("GO", "J", rep(0, 14))
  ch = c("GO", "J", "C1", "E3", "H2", "R1", "RR", "RR", "UT", "B3", rep(0, 6))
  
  shuffled_cc = sample(cc)
  shuffled_ch = sample(ch)
  
  die1 = sample(1:d, n, replace = TRUE)
  die2 = sample(1:d, n, replace = TRUE)
  die_roll = die1 + die2
  doubles = (die1 == die2)
  
  initial_position = c(0)
  wealth_vector = c(200, rep(0, n))
  
  moves = c(die1 + die2)
  position_vector = cumsum(moves) %% 40
  
  for(i in 3:n)
  {
    if(all(doubles[(i-2):i]))
    {
      position_vector[i] = 10
      doubles[i] = FALSE
    }
    else
    { 
      position_vector[i] = position_vector[i-1] + die_roll[i]
    }
    position_vector = position_vector %% 40
  }
  
  for(i in 1:n)
  {
    if(position_vector[i] == 2) #CC1
    {
      if(shuffled_cc[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 38) %% 40 
      }
      if(shuffled_cc[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] + 8) %% 40
      }
      shuffled_cc = c(shuffled_cc[-1], shuffled_cc[1])
    }
    
    if(position_vector[i] == 7) #CH1
    {
      if(shuffled_ch[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 33) %% 40
      }
      if(shuffled_ch[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] + 3) %% 40
      }
      if(shuffled_ch[1] == "C1")
      {
        position_vector[i:n] = (position_vector[i:n] + 4) %% 40
      }
      if(shuffled_ch[1] == "E3")
      {
        position_vector[i:n] = (position_vector[i:n] + 17) %% 40
      }
      if(shuffled_ch[1] == "H2")
      {
        position_vector[i:n] = (position_vector[i:n] + 32) %% 40
      }
      if(shuffled_ch[1] == "R1")
      {
        position_vector[i:n] = (position_vector[i:n] + 38) %% 40
      }
      if(shuffled_ch[1] == "RR")
      {
        position_vector[i:n] = (position_vector[i:n] + 8) %% 40
      }
      if(shuffled_ch[1] == "UT")
      {
        position_vector[i:n] = (position_vector[i:n] + 5) %% 40
      }
      if(shuffled_ch[1] == "B3")
      {
        position_vector[i:n] = (position_vector[i:n] - 3) %% 40
      }
      shuffled_ch = c(shuffled_ch[-1], shuffled_ch[1])
    }
    
    if(position_vector[i] == 17) #CC2
    {
      if(shuffled_cc[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 23) %% 40
      }
      if(shuffled_cc[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 7) %% 40
      }
      shuffled_cc = c(shuffled_cc[-1], shuffled_cc[1])
    }
    
    if(position_vector[i] == 22) #CH2
    {
      if(shuffled_ch[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 18) %% 40
      }
      if(shuffled_ch[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 12) %% 40
      }
      if(shuffled_ch[1] == "C1")
      {
        position_vector[i:n] = (position_vector[i:n] + 29) %% 40
      }
      if(shuffled_ch[1] == "E3")
      {
        position_vector[i:n] = (position_vector[i:n] + 2) %% 40
      }
      if(shuffled_ch[1] == "H2")
      {
        position_vector[i:n] = (position_vector[i:n] + 17) %% 40
      }
      if(shuffled_ch[1] == "R1")
      {
        position_vector[i:n] = (position_vector[i:n] + 23) %% 40
      }
      if(shuffled_ch[1] == "RR")
      {
        position_vector[i:n] = (position_vector[i:n] + 3) %% 40
      }
      if(shuffled_ch[1] == "UT")
      {
        position_vector[i:n] = (position_vector[i:n] + 6) %% 40
      }
      if(shuffled_ch[1] == "B3")
      {
        position_vector[i:n] = (position_vector[i:n] - 3) %% 40
      }
      shuffled_ch = c(shuffled_ch[-1], shuffled_ch[1])
    }
    
    if(position_vector[i] == 30) #G2J
    {
      position_vector[i:n] = (position_vector[i:n] - 20) %% 40
    }
    
    if(position_vector[i] == 33) #CC3
    {
      if(shuffled_cc[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 7) %% 40
      }
      if(shuffled_cc[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 23) %% 40
      }
      shuffled_cc = c(shuffled_cc[-1], shuffled_cc[1])
    }
    
    if(position_vector[i] == 36) #CH3
    {
      if(shuffled_ch[1] == "GO")
      {
        position_vector[i:n] = (position_vector[i:n] + 4) %% 40
      }
      if(shuffled_ch[1] == "J")
      {
        position_vector[i:n] = (position_vector[i:n] - 26) %% 40
      }
      if(shuffled_ch[1] == "C1")
      {
        position_vector[i:n] = (position_vector[i:n] + 15) %% 40
      }
      if(shuffled_ch[1] == "E3")
      {
        position_vector[i:n] = (position_vector[i:n] + 28) %% 40
      }
      if(shuffled_ch[1] == "H2")
      {
        position_vector[i:n] = (position_vector[i:n] + 3) %% 40
      }
      if(shuffled_ch[1] == "R1")
      {
        position_vector[i:n] = (position_vector[i:n] + 9) %% 40
      }
      if(shuffled_ch[1] == "RR")
      {
        position_vector[i:n] = (position_vector[i:n] + 9) %% 40
      }
      if(shuffled_ch[1] == "UT")
      {
        position_vector[i:n] = (position_vector[i:n] + 16) %% 40
      }
      if(shuffled_ch[1] == "B3")
      {
        position_vector[i:n] = (position_vector[i:n] - 3) %% 40
      }
      shuffled_ch = c(shuffled_ch[-1], shuffled_ch[1])
    }
  }
  final_vector = c(initial_position, position_vector)
  
  for(i in 2:(n+1))
  {
    if(final_vector[i] < final_vector[i-1])
    {
      if(final_vector[i] == 10)
      {
        wealth_vector[i] = wealth_vector[i]
      }
      else
      {
        wealth_vector[i] = wealth_vector[i] + 200
      }
    }
  }
  
  for(i in 2:(n+1))
  {
    if(final_vector[i] == 4)
    {
      wealth_vector[i] = wealth_vector[i] - 200
    }
    if(final_vector[i] == 38)
    {
      wealth_vector[i] = wealth_vector[i] - 100
    }
  }
  
  for(i in 2:(n+1))
  {
    if(final_vector[i] %in% property)
    {
      which_property = which(property == final_vector[i])
      which_rent = rent[which_property]
      wealth_vector[i] = wealth_vector[i] - which_rent
    }
  }
  wealth_vector
}

#########
#2.2

color_monopoly = function(n, k, color1)
{
  subdata = subset(property_file, Color == color1)
  replicate(k, sum(simulate_monopoly2(n, d = 6, subdata$Index, subdata$Rent)))
}

x = data.frame(sapply(levels(property_file$Color), function(x) color_monopoly(100, 1000, x)))
sapply(x, summary)

plot_legends = list(space = "right", text = list(levels(property_file$Color)),
                    lines = list(col = c('Blue', 'Green', 'Light Blue', 'Orange', 'Pink', 'Purple', 'Red', 'Yellow'),
                    lwd = 3), cex = 0.8)

densityplot(~ Blue + Green + Light.Blue + Orange + Pink + Purple + Red + Yellow, data = x, plot.points = FALSE, 
            xlim = c(-16000, 6000),xlab = "Wealth Sum", ylab = "Frequency", main = "Frequency of Wealth", 
            col = c('Blue', 'Green', 'Light Blue', 'Orange', 'Pink', 'Purple', 'Red', 'Yellow'), key = plot_legends)
