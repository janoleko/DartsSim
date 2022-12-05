darts_score = function(x,y){
  # convert x and y to radial coordinates
  r = sqrt(x^2 + y^2) # radius is euklidean distance to origin
  if(x > 0 & y >= 0){a = atan(y/x)}
  if(x < 0 & y >= 0){a = atan(y/x) + pi}
  if(x < 0 & y < 0){a = atan(y/x) + pi}
  if(x > 0 & y < 0){a = atan(y/x) + 2*pi}
  if(x == 0 & y >= 0){a = pi/2}
  if(x == 0 & y < 0){a = 3/2*pi}
  a = a*180/pi


  # actually getting the score:

  if(r < 0.635){return = 50} # bullseye inner
  if(r >= 0.635 & r < 1.59){return = 25} # bullseye outer

  else{ # rest
    # getting the slice
    if(a >= 9 & a < 27){value = 13}
    if(a >= 27 & a < 45){value = 4}
    if(a >= 45 & a < 63){value = 18}
    if(a >= 63 & a < 81){value = 1}
    if(a >= 81 & a < 99){value = 20}
    if(a >= 99 & a < 117){value = 5}
    if(a >= 117 & a < 135){value = 12}
    if(a >= 135 & a < 153){value = 9}
    if(a >= 153 & a < 171){value = 14}
    if(a >= 171 & a < 189){value = 11}
    if(a >= 189 & a < 207){value = 8}
    if(a >= 207 & a < 225){value = 16}
    if(a >= 225 & a < 243){value = 7}
    if(a >= 243 & a < 261){ value = 19}
    if(a >= 261 & a < 279){value = 3}
    if(a >= 279 & a < 297){value = 17}
    if(a >= 297 & a < 315){value = 2}
    if(a >= 315 & a < 333){value = 15}
    if(a >= 333 & a < 351){value = 10}
    if(a >= 151 | a < 9){value = 6}

    # checking whether multiplication is needed (or outside)
    if(r < 9.9){return = value}
    if(r >= 9.9 & r < 10.7){return = 3*value}
    if(r >= 10.7 & r < 16.2){return = value}
    if(r >= 16.2 & r < 17){return = 2*value}
    if(r >= 17){return = 0}
  }
  return(return)
}

darts_sim_norm = function(mean = c(0,0), sd = c(10,10), runs = 100){
  points = numeric(3*runs)
  for (i in 1:(3*runs)){
    x = rnorm(1, mean[1], sd[1])
    y = rnorm(1, mean[2], sd[2])
    points[i] = darts_score(x,y)
  }
  return(sum(points)/runs)
}

darts_sim_unif = function(runs = 100){
  points = numeric(3*runs)
  for (i in 1:(3*runs)){
    r = runif(1, 0, 17)
    a = runif(1, 0, 360)
    points[i] = darts_score(x,y)
  }
  return(sum(points)/runs)
}

darts_sim_norm(runs = 1000)
darts_sim_norm(mean = c(0, 10.3), sd = c(1,1), runs = 1000)


