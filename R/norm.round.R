# a different version of round, shamelessly stolen from here: 
# https://stackoverflow.com/questions/12688717/round-up-from-5-in-r
# this deals with the fact that the standard version of round in r 
# sometimes rounds 0.5 up and sometimes down, for good reasons
# but reasons that do not apply in our data
norm.round = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
