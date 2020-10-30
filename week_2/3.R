#3.1
lamda = 20

#3.1.1
n1 = qpois(0.9,lamda)
n1

#3.1.2
a = qpois(1-0.9,lamda)
if(ppois(a,lamda) > 0.1){
  n2 = a-1
} else {
  n2 = a
}

#3.1.3
a = qpois(0.25,lamda)
b = qpois(0.75,lamda)
n3 = b-a

#3.1.4
n4 = ppois(16,20)

#3.1.5
# the wanted value is the same as the expected value of 
# geometric distribution with p = n4
n5 = 1/n4

#3.1.6
n6 = qgeom(0.75,n4)


