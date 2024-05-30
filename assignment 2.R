name<-'Akshath Srinivas'
liuid<-'akssr921'
library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)

#sheldon_game
sheldon_game<-function(player1, player2){
  choices<-c( 'rock', 'paper', 'scissors', 'lizard', 'spock')
  if ((player1 %in% choices)&(player2 %in% choices)){
  entries<-paste(player1,'and', player2)
  possibilities<-c('scissors and rock','scissors and paper','scissors and lizard','scissors and spock','scissors and scissors',
        'rock and paper','rock and scissor','rock and lizard','rock and spock','rock and rock',
        'paper and rock','paper and paper','paper and scissors','paper and lizard','paper and spock',
        'lizard and rock','lizard and paper','lizard and scissor', 'lizard and spock', 'lizard and lizard',
        'spock and rock', 'spock and paper', 'spock and scissor', 'spock and lizard', 'spock and spock')
  d<-match(entries,possibilities) # match function compares the two arguments and  returns the index of the element where two arguments are matching
  result<-switch(d,'player 2 wins!' ,'player 1 wins!','player 1 wins!','player 2 wins!','draw!',
             'player 2 wins!','player 1 wins!','player 1 wins!','player 2 wins!','draw!',
             'player 1 wins!','draw!','player 2 wins!','player 2 wins!','player 1 wins!',
             'player 2 wins!','player 1 wins!','player 2 wins!', 'player 1 wins!', 'draw!',
             'player 1 wins!', 'player 2 wins!', 'player 1 wins!', 'player 2 wins!', 'draw!')
  }
  else{
    stop()
  }
  return(result)
}
sheldon_game("rock", "paper")
mark_my_assignment(tasks="sheldon_game")


#my_moving_median
my_moving_median<-function(x,n,...){
  if((is.atomic(x) || is.list(x)) && length(n)==1){
    b<-length(x)-n
    moving_median<-c()
    for(i in 1:b){
      t<-i+n
      moving_median<-c(moving_median,median(x[i:t],...))
    }
    return(moving_median)
  }else{
    stop()
  }
}
my_moving_median(x = 1:10, n=2)
mark_my_assignment(tasks="my_moving_median")


#multiplication table
for_mult_table<-function(from,to){
  if (length(from)==1 && length(to)==1){
    v<-c()
    for (i in from:to){
      for (j in from:to){
        v<-c(v,i*j)
      }
    }
    dim(v)<-c((to-from)+1,(to-from)+1) #we can use length(from:to) as well
    colnames(v)<-c(from:to) 
    rownames(v)<-c(from:to)
    return(v)
  }else{
    stop()
  }
}
for_mult_table(from = 10, to = 12)
mark_my_assignment(tasks="for_mult_table")


#while_cum_sum
find_cumsum<-function(x,find_sum){
  if ((is.atomic(x) || is.list(x)) && length(find_sum)==1){
    i<-1
    cumulative_sum<-0
    while(i<length(x) && find_sum>cumulative_sum){
      cumulative_sum<-sum(x[1]:x[i+1])
      i<-i+1
    }
    return(cumulative_sum)
  }else{
    stop()
  }
}
find_cumsum(x=1:100, find_sum=500)
mark_my_assignment(tasks="find_cumsum")


#while_mult_table
while_mult_table<-function(from,to){
  i<-1
  j<-1
  v<-c() #creating empty vector
  vector<-c(from:to)# creating a vector of values from:to #we can use length(from:to) as well
  while(i<=(to-from)+1){
    while(j<=(to-from)+1){
      v<-c(v,vector[i]*vector[j]) # appending the multiplied value to the empty vector
      j<-j+1
    }
    i<-i+1
    j<-1
  }# we can create matrix using matrix() as well
  dim(v)<-c((to-from)+1,(to-from)+1) #we can use length(from:to) as well
  colnames(v)<-c(from:to) 
  rownames(v)<-c(from:to)
  return(v)
}
while_mult_table(from = 10, to = 12)
mark_my_assignment(tasks="while_mult_table")


#repeat_find_cumsum
repeat_find_cumsum<-function(x,find_sum){
  if ((is.atomic(x) || is.list(x)) && length(find_sum)==1){
    i<-1
    repeat{
      t<-i+1
      cumulative_sum<-as.numeric(sum(x[1]:x[t]))
      i<-i+1
      if(i>=length(x) || find_sum<=cumulative_sum){
        break
      }
    }
    return(cumulative_sum)
  }else{
    stop()
  }
}
repeat_find_cumsum(x=1:100, find_sum=500)
mark_my_assignment(tasks="repeat_find_cumsum")


#repeat_moving_median
repeat_my_moving_median<-function(x,n,...){
  if((is.atomic(x) || is.list(x)) && length(n)==1){
    b<-length(x)-n
    moving_median<-c()
    i<-1
    repeat{
        t<-i+n
        moving_median<-c(moving_median,median(x[i:t],...))
        i<-i+1
        if(i>b){
          break
        }
    }
    return(moving_median)
  }else{
    stop()
  }
}
repeat_my_moving_median(x = 1:10, n=2)
mark_my_assignment(tasks="repeat_my_moving_median")

#environment
in_environment<-function(env){
  '!'<-'exclamation'
  "!.hexmode"<-'hexmode'
  "!.octmode"<-'octmode'
  '!='<-'notequalto'
  '$'<-'select'
  return(ls(env))
}
funs<-in_environment()
funs[1:5]
mark_my_assignment(tasks="in_environment")
env <- search()[length(search())]
env

# in_environment<-function(env){
#   return(ls(env))
# }
# funs<-in_environment(env)
# funs[1:5]
# mark_my_assignment(tasks="in_environment")


#coffiecient of variation
cov<-function(X){
  if(is.data.frame(X)){
    coffiecient_of_variation<-lapply(X,function(X) sd(X)/mean(X))
    v<-unlist(coffiecient_of_variation) # converting list to vector
    return(v)
  }else{
    stop()
  }
}
iris
cov(X = iris[1])
mark_my_assignment(tasks="cov")


#moments
moment<-function(i){
  if(is.numeric(i)){
    function(x){
      moment_central<-sum((x-mean(x))^i)/length(x)
      return(moment_central)
  }
  }else{
    stop()
  }
}
m1 <- moment(i=1)
m2 <- moment(i=2)
m1(1:100)
m2(1:100)
mark_my_assignment(tasks="moment")


mark_my_assignment()
