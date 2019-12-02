l&i|let z 1=o(+);z 2=o(*);z _=head l;g=(!!)l;o f|(h,_:m)<-splitAt(g$i+3)l=(h++(f(g.g$i+1)$g.g$i+2):m)&(i+4)=z$g i
r a b(x:_:_:l)=(x:a:b:l)&0
f a l=(r 12 2 l,head[100*x+y|x<-a,y<-a,r x y l==19690720])
main=interact$show.f[0..99].read.('[':).(++"]")
