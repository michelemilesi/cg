object Player extends App{
val V=List("N","","S","W","","E")
var Array(a,b,x,y)=readLine split " "map(_.toInt)
while(0<1){
val h=math.signum(a-x)
val v=math.signum(b-y)
x-=h
y+=v
println(V(v+1)+V(h+4))
}}
