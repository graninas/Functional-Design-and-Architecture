
rm *.class
rm -fr native
rm -fr serverContext

scalac $1.scala && scala -cp . $1
