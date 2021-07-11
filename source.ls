print "true condition";
var a = "123123";
var k = 5;
print a;
print k;
print k;
k = 8;
print k;
if (false) {
  print "inner true";
} else {
  print "inner false";
}
if (true) {
  print "second inner true";
} else {
  print "second inner false";
}

var i = 0;
while (i < 5) {
  print i;
  i = i + 1;
}