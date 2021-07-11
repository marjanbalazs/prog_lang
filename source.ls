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
  var f = "talala";
  print f;
  print "second inner true";
} else {
  print "second inner false";
}

var b = 0;
var c = 1;

while (b < 500) {
  var tmp = c + b;
  c = b;
  b = tmp;
  print tmp;
}
print i;
