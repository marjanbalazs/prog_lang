var b = 0;
var c = 1;

while (b < 100000) {
  var tmp = c + b;
  c = b;
  b = tmp;
  print tmp;
}