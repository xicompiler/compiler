use io
use conv

main(args:int[][]) {
  n: int[3];
  n = {5, 5, 5};
  while(n[0] > 0 & n[1] > 0 & n[2] > 0) {
    println(unparseInt(n[0]));
    n[0] = n[0] - 1;
    println(unparseInt(n[1]));
    n[1] = n[1] - 1;
    println(unparseInt(n[2]));
    if (n[2] < n[1] & n[2] != n[1]) {
      println("wrong");
    }
    else if (n[2] == n[1]) {
      println("wrong");
    }
    else {
      println("correct")
    }
    n[2] = n[2] - 1;
  }
}