main() {
  a:int;
  b:bool;
  a_arr:int[] = {0,1};

  a = 42;
  a = -10;
  a = a;
  a = a + a;
  a = a * a;
  a = a/a;
  a = -a;
  a = a % a;
  a = a *>> a;
  a = length(a_arr);
  a = a_arr[1];
  a_arr = a_arr + {length({true})};
  a = a_arr[1 + a];

  b = false;
  b = true;
  b = a == a;
  b = a != a;
  b = a_arr == {1, 2};
  b = a_arr != {1, 2};
  b = a < a;
  b = a > 42;
  b = a <= 42;
  b = a >= a;
  b = b & b;
  b = b | b;
  b = !b;
  b = !false;
}