lotta_binops(): int, bool {
  pemdas: int = 2 * 5 - 1 + 4
  more_pemdas: int = 3 *>> 5 - 2 / 2 % 1
  booleans: bool = true & (2 == 1) | (2 != 1) | false
  more_booleans: (2 < 1) | (3 >= 4) | (1 <= 1) | (3 > 5)
  return pemdas, more_booleans 
}

main(args: int[][]) {
  pemdas, more_booleans = lotta_binops()
}