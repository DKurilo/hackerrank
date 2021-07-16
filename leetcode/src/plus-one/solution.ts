const plusOne = (digits: number[]): number[] =>
  (({ o, ds }: { o: boolean; ds: number[] }): number[] =>
    o ? [1].concat(ds) : ds)(
    digits.reverse().reduce(
      (
        { o, ds }: { o: boolean; ds: number[] },
        d: number,
      ): { o: boolean; ds: number[] } => {
        if (o) {
          return d < 9
            ? { o: false, ds: [d + 1].concat(ds) }
            : { o: true, ds: [0].concat(ds) };
        }
        return { o, ds: [d].concat(ds) };
      },
      { o: true, ds: [] },
    ),
  );

console.log(plusOne([1, 2, 3]), [1, 2, 4]);
console.log(plusOne([4, 3, 2, 1]), [4, 3, 2, 2]);
console.log(plusOne([0]), [1]);
console.log(plusOne([9]), [1, 0]);
console.log(plusOne([1, 9]), [2, 0]);
console.log(plusOne([9, 9]), [1, 0, 0]);
console.log(plusOne(new Array(1000).fill(9)).length);
