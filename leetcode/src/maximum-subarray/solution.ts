const fromNullable = <A>(v: A) => (x: A | null): A => (x === null ? v : x);

const maxSubArray = (nums: number[]): number =>
  fromNullable(0)(
    nums.reduce(
      (
        { max, curr }: { max: null | number; curr: number },
        x: number,
      ): { max: number; curr: number } => {
        if (max === null) {
          return { max: x, curr: x > 0 ? x : 0 };
        }
        const next = curr + x;
        if (next < 0) {
          return { max: Math.max(max, x), curr: 0 };
        }
        return { max: Math.max(max, next), curr: next };
      },
      { max: null, curr: 0 },
    ).max,
  );

console.log(maxSubArray([-2, 1, -3, 4, -1, 2, 1, -5, 4]), 6);
console.log(maxSubArray([1]), 1);
console.log(maxSubArray([5, 4, -1, 7, 8]), 23);
console.log(maxSubArray([-1, -2, -3, 1]), 1);
console.log(maxSubArray([-2, 1]), 1);
console.log(maxSubArray([-1, 1]), 1);
console.log(maxSubArray([-2, -1]), -1);
