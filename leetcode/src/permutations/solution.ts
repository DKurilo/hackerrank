const mkArray = (from: number, to: number): number[] =>
  [...new Array(to - from + 1)].map((_, i) => i + from);

const permute = (nums: number[]): number[][] =>
  nums.length <= 1
    ? [nums]
    : mkArray(0, nums.length - 1)
        .map((n) =>
          permute(nums.filter((_, i) => i !== n)).map((ns) =>
            [nums[n]].concat(ns),
          ),
        )
        .flat();

console.log(permute([1, 2, 3]), [
  [1, 2, 3],
  [1, 3, 2],
  [2, 1, 3],
  [2, 3, 1],
  [3, 1, 2],
  [3, 2, 1],
]);
console.log(permute([0, 1]), [
  [0, 1],
  [1, 0],
]);
console.log(permute([1]), [[1]]);
permute([0, 1, 2, 3, 4, 5, 6, 7, 8]);
