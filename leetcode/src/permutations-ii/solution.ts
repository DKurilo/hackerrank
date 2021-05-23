const mkArray = (from: number, to: number): number[] =>
  [...new Array(to - from + 1)].map((_, i) => i + from);

const permuteUnique = (nums: number[]): number[][] => {
  nums.sort((a, b) => a - b);
  return nums.length <= 1
    ? [nums]
    : mkArray(0, nums.length - 1).reduce(
        (nss, n) =>
          n === 0 || nums[n] !== nums[n - 1]
            ? nss.concat(
                permuteUnique(nums.filter((_, j) => j !== n)).map((ns) =>
                  [nums[n]].concat(ns),
                ),
              )
            : nss,
        [],
      );
};

console.log(permuteUnique([1, 2, 1]), [
  [1, 1, 2],
  [1, 2, 1],
  [2, 1, 1],
]);
console.log(permuteUnique([1, 2, 3]), [
  [1, 2, 3],
  [1, 3, 2],
  [2, 1, 3],
  [2, 3, 1],
  [3, 1, 2],
  [3, 2, 1],
]);
console.log(permuteUnique([0, 1]), [
  [0, 1],
  [1, 0],
]);
console.log(permuteUnique([1]), [[1]]);
permuteUnique([0, 1, 2, 3, 4, 5, 6, 7, 8]);
