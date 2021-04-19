const findAllIndices = <T>(xs: T[], x: T): number[] =>
  xs.reduce((is, y, i) => (x === y ? is.concat(i) : is), []);

function twoSum(nums: number[], target: number): [number, number] {
  return nums.reduce(
    (r, n, i) => {
      if (r[0] >= 0) {
        return r;
      }
      const is = findAllIndices(nums, target - n).filter((i1) => i1 !== i);
      return is.length > 0 ? [i, is[0]] : r;
    },
    [-1, -1],
  );
}

console.log(twoSum([2, 7, 11, 15], 9));
console.log(twoSum([3, 2, 4], 6));
console.log(twoSum([3, 3], 6));
console.log(twoSum([2, 5, 11, 15, -2], 9));
