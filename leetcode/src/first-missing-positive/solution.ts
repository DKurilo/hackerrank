// I hate such exercises! It says you need to mutate data.
// Don't mutate data where it's possible.
// It means almost never mutate data!
const firstMissingPositive = (nums: number[]): number => {
  for (let i = 0; i < nums.length; i += 1) {
    if (nums[i] !== i + 1) {
      let k = nums[i] - 1;
      // eslint-disable-next-line no-param-reassign
      nums[i] = 0;
      while (k >= 0 && k + 1 !== nums[k] && k < nums.length) {
        const n = nums[k];
        // eslint-disable-next-line no-param-reassign
        nums[k] = k + 1;
        k = n - 1;
      }
    }
  }
  for (let i = 0; i < nums.length; i += 1) {
    if (nums[i] === 0) {
      return i + 1;
    }
  }
  return nums.length + 1;
};

console.log(firstMissingPositive([1]), 2);
console.log(firstMissingPositive([1, 2, 0]), 3);
console.log(firstMissingPositive([3, 4, -1, 1]), 2);
console.log(firstMissingPositive([7, 8, 9, 11, 12]), 1);
const ns1 = [...new Array(1000000)].map(
  () => Math.trunc(Math.random() * 200000) - 100000,
);
const check = <A>(ns: A[], n: A): boolean =>
  ns.reduce((r, x) => r && x !== n, true);
const n1 = firstMissingPositive(ns1.slice());
console.log(n1, check(ns1, n1));
