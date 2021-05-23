const swap = <T>(xs: T[], i: number, j: number): void => {
  const t = xs[i];
  xs[i] = xs[j];
  xs[j] = t;
};

const reverse = <T>(xs: T[], l: number, h: number): void => {
  const m = Math.trunc((l + h) / 2);
  for (let i = l; i <= m; i += 1) {
    swap(xs, i, h - i + l);
  }
};

const find = <T>(xs: T[], l: number, h: number, x: T): number | null => {
  let minI = null;
  let min = null;
  for (let i = l; i <= h; i += 1) {
    if (xs[i] > x && (min === null || xs[i] <= min)) {
      min = xs[i];
      minI = i;
    }
  }
  return minI;
};

/**
 Do not return anything, modify nums in-place instead.
 */
function nextPermutation(nums: number[]): void {
  if (nums.length <= 1) {
    return;
  }
  for (let i = nums.length - 2; i >= 0; i -= 1) {
    const f = find(nums, i + 1, nums.length - 1, nums[i]);
    if (f !== null) {
      swap(nums, i, f);
      reverse(nums, i + 1, nums.length - 1);
      return;
    }
  }
  nums.reverse();
}

const ns = [
  [1, 2, 3],
  [3, 2, 1],
  [1, 1, 5],
  [1],
  [],
  [2, 1, 4, 3],
  [2, 3, 1, 3, 3],
];
ns.forEach(nextPermutation);
console.log(ns[0], [1, 3, 2]);
console.log(ns[1], [1, 2, 3]);
console.log(ns[2], [1, 5, 1]);
console.log(ns[3], [1]);
console.log(ns[4], []);
console.log(ns[5], [2, 3, 1, 4]);
console.log(ns[6], [2, 3, 3, 1, 3]);
