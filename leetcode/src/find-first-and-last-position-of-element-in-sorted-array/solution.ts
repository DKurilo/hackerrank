const fix = <T>(rec: (g: (x: T) => T) => (x: T) => T): ((x: T) => T) => {
  const g = (x: T): T => rec(g)(x);
  return g;
};

const searchLeft = (nums: number[], target: number): number =>
  fix<[number, number, number, number]>((rec) => ([ll, l, h, hh]) => {
    if (nums[ll] === target) {
      return [ll, ll, ll, ll];
    }
    if (h - l <= 1) {
      if (nums[l] === target) {
        return [l, l, l, l];
      }
      if (nums[h] === target) {
        return [h, h, h, h];
      }
      return [-1, -1, -1, -1];
    }
    const m = Math.trunc((h + l) / 2);
    if (nums[m] < target) {
      return rec([m, m, h, hh]);
    }
    if (nums[m] > target) {
      return rec([ll, l, m, m]);
    }
    return rec([ll, ll, m, m]);
  })([0, 0, nums.length - 1, nums.length - 1])[0];

const searchRight = (nums: number[], target: number): number =>
  fix<[number, number, number, number]>((rec) => ([ll, l, h, hh]) => {
    if (nums[hh] === target) {
      return [hh, hh, hh, hh];
    }
    if (h - l <= 1) {
      if (nums[h] === target) {
        return [h, h, h, h];
      }
      if (nums[l] === target) {
        return [l, l, l, l];
      }
      return [-1, -1, -1, -1];
    }
    const m = Math.trunc((h + l) / 2);
    if (nums[m] > target) {
      return rec([ll, l, m, m]);
    }
    if (nums[m] < target) {
      return rec([m, m, h, hh]);
    }
    return rec([m, m, hh, hh]);
  })([0, 0, nums.length - 1, nums.length - 1])[0];

const searchRange = (nums: number[], target: number): [number, number] => [
  searchLeft(nums, target),
  searchRight(nums, target),
];

console.log(searchRange([5, 7, 7, 8, 8, 10], 8), [3, 4]);
console.log(searchRange([5, 7, 7, 8, 8, 10], 6), [-1, -1]);
console.log(searchRange([], 0), [-1, -1]);
console.log(searchRange([1, 1, 1, 1, 1, 1, 1], 1), [0, 6]);
