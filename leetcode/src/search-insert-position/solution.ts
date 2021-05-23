const fix = <T>(rec: (g: (x: T) => T) => (x: T) => T): ((x: T) => T) => {
  const g = (x: T): T => rec(g)(x);
  return g;
};

const searchInsert = (nums: number[], target: number): number =>
  fix<[number, number]>((rec) => ([l, h]) => {
    if (nums[l] === target) {
      return [l, l];
    }
    if (nums[h] === target) {
      return [h, h];
    }
    if (h - l <= 1) {
      if (nums[h] < target) {
        return [h + 1, h + 1];
      }
      if (nums[l] > target) {
        return [l, l];
      }
      return [h, h];
    }
    const m = Math.trunc((h + l) / 2);
    if (nums[m] === target) {
      return [m, m];
    }
    if (nums[m] < target) {
      return rec([m, h]);
    }
    return rec([l, m]);
  })([0, nums.length - 1])[0];

console.log(searchInsert([1, 3, 5, 6], 5), 2);
console.log(searchInsert([1, 3, 5, 6], 2), 1);
console.log(searchInsert([1, 3, 5, 6], 7), 4);
console.log(searchInsert([1, 3, 5, 6], 0), 0);
console.log(searchInsert([1], 0), 0);
