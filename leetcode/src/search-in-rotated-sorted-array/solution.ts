const fix = <T>(rec: (g: (x: T) => T) => (x: T) => T): ((x: T) => T) => {
  const g = (x: T): T => rec(g)(x);
  return g;
};

const search = (nums: number[], target: number): number => {
  const pivot = fix<[number, number]>((rec) => ([l, h]) => {
    if (h - l <= 1) {
      return [h, h];
    }
    if (nums[l] > nums[h]) {
      const m = Math.trunc((h + l) / 2);
      return nums[m] > nums[l] ? rec([m, h]) : rec([l, m]);
    }
    return [l, l];
  })([0, nums.length - 1])[0];
  const fixI = (i: number): number => (i + nums.length - pivot) % nums.length;
  const unfixI = (i: number): number => (i + pivot) % nums.length;
  return fix<[number, number]>((rec) => ([l, h]) => {
    const fixedL = fixI(l);
    const fixedH = fixI(h);
    if (
      (nums[l] > target && nums[h] > target) ||
      (nums[l] < target && nums[h] < target)
    ) {
      return [-1, -1];
    }
    if (fixedH - fixedL <= 1) {
      if (nums[h] === target) {
        return [h, h];
      }
      if (nums[l] === target) {
        return [l, l];
      }
      return [-1, -1];
    }
    const fixedM = Math.trunc((fixedH + fixedL) / 2);
    const m = unfixI(fixedM);
    if (nums[m] === target) {
      return [m, m];
    }
    if (nums[m] > target) {
      return rec([l, m]);
    }
    return rec([m, h]);
  })([unfixI(0), unfixI(nums.length - 1)])[0];
};

console.log(search([4, 5, 6, 7, 0, 1, 2], 0), 4);
console.log(search([4, 5, 6, 7, 0, 1, 2], 3), -1);
console.log(search([1], 0), -1);
console.log(search([1], 1), 0);
console.log(search([1, 2, 3, 4, 5], 3), 2);
