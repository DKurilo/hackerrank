const findClosest = (
  ns: number[],
  target: number,
  from: number,
  to: number,
): number => {
  if (from === to) {
    return ns[from];
  }
  if (to - from === 1) {
    return Math.abs(ns[from] - target) <= Math.abs(ns[to] - target)
      ? ns[from]
      : ns[to];
  }
  const middle = Math.floor((from + to) / 2);
  if (ns[middle] === target) {
    return ns[middle];
  }
  return ns[middle] > target
    ? findClosest(ns, target, from, middle)
    : findClosest(ns, target, middle, to);
};

function threeSumClosest(nums: number[], target: number): number {
  if (nums.length < 3) {
    throw new Error('nums length should be 3 or more');
  }
  nums.sort((a, b) => a - b);
  const sum = nums
    .slice(0, nums.length - 2)
    .reduce((s: number, n1: number, i: number): number => {
      const s1 = nums
        .slice(i + 1, nums.length - 1)
        .reduce((s2: number, n2: number, k: number): number => {
          const n3 = findClosest(
            nums,
            target - n1 - n2,
            k + i + 2,
            nums.length - 1,
          );
          const s3 = n1 + n2 + n3;
          return s2 !== null && Math.abs(s2 - target) <= Math.abs(s3 - target)
            ? s2
            : s3;
        }, null);
      return s !== null && Math.abs(s - target) <= Math.abs(s1 - target)
        ? s
        : s1;
    }, null);
  if (sum === null) {
    throw new Error('something went wrong.');
  }
  return sum;
}

console.log(threeSumClosest([-1, 2, 1, -4], 1), 2);
console.log(threeSumClosest([1, 1, -1, -1, 3], -1), -1);
console.log(
  threeSumClosest(
    [...new Array(3000)].map(() => Math.floor(Math.random() * 2 * 1000) - 1000),
    Math.floor(Math.random() * 2 * 10000) - 10000,
  ),
);
