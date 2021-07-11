const canJump = (nums: number[]): boolean =>
  nums.reduce((l: number, n: number, i: number) => {
    if (l < 0) {
      return l;
    }
    if (n === 0 && l <= i && l !== nums.length - 1) {
      return -1;
    }
    return Math.max(l, i + n);
  }, 0) >=
  nums.length - 1;

console.log(canJump([2, 3, 1, 1, 4]), true);
console.log(canJump([3, 2, 1, 0, 4]), false);
console.log(canJump([0]), true);
