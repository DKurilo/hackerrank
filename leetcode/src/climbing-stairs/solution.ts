const fibsMemo: number[] = [];

const fibs = (n: number): number => {
  if (n <= 1) {
    return 1;
  }
  if (fibsMemo[n] !== undefined) {
    return fibsMemo[n];
  }
  fibsMemo[n] = fibs(n - 1) + fibs(n - 2);
  return fibsMemo[n];
};

const climbStairs = (n: number): number => {
  return fibs(n);
};

console.log(climbStairs(2), 2);
console.log(climbStairs(3), 3);
console.log(climbStairs(45), 1836311903);
