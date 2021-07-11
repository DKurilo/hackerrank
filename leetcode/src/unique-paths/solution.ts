const memo: Record<string, number> = {};

const uniquePaths = (m: number, n: number): number => {
  if (m <= 1 || n <= 1) {
    return 1;
  }
  const key = `${m}#${n}`;
  if (memo[key] === undefined) {
    memo[key] = uniquePaths(m - 1, n) + uniquePaths(n - 1, m);
  }
  return memo[key];
};

console.log(uniquePaths(3, 7), 28);
console.log(uniquePaths(3, 2), 3);
console.log(uniquePaths(7, 3), 28);
console.log(uniquePaths(3, 3), 6);
console.log(uniquePaths(1, 1), 1);
console.log(uniquePaths(100, 10));
console.log(uniquePaths(100, 100));
