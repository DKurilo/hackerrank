const memofac: number[] = [];

const fac = (n: number) => {
  if (n === 0) {
    return 1;
  }
  if (memofac[n] === undefined) {
    memofac[n] = n * fac(n - 1);
  }
  return memofac[n];
};

// uniquePaths = (m + n - 2)! / ((m - 1)! * (n - 1)!)
const uniquePaths = (m: number, n: number): number => {
  return Math.round(fac(m + n - 2) / fac(m - 1) / fac(n - 1));
};

console.log(uniquePaths(3, 7), 28);
console.log(uniquePaths(3, 2), 3);
console.log(uniquePaths(7, 3), 28);
console.log(uniquePaths(3, 3), 6);
console.log(uniquePaths(1, 1), 1);
console.log(uniquePaths(100, 10));
console.log(uniquePaths(100, 100));
