type Acc = {
  t: number;
  l: number;
  b: number;
  r: number;
  di: number;
  dj: number;
  i: number;
  j: number;
  res: number[];
};
const spiralOrder = (matrix: number[][]): number[] => {
  if (matrix.length === 0 || matrix[0].length === 0) {
    return [];
  }
  const mn = matrix.length;
  const mm = matrix[0].length;
  const acc: Acc = [...new Array(mn * mm)].reduce(
    ({ t, l, b, r, di, dj, i, j, res }: Acc): Acc => {
      if (di === 1 && i === r) {
        return {
          t: t + 1,
          l,
          b,
          r,
          di: 0,
          dj: 1,
          i,
          j: j + 1,
          res: res.concat([matrix[j][i]]),
        };
      }
      if (di === -1 && i === l) {
        return {
          t,
          l,
          b: b - 1,
          r,
          di: 0,
          dj: -1,
          i,
          j: j - 1,
          res: res.concat([matrix[j][i]]),
        };
      }
      if (dj === 1 && j === b) {
        return {
          t,
          l,
          b,
          r: r - 1,
          di: -1,
          dj: 0,
          i: i - 1,
          j,
          res: res.concat([matrix[j][i]]),
        };
      }
      if (dj === -1 && j === t) {
        return {
          t,
          l: l + 1,
          b,
          r,
          di: 1,
          dj: 0,
          i: i + 1,
          j,
          res: res.concat([matrix[j][i]]),
        };
      }
      return {
        t,
        l,
        b,
        r,
        di,
        dj,
        i: i + di,
        j: j + dj,
        res: res.concat([matrix[j][i]]),
      };
    },
    { t: 0, l: 0, b: mn - 1, r: mm - 1, di: 1, dj: 0, i: 0, j: 0, res: [] },
  );
  return acc.res;
};

console.log(
  spiralOrder([
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
  ]),
  [1, 2, 3, 6, 9, 8, 7, 4, 5],
);
console.log(
  spiralOrder([
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12],
  ]),
  [1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7],
);
