const combinationSum2 = (candidates: number[], target: number): number[][] => {
  const solver = (cds: number[], tgt: number, prev: number): number[][] => {
    if (cds.length === 0) {
      return [];
    }
    const x = cds[0];
    if (x === prev) {
      return solver(cds.slice(1), tgt, x);
    }
    if (x > tgt) {
      return solver(cds.slice(1), tgt, x);
    }
    if (x === tgt) {
      return [[x]].concat(solver(cds.slice(1), tgt, x));
    }
    return solver(cds.slice(1), tgt - x, -1)
      .map((xs) => [x].concat(xs))
      .concat(solver(cds.slice(1), tgt, x));
  };
  return solver(
    candidates.sort((a, b) => a - b),
    target,
    -1,
  );
};

console.log(combinationSum2([10, 1, 2, 7, 6, 1, 5], 8), [
  [1, 1, 6],
  [1, 2, 5],
  [1, 7],
  [2, 6],
]);
console.log(combinationSum2([2, 5, 2, 1, 2], 5), [[1, 2, 2], [5]]);
console.log(combinationSum2([2], 1), []);
console.log(combinationSum2([1], 1), [[1]]);
console.log(combinationSum2([1], 2), []);
