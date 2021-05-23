const combinationSum = (candidates: number[], target: number): number[][] => {
  if (candidates.length === 0) {
    return [];
  }
  const x = candidates[0];
  if (x > target) {
    return combinationSum(candidates.slice(1), target);
  }
  if (x === target) {
    return [[x]].concat(combinationSum(candidates.slice(1), target));
  }
  return combinationSum(candidates, target - x)
    .map((xs) => [x].concat(xs))
    .concat(combinationSum(candidates.slice(1), target));
};

console.log(combinationSum([2, 3, 6, 7], 7), [[2, 2, 3], [7]]);
console.log(combinationSum([2, 3, 5], 8), [
  [2, 2, 2, 2],
  [2, 3, 3],
  [3, 5],
]);
console.log(combinationSum([2], 1), []);
console.log(combinationSum([1], 1), [[1]]);
console.log(combinationSum([1], 2), [[1, 1]]);
