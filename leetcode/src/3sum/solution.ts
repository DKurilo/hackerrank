type Triple<T> = [T, T, T];
type TripleN = Triple<number>;

function threeSum(nums: number[]): TripleN[] {
  if (nums.length < 3) {
    return [];
  }

  const [vmap, uniqs]: [Record<number, number>, number[]] = nums.reduce(
    (
      [nsMap, ns]: [Record<number, number>, number[]],
      n: number,
    ): [Record<number, number>, number[]] =>
      nsMap[n]
        ? [{ ...nsMap, [n]: nsMap[n] + 1 }, ns]
        : [{ ...nsMap, [n]: 1 }, ns.concat([n])],
    [{}, []],
  );
  uniqs.sort((a, b) => a - b);

  return uniqs
    .slice(0, uniqs.length)
    .map((n1: number, i: number): TripleN[] => {
      return uniqs
        .slice(i)
        .map((n2: number): TripleN[] => {
          if (n1 === n2 && vmap[n1] < 2) {
            return [];
          }
          if (!vmap[-(n1 + n2)]) {
            return [];
          }
          const n3 = -(n1 + n2);
          if (
            n3 < n2 ||
            (n1 === n2 && n1 === n3 && vmap[n3] < 3) ||
            (n1 !== n2 && (n1 === n3 || n2 === n3) && vmap[n3] < 2)
          ) {
            return [];
          }
          return [[n1, n2, n3]];
        })
        .flat();
    })
    .flat();
  // const tringles: TripleN[] = [];
  // for (let i = 0; i < uniqs.length; i += 1) {
  //   const n1 = uniqs[i];
  //   for (let k = i; k < uniqs.length; k += 1) {
  //     const n2 = uniqs[k];
  //     const n3 = -(n1 + n2);
  //     if (
  //       (n2 > n1 || vmap[n1] >= 2) &&
  //       vmap[n3] &&
  //       (n3 > n2 ||
  //         (n1 === n2 && n1 === n3 && vmap[n3] >= 3) ||
  //         (n1 !== n2 && n3 >= n2 && (n1 === n3 || n2 === n3) && vmap[n3] >= 2))
  //     ) {
  //       tringles.push([n1, n2, n3]);
  //     }
  //   }
  // }
  // return tringles;
}

console.log(threeSum([-1, 0, 1, 2, -1, -4]), [
  [-1, -1, 2],
  [-1, 0, 1],
]);
console.log(threeSum([]), []);
console.log(threeSum([0]), []);
console.log(
  threeSum(
    [...new Array(3000)].map(
      () => Math.floor(Math.random() * 2 * 100000) - 100000,
    ),
  ),
);
