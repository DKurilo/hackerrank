type Quadruple<T> = [T, T, T, T];
type QuadrupleN = Quadruple<number>;
type Pair = {
  n1: number;
  i: number;
  n2: number;
  j: number;
};

const dedupeQuadruples = <T>(quadruples: Quadruple<T>[]): Quadruple<T>[] =>
  quadruples.reduce(
    (
      { qs, qMap }: { qs: Quadruple<T>[]; qMap: Record<string, boolean> },
      q: Quadruple<T>,
    ): { qs: Quadruple<T>[]; qMap: Record<string, boolean> } => {
      const s = JSON.stringify(q);
      return qMap[s]
        ? { qs, qMap }
        : { qs: qs.concat([q]), qMap: Object.assign(qMap, { [s]: true }) };
    },
    { qs: [], qMap: {} },
  ).qs;

function fourSum(nums: number[], target: number): QuadrupleN[] {
  if (nums.length < 4) {
    return [];
  }
  nums.sort((a, b) => a - b);
  const pairStart = nums.reduce(
    (
      { ns, nPrev }: { ns: [number, number][]; nPrev: number },
      n: number,
      i: number,
    ): { ns: [number, number][]; nPrev: number } =>
      n !== nPrev ? { ns: ns.concat([[n, i]]), nPrev: n } : { ns, nPrev },
    { ns: [[nums[0], 0]], nPrev: nums[0] },
  ).ns;
  const pairs = pairStart.reduce(
    (ps: Pair[], [n1, i]: [number, number]): Pair[] =>
      ps.concat(
        nums.slice(i + 1).map((n2, j) => ({ n1, i, n2, j: j + i + 1 })),
      ),
    [],
  );
  const pairMap = nums.reduce(
    (
      pm: Record<number, Pair[]>,
      n1: number,
      i: number,
    ): Record<number, Pair[]> => {
      const ps = nums
        .slice(i + 1)
        .map((n2, j) => ({ n1, i, n2, j: j + i + 1 }));
      return ps.reduce(
        (pm1, p) =>
          Object.assign(pm1, {
            [p.n1 + p.n2]: (pm1[p.n1 + p.n2] || []).concat(p),
          }),
        pm,
      );
    },
    {},
  );
  return dedupeQuadruples(
    pairs
      .map((p1) => {
        const sum = target - p1.n1 - p1.n2;
        if (!pairMap[sum]) {
          return [];
        }
        return pairMap[sum].reduce((ps, p2) => {
          if (p1.j >= p2.i) {
            return ps;
          }
          return ps.concat([[p1.n1, p1.n2, p2.n1, p2.n2]]);
        }, []);
      })
      .flat(),
  );
}

console.log(fourSum([1, 0, -1, 0, -2, 2], 0), [
  [-2, -1, 1, 2],
  [-2, 0, 0, 2],
  [-1, 0, 0, 1],
]);
console.log(fourSum([2, 2, 2, 2, 2], 8), [[2, 2, 2, 2]]);
const rn = () => Math.trunc(Math.random() * 2 * 10 ** 4 - 10 ** 4);
console.log(fourSum([...new Array(200)].map(rn), rn()));
