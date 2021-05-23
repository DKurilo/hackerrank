const zipWith = <A, B, C>(f: (x: A, y: B) => C) => (xs: A[], ys: B[]): C[] => {
  const zs = [];
  for (let i = 0; i < xs.length && i < ys.length; i += 1) {
    zs.push(f(xs[i], ys[i]));
  }
  return zs;
};

const addMax = (
  ms: [number, number][],
  v: [number, number],
): [number, number][] =>
  ms.length <= 1 ||
  (ms[ms.length - 1][0] <= v[0] &&
    ms[ms.length - 2][0] <= ms[ms.length - 1][0]) ||
  v[0] < ms[ms.length - 1][0]
    ? ms.concat([v])
    : addMax(ms.slice(0, ms.length - 1), v);

const getMaxes = (hs: number[]): [number, number][] =>
  hs.concat([0]).reduce(
    (
      { ms, v, d }: { ms: [number, number][]; v: [number, number]; d: number },
      h: number,
      i: number,
    ): { ms: [number, number][]; v: [number, number]; d: number } => {
      if (d >= 0) {
        if (h >= v[0]) {
          return { ms, v: [h, i], d: 1 };
        }
        return {
          ms: addMax(ms, v),
          v: [h, i],
          d: -1,
        };
      }
      if (d < 0 && h <= v[0]) {
        return { ms, v: [h, i], d: -1 };
      }
      return { ms, v: [h, i], d: 1 };
    },
    { ms: [], v: [0, -1], d: 0 },
  ).ms;

const trap = (height: number[]): number => {
  const maxes = getMaxes(height);
  const intervals = zipWith<
    [number, number],
    [number, number],
    [number, number, number]
  >(([maxx, x], [maxy, y]) => [Math.min(maxx, maxy), x, y])(
    maxes,
    maxes.slice(1),
  );
  return intervals.reduce(
    (v, [min, i, j]) =>
      height
        .slice(i, j + 1)
        .reduce((v1, h) => (h < min ? v1 + min - h : v1), v),
    0,
  );
};

console.log(getMaxes([0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]));
console.log(getMaxes([2, 3, 5, 7, 9, 8, 4, 1]));
console.log(getMaxes([4, 2, 0, 3, 2, 5]));
console.log(getMaxes([8, 8, 1, 5, 6, 2, 5, 3, 3, 9]));
console.log(trap([0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1]), 6);
console.log(trap([2, 3, 5, 7, 9, 8, 4, 1]), 0);
console.log(trap([4, 2, 0, 3, 2, 5]), 9);
console.log(trap([1, 0, 2, 0, 3, 0, 2, 0, 1]), 6);
console.log(trap([8, 8, 1, 5, 6, 2, 5, 3, 3, 9]), 31);
console.log(
  trap([...new Array(100000)].map(() => Math.trunc(Math.random() * 100000))),
);
