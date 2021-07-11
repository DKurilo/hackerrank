const merge = (intervals: [number, number][]): [number, number][] => {
  intervals.sort(([sa, ea], [sb, eb]) => {
    const ds = sa - sb;
    if (ds === 0) {
      return ea - eb;
    }
    return ds;
  });
  return intervals.reduce((is: [number, number][], [s, e]: [number, number]): [
    number,
    number,
  ][] => {
    if (is.length === 0) {
      return [[s, e]];
    }
    const [sl, el] = is[is.length - 1];
    if (s <= el) {
      return is
        .slice(0, is.length - 1)
        .concat([[Math.min(sl, s), Math.max(el, e)]]);
    }
    return is.concat([[s, e]]);
  }, []);
};

console.log(
  merge([
    [1, 3],
    [2, 6],
    [8, 10],
    [15, 18],
  ]),
  [
    [1, 6],
    [8, 10],
    [15, 18],
  ],
);
console.log(
  merge([
    [1, 4],
    [4, 5],
  ]),
  [[1, 5]],
);
