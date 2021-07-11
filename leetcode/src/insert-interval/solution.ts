const insert = (
  intervals: [number, number][],
  [sni, eni]: [number, number],
): [number, number][] => {
  const newIntervals = ((is) => {
    if (is.length === 0) {
      return [[sni, eni]];
    }
    if (sni > is[is.length - 1][0]) {
      return is.concat([[sni, eni]]);
    }
    return is;
  })(
    intervals.reduce((is, [s, e]) => {
      if (is.length === 0) {
        return sni <= s
          ? [
              [sni, eni],
              [s, e],
            ]
          : [[s, e]];
      }
      return is.concat(
        sni <= s && sni >= is[is.length - 1][0]
          ? [
              [sni, eni],
              [s, e],
            ]
          : [[s, e]],
      );
    }, []),
  );
  return newIntervals.reduce(
    (is: [number, number][], [s, e]: [number, number]): [number, number][] => {
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
    },
    [],
  );
};

console.log(
  insert(
    [
      [1, 3],
      [6, 9],
    ],
    [2, 5],
  ),
  [
    [1, 5],
    [6, 9],
  ],
);
console.log(
  insert(
    [
      [1, 2],
      [3, 5],
      [6, 7],
      [8, 10],
      [12, 16],
    ],
    [4, 8],
  ),
  [
    [1, 2],
    [3, 10],
    [12, 16],
  ],
);
console.log(insert([], [5, 7]), [[5, 7]]);
console.log(insert([[1, 5]], [2, 3]), [[1, 5]]);
console.log(insert([[1, 5]], [2, 7]), [[1, 7]]);
console.log(insert([[1, 5]], [2, 7]), [[1, 7]]);
console.log(insert([[1, 5]], [6, 8]), [
  [1, 5],
  [6, 8],
]);
console.log(
  insert(
    [
      [1, 5],
      [10, 20],
    ],
    [6, 8],
  ),
  [
    [1, 5],
    [6, 8],
    [10, 20],
  ],
);
