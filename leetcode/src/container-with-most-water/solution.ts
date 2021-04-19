function maxArea(height: number[]): number {
  const hl = height.length;
  const longests: number[] = height.reduce(
    (ls: number[], _, i: number): number[] =>
      ls.length === 0 || height[hl - i - 1] > height[ls[ls.length - 1]]
        ? ls.concat([hl - i - 1])
        : ls,
    [],
  );

  const doer1 = (
    h: number,
    i: number,
    max: number,
    ls: number[],
  ): { maxNew: number; lsNew: number[] } => {
    if (ls.length === 0) {
      return { maxNew: max, lsNew: ls };
    }
    const h1 = height[ls[0]];
    const v = Math.max(Math.min(h, h1) * (ls[0] - i), max);
    if (h >= h1) {
      return doer1(h, i, v, ls.slice(1));
    }
    return { maxNew: v, lsNew: ls };
  };

  const doer = (
    { max, maxh, ls }: { max: number; maxh: number; ls: number[] },
    h: number,
    i: number,
  ): { max: number; maxh: number; ls: number[] } => {
    if (h <= maxh || ls.length === 0 || i >= ls[0]) {
      return { max, maxh, ls };
    }
    const { maxNew, lsNew } = doer1(h, i, max, ls);
    return { max: maxNew, maxh: h, ls: lsNew };
  };

  return height.reduce(doer, { max: 0, maxh: 0, ls: longests }).max;
}

console.log(maxArea([]), 0);
console.log(maxArea([1]), 0);
console.log(maxArea([1, 1]), 1);
console.log(maxArea([1, 8, 6, 2, 5, 4, 8, 3, 7]), 49);
console.log(maxArea([4, 3, 2, 1, 4]), 16);
console.log(maxArea([1, 2, 1]), 2);
console.log(maxArea([1, 3, 4, 2, 3]), 9);
console.log(maxArea([...new Array(1000000)].map(() => Math.random() * 100001)));
