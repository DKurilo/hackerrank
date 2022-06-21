const furthestBuildingYouCanReach = () => {
  const zipWith = <T, U>(f: (x: T, y: T) => U, xs: T[], ys: T[]): U[] => {
    const resLength = Math.min(xs.length, ys.length);
    const res = [];
    for (let i = 0; i < resLength; i += 1) {
      res.push(f(xs[i], ys[i]));
    }
    return res;
  };

  const prepareDiffs = (xs: number[]): number[] =>
    xs.filter((x) => x > 0).sort((x: number, y: number): number => x - y);

  const isReachable = (
    diffs: number[],
    bricks: number,
    ladders: number,
  ): boolean => {
    let briksLeft = bricks;
    let currentDiff = 0;
    while (briksLeft >= diffs[currentDiff] && currentDiff < diffs.length) {
      briksLeft -= diffs[currentDiff];
      currentDiff += 1;
    }
    return ladders >= diffs.length - currentDiff;
  };

  const binarySearch = (
    diffs: number[],
    from: number,
    to: number,
    bricks: number,
    ladders: number,
  ): number => {
    if (to - from <= 1) {
      if (isReachable(prepareDiffs(diffs.slice(0, to)), bricks, ladders)) {
        return to;
      }
      return from;
    }
    const middle = Math.floor((from + to) / 2);
    if (isReachable(prepareDiffs(diffs.slice(0, middle)), bricks, ladders)) {
      return binarySearch(diffs, middle, to, bricks, ladders);
    }
    return binarySearch(diffs, from, middle, bricks, ladders);
  };

  const furthestBuilding = (
    heights: number[],
    bricks: number,
    ladders: number,
  ): number => {
    const diffs = zipWith(
      (x: number, y: number): number => y - x,
      heights,
      heights.slice(1),
    );

    return binarySearch(diffs, 0, diffs.length, bricks, ladders);
  };

  console.log(furthestBuilding([4, 2, 7, 6, 9, 14, 12], 5, 1), 4);

  console.log(furthestBuilding([4, 12, 2, 7, 3, 18, 20, 3, 19], 10, 2), 7);

  console.log(furthestBuilding([14, 3, 19, 3], 17, 0), 3);

  const hs1 = new Array(100000)
    .fill(0)
    .map(() => Math.floor(Math.random() * 1000000) + 1);
  const bs1 = Math.floor(Math.random() * 10 ** 9);
  const ls1 = Math.floor((Math.random() * 100001) / 5);
  console.log(furthestBuilding(hs1, bs1, ls1));
};

furthestBuildingYouCanReach();
