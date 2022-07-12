const matchsticksToSquare = () => {
  const makesquare = (matchsticks: number[]): boolean => {
    if (matchsticks.length < 4) {
      return false;
    }
    const sum = matchsticks.reduce((a, x) => a + x, 0);
    if (sum % 4 !== 0) {
      return false;
    }
    const side = sum / 4;

    const combinations = (
      cds: number[],
      tgt: number,
      prev: number,
    ): number[][] => {
      if (cds.length === 0) {
        return [];
      }
      const x = cds[0];
      if (x === prev) {
        return combinations(cds.slice(1), tgt, x);
      }
      if (x > tgt) {
        return combinations(cds.slice(1), tgt, x);
      }
      if (x === tgt) {
        return [[x]].concat(combinations(cds.slice(1), tgt, x));
      }
      return combinations(cds.slice(1), tgt - x, -1)
        .map((xs) => [x].concat(xs))
        .concat(combinations(cds.slice(1), tgt, x));
    };

    const sideFinder = (n: number, mcs: number[]): boolean => {
      if (n === 4) {
        return true;
      }
      const cmbs = combinations(
        mcs.slice(0).sort((a, b) => a - b),
        side,
        Infinity,
      );
      if (cmbs.length === 0) {
        return false;
      }
      for (let i = 0; i < cmbs.length; i += 1) {
        const nextMcs = cmbs[i].reduce((mcs1, k) => {
          const j = mcs1.findIndex((x) => x === k);
          mcs1.splice(j, 1);
          return mcs1;
        }, mcs.slice(0));
        if (sideFinder(n + 1, nextMcs)) {
          return true;
        }
      }
      return false;
    };
    return sideFinder(0, matchsticks);
  };

  const makesquareSimpleCode = (matchsticks: number[]): boolean => {
    if (matchsticks.length < 4) {
      return false;
    }
    const sum = matchsticks.reduce((a, x) => a + x, 0);
    if (sum % 4 !== 0) {
      return false;
    }
    const side = sum / 4;
    matchsticks.sort((a, b) => b - a);
    const edges = [0, 0, 0, 0];
    const doer = (i: number): boolean => {
      if (i === matchsticks.length) {
        return true;
      }
      for (let j = 0; j < 4; j += 1) {
        edges[j] += matchsticks[i];
        if (edges[j] <= side && doer(i + 1)) {
          return true;
        }
        edges[j] -= matchsticks[i];
      }
      return false;
    };
    return doer(0);
  };

  console.log(makesquare([1, 1, 2, 2, 2]), true);

  console.log(makesquare([3, 3, 3, 3, 4]), false);

  console.log(makesquare([3, 3, 1, 2, 2, 2, 2, 2, 2, 5]), true);

  console.log(makesquare([5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3]), true);

  console.log(
    makesquare([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 102]),
    false,
  );
};

matchsticksToSquare();
