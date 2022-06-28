const combinations = () => {
  const combine = (n: number, k: number): number[][] => {
    const cmbs: number[][] = [[]];
    for (let i = 0; i < k; i += 1) {
      const l = cmbs.length;
      for (let j = 0; j < l; j += 1) {
        const start = cmbs[j].length === 0 ? 0 : cmbs[j][cmbs[j].length - 1];
        for (let m = start + 1; m <= n - k + i; m += 1) {
          const newArr = cmbs[j].slice(0);
          newArr.push(m + 1);
          cmbs.push(newArr);
        }
        cmbs[j].push(start + 1);
      }
    }
    return cmbs;
  };

  const combineRec = (n: number, k: number): number[][] => {
    const doer = (l: number, start: number): number[][] =>
      l === 0
        ? [[]]
        : new Array(n - start + 1)
            .fill(0)
            .map((_, i) =>
              doer(l - 1, start + i + 1).map((arr) => [start + i].concat(arr)),
            )
            .flat();
    return doer(k, 1);
  };

  console.log(combine(4, 2), [
    [2, 4],
    [3, 4],
    [2, 3],
    [1, 2],
    [1, 3],
    [1, 4],
  ]);
  console.log('rec', combineRec(4, 2));

  console.log(combine(1, 1), [[1]]);

  const fac = (n: number): number => (n === 0 ? 1 : n * fac(n - 1));
  const cmbsn = (n: number, k: number): number => fac(n) / fac(k) / fac(n - k);
  console.log(combine(5, 3).length, cmbsn(5, 3));
  console.log(combine(20, 10).length, cmbsn(20, 10));
  console.log(combineRec(20, 10).length, cmbsn(20, 10));
};

combinations();
