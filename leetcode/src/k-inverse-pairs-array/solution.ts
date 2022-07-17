const kInversePairsArray = () => {
  const kInversePairs = (n: number, k: number): number => {
    if (n < 2) {
      return k < 1 ? 1 : 0;
    }
    const pairs = (x: number): number => (x * (x - 1)) / 2;
    if (k > pairs(n) || k < 0) {
      return 0;
    }
    const modulo = 10 ** 9 + 7;
    const sum = (x: number, y: number): number => (x + y) % modulo;
    const all: number[][] = [[1, 1]];
    const getIndex = (i: number, j: number): number => {
      const psi = pairs(i);
      const psiCenter = Math.ceil((psi + 1) / 2) - 1;
      if (j <= psiCenter) {
        return j;
      }
      return psi - j;
    };
    for (let i = 3; i <= n; i += 1) {
      all.push([1]);
      const psi = pairs(i);
      const psiCenter = Math.ceil((psi + 1) / 2) - 1;
      for (let j = 1; j <= Math.min(k, psiCenter); j += 1) {
        all[1].push(
          sum(
            sum(all[1][j - 1], all[0][getIndex(i - 1, j)]),
            modulo - (j >= i ? all[0][getIndex(i - 1, j - i)] : 0),
          ),
        );
      }
      all.shift();
    }
    const ps = pairs(n);
    const center = Math.ceil((ps + 1) / 2) - 1;
    const k1 = k <= center ? k : ps - k;
    return all[0][k1];
  };

  console.log(kInversePairs(1, 0), 1);
  console.log(kInversePairs(1, 1), 0);

  console.log(kInversePairs(3, 0), 1);

  console.log(kInversePairs(3, 1), 2);

  // 5
  console.log('\n5');
  for (let i = 0; i < 11; i += 1) {
    console.log(kInversePairs(5, i));
  }

  console.log('\n6');
  for (let i = 0; i < 16; i += 1) {
    console.log(kInversePairs(6, i));
  }
  console.log('');

  console.log(kInversePairs(100, 100));
  console.log(kInversePairs(1000, 1));
  console.log(kInversePairs(1000, 1000));
};

kInversePairsArray();
