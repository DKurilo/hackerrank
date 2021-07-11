const getPermutation = (n: number, k: number): string => {
  const abc = [...new Array(n)].map((_, i) => (i + 1).toString(10));
  const facs: number[] = abc.reduce(
    (
      { fs, f }: { fs: number[]; f: number },
      _,
      i: number,
    ): { fs: number[]; f: number } => ({
      fs: fs.concat([f]),
      f: f * (i + 1),
    }),
    { fs: [], f: 1 },
  ).fs;
  const doer = (abc1: string[], k1: number): string => {
    if (abc1.length === 1) {
      return abc1[0];
    }
    const thisIndex = Math.trunc(k1 / facs[abc1.length - 1]);
    const nextK = k1 % facs[abc1.length - 1];
    return (
      abc1[thisIndex] +
      doer(
        abc1.filter((_, i) => i !== thisIndex),
        nextK,
      )
    );
  };
  return doer(abc, k - 1);
};

console.log(getPermutation(3, 3), '213');
console.log(getPermutation(4, 9), '2314');
console.log(getPermutation(3, 1), '123');
console.log(getPermutation(30, 4000000));
