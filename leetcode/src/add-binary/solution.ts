const zipEquals = <A, B>(xs: A[], ys: B[]): [A, B][] =>
  xs.map((x: A, i: number): [A, B] => [x, ys[i]]);

const sum3 = (x: string, y: string, o: string): { s: string; t: string } => {
  const a = [x, y, o].filter((c) => c === '1');
  return { s: a.length % 2 === 0 ? '0' : '1', t: a.length <= 1 ? '0' : '1' };
};

const addBinary = (a: string, b: string): string => {
  const l = Math.max(a.length, b.length);
  const unify = (cs: string): string[] =>
    '0'
      .repeat(l - cs.length)
      .concat(cs)
      .split('')
      .reverse();
  const aRev = unify(a);
  const bRev = unify(b);
  return (({ sum, o }) => (o === '1' ? sum.concat(o) : sum))(
    zipEquals(aRev, bRev).reduce(
      (
        { sum, o }: { sum: string[]; o: string },
        [x, y]: [string, string],
      ): { sum: string[]; o: string } => {
        const { s, t } = sum3(x, y, o);
        return { sum: sum.concat(s), o: t };
      },
      { sum: [], o: '0' },
    ),
  )
    .reverse()
    .join('');
};

console.log(addBinary('0', '0'), '0');
console.log(addBinary('0', '1'), '1');
console.log(addBinary('1', '0'), '1');
console.log(addBinary('1', '1'), '10');
console.log(addBinary('11', '1'), '100');
console.log(addBinary('1010', '1011'), '10101');
console.log(addBinary('1111', '1'), '10000');
