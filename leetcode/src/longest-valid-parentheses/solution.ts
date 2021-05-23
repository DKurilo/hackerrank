const zipWith = <A, B, C>(f: (x: A, y: B) => C) => (xs: A[], ys: B[]): C[] => {
  const zs = [];
  for (let i = 0; i < xs.length && i < ys.length; i += 1) {
    zs.push(f(xs[i], ys[i]));
  }
  return zs;
};

const longestValidParentheses = (s: string): number => {
  const ps: { pc: string; p: number }[] = s
    .split('')
    .reduce(
      (
        queue: { pc: string; p: number }[],
        c: string,
        i: number,
      ): { pc: string; p: number }[] => {
        if (c === '(') {
          return queue.concat([{ pc: '(', p: i }]);
        }
        if (c === ')') {
          return queue[queue.length - 1].pc === '('
            ? queue.slice(0, queue.length - 1)
            : queue.concat([{ pc: ')', p: i }]);
        }
        return queue;
      },
      [{ pc: '*', p: -1 }],
    )
    .concat([{ pc: '*', p: s.length }]);
  return Math.max(
    ...zipWith<{ pc: string; p: number }, { pc: string; p: number }, number>(
      (x, y) => y.p - x.p - 1,
    )(ps, ps.slice(1)),
  );
};

console.log(longestValidParentheses('(()'), 2);
console.log(longestValidParentheses(')()())'), 4);
console.log(longestValidParentheses(''), 0);
console.log(longestValidParentheses(')))()((((()))))()()((('), 16);
