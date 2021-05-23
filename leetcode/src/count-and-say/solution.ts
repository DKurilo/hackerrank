type Either<A, B> = { tag: 'Left'; left: A } | { tag: 'Right'; right: B };

const left = <A>(x: A): Either<A, never> => ({ tag: 'Left', left: x });
const right = <B>(x: B): Either<never, B> => ({ tag: 'Right', right: x });
const isLeft = (
  x: Either<unknown, unknown>,
): x is { tag: 'Left'; left: unknown } => x.tag === 'Left';
const isRight = (
  x: Either<unknown, unknown>,
): x is { tag: 'Right'; right: unknown } => x.tag === 'Right';

const loop = <A, B>(act: (x: A) => Either<A, B>) => (x: A): B => {
  let x1 = act(x);
  // eslint-disable-next-line no-constant-condition
  while (true) {
    if (isRight(x1)) {
      return x1.right;
    }
    x1 = act(x1.left);
  }
};

const nextStep = (s: string): string =>
  (({ sr, cc, n }) => `${sr}${cc.length === 0 ? '' : n.toString()}${cc}`)(
    s.split('').reduce(
      ({ sr, cc, n }, c) =>
        c === cc
          ? { sr, cc, n: n + 1 }
          : {
              sr: `${sr}${cc.length === 0 ? '' : n.toString()}${cc}`,
              cc: c,
              n: 1,
            },
      { sr: '', cc: '', n: 0 },
    ),
  );

const countAndSay = (n: number): string =>
  loop<[number, string], string>(([i, s]) => {
    if (i === n) {
      return right(s);
    }
    return left([i + 1, nextStep(s)]);
  })([1, '1']);

console.log(nextStep('3322251'), '23321511');
console.log(countAndSay(1), '1');
console.log(countAndSay(4), '1211');
console.log(countAndSay(30));
