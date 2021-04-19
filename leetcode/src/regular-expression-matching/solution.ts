type PatternChecker = (s: string) => boolean;

type State =
  | {
      tag: 'start';
      next: number[];
    }
  | {
      tag: 'char';
      required: boolean;
      char: string;
      next: number[];
    }
  | {
      tag: 'wildcard';
      required: boolean;
      next: number[];
    }
  | { tag: 'end' };

const dedupe = <T>(xs: T[]): T[] => Array.from(new Set(xs).values());

const mkPatternChecker = (p: string): PatternChecker => {
  const fsaUnoptimized: State[] = p
    .split('')
    .reduce(
      (fsa: State[], c: string): State[] => {
        if (c === '*' && fsa.length > 0) {
          const lastState = fsa[fsa.length - 1];
          if (lastState.tag === 'end' || lastState.tag === 'start') {
            return [{ tag: 'end' }]; // error!
          }
          return fsa.slice(0, fsa.length - 1).concat([
            {
              ...lastState,
              required: false,
              next: lastState.next.concat(fsa.length - 1),
            },
          ]);
        }
        if (c === '.') {
          return fsa.concat({
            tag: 'wildcard',
            required: true,
            next: [fsa.length + 1],
          });
        }
        return fsa.concat({
          tag: 'char',
          char: c,
          required: true,
          next: [fsa.length + 1],
        });
      },
      [{ tag: 'start', next: [1] }],
    )
    .concat({ tag: 'end' });

  const getNextStates = (i: number): number[] => {
    const state = fsaUnoptimized[i];
    switch (state.tag) {
      case 'end':
        return [];
      default:
        return (state.tag === 'start' || state.required ? [] : [i])
          .concat(state.next)
          .concat(
            state.next
              .filter((k) => k !== i)
              .map((k: number): number[] => {
                const stateK = fsaUnoptimized[k];
                switch (stateK.tag) {
                  case 'wildcard':
                  case 'char':
                    if (!stateK.required) {
                      return [k].concat(getNextStates(k));
                    }
                    return [k];
                  default:
                    return [k];
                }
              })
              .flat(),
          );
    }
  };

  const fsa = fsaUnoptimized.map((state, i) =>
    state.tag === 'end' ? state : { ...state, next: dedupe(getNextStates(i)) },
  );

  return (s: string): boolean => {
    const finalStates = s.split('').reduce(
      (states: number[], c: string): number[] =>
        dedupe(
          states.reduce((nextStates: number[], i: number) => {
            const state = fsa[i];
            if (state.tag === 'start') {
              return [];
            }
            if (state.tag === 'end') {
              return nextStates;
            }
            if (state.tag === 'wildcard') {
              return nextStates.concat(state.next);
            }
            return state.char === c
              ? nextStates.concat(state.next)
              : nextStates;
          }, []),
        ),
      dedupe(getNextStates(0)),
    );
    return finalStates.filter((i) => fsa[i].tag === 'end').length > 0;
  };
};

function isMatch(s: string, p: string): boolean {
  return mkPatternChecker(p)(s);
}

console.log(isMatch('aa', 'a'), false);
console.log(isMatch('a', '.'), true);
console.log(isMatch('aa', 'a*'), true);
console.log(isMatch('ab', '.*'), true);
console.log(isMatch('aab', 'c*a*b'), true);
console.log(isMatch('aaa', 'a*a*a*a*a*'), true);
console.log(isMatch('mississippi', 'mis*is*p*.'), false);
console.log(isMatch('aa', '.*c'), false);
console.log(isMatch('mississippi', 'mis*is*ip*.'), true);
console.log(isMatch('', '.*'), true);
console.log(isMatch('', '.'), false);
console.log(isMatch('a', 'ab*'), true);
console.log(isMatch('aaa', 'ab*ac*a'), true);
