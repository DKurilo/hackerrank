// I really hope nobody uses such challenges during whiteboard interview.
// I solved regular expressions exercises earlier, for example:
// https://github.com/DKurilo/hackerrank/blob/master/count-strings/solution.hs
// But I was need to open Wiki to remmeber how it works, just because algorithm is a bit complex
// and it's difficult to memorize it and even more difficult to build it from scratch.
// Primitive solution with backtracking works fine here, but very slow.
// Very strightforward and simplified implementation of regular expressions
// Without any optimization except ** to *.
// Information:
// https://en.wikipedia.org/wiki/Regular_expression#Implementations_and_running_times
// https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm - I didn't use it here, but it's interesting
// https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
// https://en.wikipedia.org/wiki/Powerset_construction
// https://en.wikipedia.org/wiki/Deterministic_finite_automaton
// So first we are building NFA from pattern (buildNFA), then DFA from NFA (transformNFAtoDFA)
// and then applying it to given string (runDFA)
// It's easy to use it for morecomplex finite languages and it's easy to add pattern like 'a*' (zero or more 'a')

const optimizePatternA = <A extends string>(p: A[]): A[] =>
  p.reduce(
    (o, c) => (c === '*' && o.c === '*' ? o : { p: o.p.concat([c]), c }),
    {
      p: [],
      c: '',
    },
  ).p;

// epsilon - transition without consuming string
const e = Symbol('e');
type E = typeof e;

// our alphabet
const abc = [
  'a',
  'b',
  'c',
  'd',
  'e',
  'f',
  'g',
  'h',
  'i',
  'j',
  'k',
  'l',
  'm',
  'n',
  'o',
  'p',
  'q',
  'r',
  's',
  't',
  'u',
  'v',
  'w',
  'x',
  'y',
  'z',
] as const;

// state for NFA
type St = number;

// NFA https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton#Formal_definition
type NFA<A extends string> = {
  tag: 'NFA';
  q: Set<St>;
  abc: readonly A[];
  d: Record<St, Record<A | E, Set<St>>>;
  q0: St;
  f: Set<St>;
};

// DFA https://en.wikipedia.org/wiki/Deterministic_finite_automaton#Formal_definition
type DFA<A extends string> = {
  tag: 'DFA';
  q: Set<string>;
  abc: readonly A[];
  d: Record<string, Record<A, string>>;
  q0: string;
  f: Set<string>;
};

const buildNFA = <A extends string>(
  p: (A | '*' | '?')[],
  ab: readonly A[],
): NFA<A> => {
  const d = (st: St, c: A | E): Set<St> => {
    switch (p[st]) {
      case '*': // in case of * we can move to the same state or to the next state, even for epsilon (without consume character)
        return new Set([st, st + 1]);
      case '?': // in case of ? we consume any character except epsilon (we have to consume string)
        return c === e ? new Set([]) : new Set([st + 1]);
      default:
        // first condition (c !== epsilon) is not necessary, because e is not part of string,
        // but it's easier to understand it this way. We just consume character if it's proper character
        return c !== e && c === p[st] ? new Set([st + 1]) : new Set([]);
    }
  };
  const abce: (A | E)[] = [...ab, e];
  const q = new Set([...new Array(p.length + 1)].map((_, i) => i));
  return {
    tag: 'NFA',
    q,
    abc: ab,
    d: [...q].reduce(
      (r, st) => ({
        ...r,
        [st]: abce.reduce((rst, c) => ({ ...rst, [c]: d(st, c) }), {}),
      }),
      {},
    ),
    q0: 0,
    f: new Set([p.length]),
  };
};

const transformNFAtoDFA = <A extends string>(nfa: NFA<A>): DFA<A> => {
  const nfaGetNextStates = (st: St, c: A | E): Set<St> => {
    const nextSts = new Set([]);
    nfa.d[st][c].forEach((st1) => {
      if (st1 !== undefined) {
        nextSts.add(st1);
      }
    });
    let done = false;
    let eSts = nextSts;
    while (!done) {
      done = true;
      const nextESts = new Set([]);
      eSts.forEach((st1) => {
        nfa.d[st1][e].forEach((st2) => {
          if (!nextSts.has(st2)) {
            nextESts.add(st2);
          }
        });
      });
      if (nextESts.size !== 0) {
        done = false;
        nextESts.forEach((st1) => {
          nextSts.add(st1);
        });
        eSts = nextESts;
      }
    }
    return nextSts;
  };
  const setStToString = (sts: Set<St>): string =>
    [...sts].sort((a, b) => a - b).join(':+:');
  const isFinal = (sts: Set<St>): boolean =>
    [...sts].reduce((p: boolean, st: St): boolean => p || nfa.f.has(st), false);
  const q0st = nfaGetNextStates(nfa.q0, e);
  q0st.add(nfa.q0);
  const q0 = setStToString(q0st);
  const q = new Set([q0]);
  const f = isFinal(q0st) ? new Set([q0]) : new Set([]);
  // to make it working faster I'm using imperative programming aproach here.
  // TypeScript is not very good in expressing some parts.
  // Strictly functional programming aproach you can check here:
  // https://github.com/DKurilo/hackerrank/blob/master/count-strings/solution.hs
  // it's solution for https://www.hackerrank.com/challenges/count-strings/problem
  let curSts = [q0st];
  let curs = [q0];
  const d: Record<string, Record<A, string>> = {};
  let done = false;
  while (!done) {
    done = true;
    const nextSts: typeof curSts = [];
    const nexts: typeof curs = [];
    for (let i = 0; i < curs.length; i += 1) {
      const curSt = curSts[i];
      const cur = curs[i];
      const nextStatesForABC: Record<string, string> = {};
      for (let k = 0; k < nfa.abc.length; k += 1) {
        const nst = new Set([]);
        curSt.forEach((st) => {
          nfaGetNextStates(st, nfa.abc[k]).forEach((st1) => nst.add(st1));
        });
        const n = setStToString(nst);
        nextStatesForABC[nfa.abc[k]] = n;
        if (!q.has(n)) {
          done = false;
          q.add(n);
          if (isFinal(nst)) {
            f.add(n);
          }
          nextSts.push(nst);
          nexts.push(n);
        }
      }
      d[cur] = nextStatesForABC;
    }
    curSts = nextSts;
    curs = nexts;
  }
  return {
    tag: 'DFA',
    q,
    abc: nfa.abc,
    d,
    q0,
    f,
  };
};

const compilePattern = <A extends string>(
  p: (A | '?' | '*')[],
  ab: readonly A[],
): DFA<A> => transformNFAtoDFA(buildNFA(p, ab));

const runDFA = <A extends string>(dfa: DFA<A>) => (s: A[]): boolean =>
  dfa.f.has(s.reduce((st, c) => dfa.d[st][c], dfa.q0));

const isMatch = (s: string, p: string): boolean =>
  runDFA(compilePattern(optimizePatternA(p.split('')), abc))(s.split(''));

console.log(isMatch('aa', 'a'), false);
console.log(isMatch('a', 'a'), true);
console.log(isMatch('aa', '*'), true);
console.log(isMatch('cb', '?a'), false);
console.log(isMatch('adceb', '*a*b'), true);
console.log(isMatch('acdcb', '*a*c?b'), false);
console.log(
  isMatch(
    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab',
    '*a',
  ),
  false,
);
console.log(isMatch('aaaaaaaaaaaaaaaaaaaaaa', '********b'), false);
console.log(
  isMatch(
    'aaabababaaabaababbbaaaabbbbbbabbbbabbbabbaabbababab',
    '*ab***ba**b*b*aaab*b',
  ),
  true,
);
console.log(
  isMatch(
    'abbaabbbbababaababababbabbbaaaabbbbaaabbbabaabbbbbabbbbabbabbaaabaaaabbbbbbaaabbabbbbababbbaaabbabbabb',
    '***b**a*a*b***b*a*b*bbb**baa*bba**b**bb***b*a*aab*a**',
  ),
  true,
);
console.log(
  isMatch(
    'babbbbaabababaabbababaababaabbaabababbaaababbababaaaaaabbabaaaabababbabbababbbaaaababbbabbbbbbbbbbaabbb',
    'b*a*b*a*b*a',
  ),
  false,
);
console.log(
  isMatch(
    'babbbbaabababaabbababaababaabbaabababbaaababbababaaaaaabbabaaaabababbabbababbbaaaababbbabbbbbbbbbbaabbb',
    'b**bb**a**bba*b**a*bbb**aba***babbb*aa****aabb*bbb***a',
  ),
  false,
);
console.log(
  isMatch(
    'baaaababbbaabbbbabbababaababaaabaababbbaabaabbbbaabbbbbbaabaabbababaaaaaaaabbbaaabbbababbbbbabbbbabbbbabbaabaababababbbababbbbbbbaaaaabbbbabbbbbaabbbaaaabaaabbabbabaabbbbabbbabbbaababbabbaaaababbababa',
    '*bb*abb*a**ba**aba*b*bbb*abbaaa*bb*b**a*b*b**a**abb***ab*b**b*b*a******a*a*babaa*bab*a*b****bb*babb*baa',
  ),
  false,
);
console.log(
  isMatch(
    'bbababababbababaaababbbaaababaaababbbbabaaaaabaabbaaababbbbabbabbbaaababbabbbbbbabbabababbbbabaabaabbaaaabbaaabaaabbaabababaababbaabbbbbabbbbabbbaabbabaaaaababbbaaabbbbabaababaaaababaaaabbbaaaaaababbaaba',
    '*ba**aa*aa*aa*bbba*baaba*ab*b*b*abb*b*bb*b*****a*bba**aa*b***b***aba**baa****b***a*b**ba*ba****a*aaa',
  ),
  false,
);
