// I really hope nobody uses such challenges during whiteboard interview. :)
// This exersice has elegant solution that you can find in discussion (using two points to track last successful point).
// But I wanted to solve it using regular expressions technique.
// I solved regular expressions exercises earlier, for example:
// https://github.com/DKurilo/hackerrank/blob/master/count-strings/solution.hs
// But I was need to open Wiki to remmeber how it works, just because algorithm is a bit complex
// and it's difficult to memorize it and even more difficult to build it from scratch.
// Solution with building DFA for one run is slower than leetcode is expecting.
// So I didn't convert NFA to DFA but calculate set of next sstates when I need it.
// In case we need to test multiple strings with the same pattern it will work slower,
// but here we need to test only one string with one pattern.
// So this is very strightforward and simplified implementation of regular expressions
// Without any optimization.
// Information:
// https://en.wikipedia.org/wiki/Regular_expression#Implementations_and_running_times
// https://en.wikipedia.org/wiki/Glushkov%27s_construction_algorithm - I didn't use it here, but it's interesting
// https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
// https://en.wikipedia.org/wiki/Powerset_construction
// https://en.wikipedia.org/wiki/Deterministic_finite_automaton
// So first we are building NFA from pattern (buildNFA) and then applying it to given string (runNFA)
// It's easy to use it for more complex finite languages and it's easy to add pattern like 'a*' (zero or more 'a')

/* eslint-disable no-loop-func */
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

const runNFA = <A extends string>(nfa: NFA<A>) => (s: A[]): boolean => {
  let sts = new Set([nfa.q0]);
  let front0 = new Set([nfa.q0]);
  while (front0.size > 0) {
    const nextFront = new Set([]);
    front0.forEach((st1) => {
      nfa.d[st1][e].forEach((st2) => {
        if (!sts.has(st2)) {
          nextFront.add(st2);
          sts.add(st2);
        }
      });
    });
    front0 = nextFront;
  }
  for (let i = 0; i < s.length; i += 1) {
    const nextSts = new Set([]);
    sts.forEach((st) => {
      const allFronts = new Set([st]);
      let front = new Set([st]);
      nfa.d[st][s[i]].forEach((st1) => {
        nextSts.add(st1);
        allFronts.add(st1);
        front.add(st1);
      });
      while (front.size > 0) {
        const nextFront = new Set([]);
        front.forEach((st1) => {
          nfa.d[st1][e].forEach((st2) => {
            if (!allFronts.has(st2)) {
              nextSts.add(st2);
              nextFront.add(st2);
              allFronts.add(st2);
            }
          });
        });
        front = nextFront;
      }
    });
    sts = nextSts;
  }
  return [...sts].reduce(
    (r: boolean, st: St): boolean => r || nfa.f.has(st),
    false,
  );
};

const isMatch = (s: string, p: string): boolean =>
  runNFA(buildNFA(p.split(''), abc))(s.split(''));

// console.log(buildNFA('*abc???de*'.split(''), abc));
console.log(isMatch('abcabczzzde', '*abc???de*'), true);
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
