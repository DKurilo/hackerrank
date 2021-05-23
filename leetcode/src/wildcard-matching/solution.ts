// Backtracking solution. Works properly, but slow..
const optimizePattern = (p: string): string =>
  p
    .split('')
    .reduce((o, c) => (c === '*' && o.c === '*' ? o : { p: o.p + c, c }), {
      p: '',
      c: '',
    }).p;

const isMatch = (s: string, p: string): boolean => {
  const o = optimizePattern(p);
  const processStar = (ns: number, np: number): boolean => {
    const nextNs = o[np + 1] === '?' ? ns + 1 : s.indexOf(o[np + 1], ns + 1);
    if (nextNs < 0) {
      return false;
    }
    // eslint-disable-next-line no-use-before-define
    return doer(nextNs, np + 1) || doer(ns, np + 1) || doer(nextNs, np);
  };
  const doer = (ns: number, np: number): boolean => {
    if (o.length === np && s.length === ns) {
      return true;
    }
    if (o.length === np) {
      return false;
    }
    if (s.length === ns && (o.length !== np + 1 || o[np] !== '*')) {
      return false;
    }
    if (o.length === np + 1 && o[np] === '*') {
      return true;
    }
    switch (o[np]) {
      case '?':
        return doer(ns + 1, np + 1);
      case '*':
        return processStar(ns, np);
      default:
        if (s[ns] === o[np]) {
          return doer(ns + 1, np + 1);
        }
        return false;
    }
  };
  return doer(0, 0);
};

console.log(optimizePattern('*************'), '*');
console.log(optimizePattern('*ab***ba**b*b*aaab*b'), '*ab*ba*b*b*aaab*b');
console.log(isMatch('aa', 'a'), false);
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
