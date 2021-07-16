type UnformattedString = {
  ws: string[];
  ls: number;
  l: number;
  isLast: boolean;
};
type Rule = (u: UnformattedString, maxWidth: number) => string;

const stringify = (words: string[], maxWidth: number): UnformattedString[] =>
  words.reduce(
    (ss: UnformattedString[], w: string, i: number): UnformattedString[] => {
      const isLast = i === words.length - 1;
      if (ss.length === 0) {
        return [{ ws: [w], l: w.length, ls: w.length, isLast }];
      }
      const last = ss[ss.length - 1];
      if (w.length + last.ls >= maxWidth) {
        return ss.concat([{ ws: [w], l: w.length, ls: w.length, isLast }]);
      }
      const init = ss.slice(0, -1);
      return init.concat([
        {
          ws: last.ws.concat([w]),
          l: last.l + w.length,
          ls: last.ls + w.length + 1,
          isLast,
        },
      ]);
    },
    [],
  );

const alignLeft: Rule = ({ ws }, maxWidth) => {
  const s = ws.join(' ');
  return s.concat(' '.repeat(maxWidth - s.length));
};

const justify: Rule = ({ ws, l }, maxWidth) => {
  const spaces = (maxWidth - l) / (ws.length - 1);
  const increasedSpaces = (maxWidth - l) % (ws.length - 1);
  const concatS = ws.reduce((s: string, w: string, i: number): string => {
    if (s === '') {
      return w;
    }
    return s
      .concat(' '.repeat(spaces + (i <= increasedSpaces ? 1 : 0)))
      .concat(w);
  }, '');
  return concatS.concat(' '.repeat(maxWidth - concatS.length));
};

const applyRules = (us: UnformattedString[], maxWidth: number): string[] =>
  us.map((u) => (u.isLast ? alignLeft : justify)(u, maxWidth));

const fullJustify = (words: string[], maxWidth: number): string[] =>
  applyRules(stringify(words, maxWidth), maxWidth);

console.log(
  fullJustify(
    ['This', 'is', 'an', 'example', 'of', 'text', 'justification.'],
    16,
  ),
  ['This    is    an', 'example  of text', 'justification.  '],
);

console.log(
  fullJustify(['What', 'must', 'be', 'acknowledgment', 'shall', 'be'], 16),
  ['What   must   be', 'acknowledgment  ', 'shall be        '],
);

console.log(
  fullJustify(
    [
      'Science',
      'is',
      'what',
      'we',
      'understand',
      'well',
      'enough',
      'to',
      'explain',
      'to',
      'a',
      'computer.',
      'Art',
      'is',
      'everything',
      'else',
      'we',
      'do',
    ],
    20,
  ),
  [
    'Science  is  what we',
    'understand      well',
    'enough to explain to',
    'a  computer.  Art is',
    'everything  else  we',
    'do                  ',
  ],
);
