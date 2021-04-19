const digitsToLetters = (d: string): string[] => {
  switch (d) {
    case '2':
      return 'abc'.split('');
    case '3':
      return 'def'.split('');
    case '4':
      return 'ghi'.split('');
    case '5':
      return 'jkl'.split('');
    case '6':
      return 'mno'.split('');
    case '7':
      return 'pqrs'.split('');
    case '8':
      return 'tuv'.split('');
    case '9':
      return 'wxyz'.split('');
    default:
      return [];
  }
};

const getCombinations = (ls: string[][]): string[] => {
  if (ls.length === 0) {
    return [];
  }
  const next = getCombinations(ls.slice(1));
  return next.length === 0
    ? ls[0]
    : ls[0].map((l) => next.map((s) => l + s)).flat();
};

function letterCombinations(digits: string): string[] {
  return getCombinations(digits.split('').map(digitsToLetters));
}

console.log(letterCombinations('23'), [
  'ad',
  'ae',
  'af',
  'bd',
  'be',
  'bf',
  'cd',
  'ce',
  'cf',
]);
console.log(letterCombinations(''), []);
console.log(letterCombinations('2'), ['a', 'b', 'c']);
console.log(letterCombinations('2345'));
