const extend = (
  s: string,
  palindrome: string,
  i: number,
  k: number,
): string => {
  if (i < 0 || k >= s.length || s[i] !== s[k]) {
    return palindrome;
  }
  return extend(s, `${s[i]}${palindrome}${s[k]}`, i - 1, k + 1);
};

const extendOdd = (s: string, i: number): string =>
  extend(s, s[i], i - 1, i + 1);

const extendEven = (s: string, i: number): string =>
  i < 1 || s[i - 1] !== s[i]
    ? ''
    : extend(s, `${s[i - 1]}${s[i]}`, i - 2, i + 1);

function longestPalindrome(s: string): string {
  return s === ''
    ? ''
    : [...new Array(s.length)]
        .map((_, i) => {
          return [extendEven(s, i), extendOdd(s, i)];
        })
        .flat()
        .reduce((ls, cs) => (cs.length > ls.length ? cs : ls), '');
}

console.log(longestPalindrome('babad'), 'bab');
console.log(longestPalindrome('cbbd'), 'bb');
console.log(longestPalindrome('a'), 'a');
console.log(longestPalindrome('ac'), 'a');
console.log(longestPalindrome(''), '');
console.log(
  longestPalindrome('satorarepotenetoperarotas'),
  'satorarepotenetoperarotas',
);
const randFromTo = (from: number, to: number): number =>
  Math.floor(Math.random() * (to - from)) + from;
const randomChar = () =>
  Math.random() > 0.5
    ? String.fromCharCode(randFromTo(97, 122))
    : String.fromCharCode(randFromTo(65, 90));
const longString1 = [...new Array(5000000)].map(() => randomChar()).join('');
console.log(longestPalindrome(longString1));
