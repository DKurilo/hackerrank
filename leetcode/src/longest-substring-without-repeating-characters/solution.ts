function lengthOfLongestSubstring(s: string): number {
  const doer = (
    [maxLength, startIndex, chars]: [number, number, { [key: string]: number }],
    c: string,
    i: number,
  ): [number, number, { [key: string]: number }] => {
    if (chars[c] === undefined || chars[c] < 0) {
      const newLength = i - startIndex + 1;
      return [
        newLength > maxLength ? newLength : maxLength,
        startIndex,
        { ...chars, [c]: i },
      ];
    }
    const prevI = chars[c];
    return [
      maxLength,
      prevI + 1,
      {
        ...chars,
        ...s
          .slice(startIndex, prevI)
          .split('')
          .reduce(
            (
              o: { [key: string]: number },
              c1: string,
            ): { [key: string]: number } => ({ ...o, [c1]: -1 }),
            {},
          ),
        [c]: i,
      },
    ];
  };
  return s.split('').reduce(doer, [0, 0, {}])[0];
}

console.log(lengthOfLongestSubstring('abcabcbb'), 3);
console.log(lengthOfLongestSubstring('bbbbb'), 1);
console.log(lengthOfLongestSubstring('pwwkew'), 3);
console.log(lengthOfLongestSubstring(''), 0);
console.log(lengthOfLongestSubstring('tmmzuxt'), 5);
const longString = [...new Array(5000)]
  .map(() => String.fromCharCode(Math.floor(Math.random() * 90) + 32))
  .join('');
console.log(lengthOfLongestSubstring(longString));
