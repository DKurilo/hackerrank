const groupAnagrams = (strs: string[]): string[][] =>
  (({ r, lg }) => r.concat([lg]))(
    strs
      .map((s, i): [string, number] => [s.split('').sort().join(), i])
      .sort((a, b) => {
        if (a[0] > b[0]) {
          return 1;
        }
        if (a[0] < b[0]) {
          return -1;
        }
        return 0;
      })
      .reduce(
        (
          { r, lg, l }: { r: string[][]; lg: string[]; l: string },
          [s, i]: [string, number],
        ): { r: string[][]; lg: string[]; l: string } =>
          s === l
            ? { r, lg: lg.concat([strs[i]]), l }
            : { r: lg.length > 0 ? r.concat([lg]) : r, lg: [strs[i]], l: s },
        { r: [], lg: [], l: '' },
      ),
  );

console.log(groupAnagrams(['eat', 'tea', 'tan', 'ate', 'nat', 'bat']), [
  ['bat'],
  ['nat', 'tan'],
  ['ate', 'eat', 'tea'],
]);
console.log(groupAnagrams(['']), [['']]);
console.log(groupAnagrams(['a']), [['a']]);
