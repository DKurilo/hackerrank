function longestCommonPrefix(strs: string[]): string {
  if (strs.length === 0) {
    return '';
  }
  const s0 = strs[0];
  const ss = strs.slice(1);
  return s0.split('').reduce(
    ({ pr, done }, c, i) => {
      if (done) {
        return { pr, done };
      }
      return ss.reduce((p, s) => p && s[i] === c, true)
        ? { pr: pr + c, done: false }
        : { pr, done: true };
    },
    { pr: '', done: false },
  ).pr;
}

console.log(longestCommonPrefix([]), '');
console.log(longestCommonPrefix(['flower']), 'flower');
console.log(longestCommonPrefix(['flower', 'flow', 'flight']), 'fl');
console.log(longestCommonPrefix(['dog', 'racecar', 'car']), '');
