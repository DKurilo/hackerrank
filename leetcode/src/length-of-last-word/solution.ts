const lengthOfLastWord = (s: string): number => {
  let i = 0;
  let started = false;
  const l = s.length;
  for (let j = 0; j < l; j += 1) {
    if (s[l - j - 1] === ' ' && started) {
      return i;
    }
    if (s[l - j - 1] !== ' ') {
      started = true;
    }
    if (started) {
      i += 1;
    }
  }
  return started ? i : 0;
};

console.log(lengthOfLastWord('Hello World'), 5);
console.log(lengthOfLastWord('Hello'), 5);
console.log(lengthOfLastWord('Hello '), 5);
console.log(lengthOfLastWord(''), 0);
console.log(lengthOfLastWord(' '), 0);
console.log(lengthOfLastWord(' Hello'), 5);
