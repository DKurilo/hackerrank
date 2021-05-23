const strStr = (haystack: string, needle: string): number => {
  if (needle.length === 0) {
    return 0;
  }
  const n = haystack.length - needle.length + 1;
  for (let i = 0; i < n; i += 1) {
    if (haystack.slice(i, i + needle.length) === needle) {
      return i;
    }
  }
  return -1;
};

console.log(strStr('hello', 'll'), 2);
console.log(strStr('hello', 'helloq'), -1);
console.log(strStr('hello', 'lo'), 3);
console.log(strStr('aaaaa', 'bba'), -1);
console.log(strStr('', ''), 0);
console.log(strStr('a'.repeat(50000), 'b'), -1);
