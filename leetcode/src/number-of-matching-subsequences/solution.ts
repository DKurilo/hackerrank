const numberOfMatchingSubsequences = () => {
  const numMatchingSubseq = (s: string, words: string[]): number => {
    let count = 0;
    for (let i = 0; i < words.length; i += 1) {
      let currentPos = 0;
      for (let j = 0; currentPos >= 0 && j < words[i].length; j += 1) {
        currentPos = s.indexOf(words[i][j], currentPos);
        if (currentPos >= 0) {
          currentPos += 1;
        }
      }
      if (currentPos > 0) {
        count += 1;
      }
    }
    return count;
  };

  console.log(numMatchingSubseq('abcde', ['a', 'bb', 'acd', 'ace']), 3);

  console.log(
    numMatchingSubseq('dsahjpjauf', [
      'ahjpjau',
      'ja',
      'ahbwzgqnuk',
      'tnmlanowax',
    ]),
    2,
  );

  const generateString = (n: number): string =>
    new Array(n)
      .fill(0)
      .map(() =>
        String.fromCharCode(Math.floor(Math.random() * (122 - 97 + 1)) + 97),
      )
      .join('');

  const s1 = generateString(50000000).split('').join('');
  const words1 = new Array(500000).fill(0).map(() => generateString(10));

  const start1 = process.hrtime();
  console.log(numMatchingSubseq(s1, words1));
  const end1 = process.hrtime(start1);
  console.log(end1[0], end1[1] / 10 ** 9);
};

numberOfMatchingSubsequences();
