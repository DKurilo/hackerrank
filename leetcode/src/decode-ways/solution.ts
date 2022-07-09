const decodeWays = () => {
  const numDecodings = (s: string): number => {
    const wordsCount = new Array(s.length).fill(0);
    for (let i = s.length - 1; i >= 0; i -= 1) {
      if (s[i] !== '0') {
        if (i + 1 < s.length) {
          wordsCount[i] = wordsCount[i + 1];
          if (
            s[i] === '1' ||
            (s[i] === '2' && s[i + 1] >= '0' && s[i + 1] <= '6')
          ) {
            if (i + 2 < s.length) {
              wordsCount[i] += wordsCount[i + 2];
            } else {
              wordsCount[i] += 1;
            }
          }
        } else {
          wordsCount[i] = 1;
        }
      }
    }
    return wordsCount[0];
  };

  console.log(numDecodings('12'), 2);

  console.log(numDecodings('226'), 3);

  console.log(numDecodings('06'), 0);

  console.log(
    numDecodings(
      '4861914241921019426119686191114610191411106191610194861914111061914116119410610419111041910104191211194111261019412111941112619112116811912',
    ),
    0,
  );
};

decodeWays();
