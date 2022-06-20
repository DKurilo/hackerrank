const shortEncodingOfWords = () => {
  const minimumLengthEncoding = (words: string[]): number =>
    words
      .map((w) => w.split('').reverse().join(''))
      .sort()
      .reduce((res: string[], w: string): string[] => {
        if (res.length === 0) {
          return [w];
        }
        const lastWord = res[res.length - 1];
        if (w.startsWith(lastWord)) {
          res.pop();
        }
        res.push(w);
        return res;
      }, [])
      .join('#').length + 1;

  console.log(minimumLengthEncoding(['time', 'me', 'bell']), 10);

  console.log(minimumLengthEncoding(['t']), 2);
};

shortEncodingOfWords();
