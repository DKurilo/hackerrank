const wordSearch = () => {
  const exist = (board: string[][], word: string): boolean => {
    const hash = (x: number, y: number): string => `${x},${y}`;
    const checkWord = (
      n: number,
      x: number,
      y: number,
      used: Set<string>,
    ): boolean => {
      if (n >= word.length) {
        return true;
      }

      if (!board[y] || board[y][x] !== word[n] || used.has(hash(x, y))) {
        return false;
      }

      used.add(hash(x, y));

      if (checkWord(n + 1, x, y - 1, used)) {
        return true;
      }

      if (checkWord(n + 1, x, y + 1, used)) {
        return true;
      }

      if (checkWord(n + 1, x - 1, y, used)) {
        return true;
      }

      if (checkWord(n + 1, x + 1, y, used)) {
        return true;
      }

      used.delete(hash(x, y));

      return false;
    };
    for (let i = 0; i < board.length; i += 1) {
      for (let j = 0; j < board[i].length; j += 1) {
        const used: Set<string> = new Set();
        if (checkWord(0, j, i, used)) {
          return true;
        }
      }
    }
    return false;
  };

  console.log(exist([['A', 'B', 'C', 'E']], 'ABCE'), true);

  console.log(
    exist(
      [
        ['A', 'B', 'C', 'E'],
        ['S', 'F', 'C', 'S'],
        ['A', 'D', 'E', 'E'],
      ],
      'ABCCED',
    ),
    true,
  );

  console.log(
    exist(
      [
        ['A', 'B', 'C', 'E'],
        ['S', 'F', 'C', 'S'],
        ['A', 'D', 'E', 'E'],
      ],
      'SEE',
    ),
    true,
  );

  console.log(
    exist(
      [
        ['A', 'B', 'C', 'E'],
        ['S', 'F', 'C', 'S'],
        ['A', 'D', 'E', 'E'],
      ],
      'ABCB',
    ),
    false,
  );
};

wordSearch();
