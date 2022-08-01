const lines = [
  '2 1', '1H',
  '3 3', '2.X', 'X.H', '.H1',
  '8 8', '.XXX.5X.', 'X.4.X..X', 'X4..X3.X', 'X...X.X.', '.X.X.H.X', 'X.HX...X', 'X..X.H.X', '.XH.XXX.'
];
let lineCounter = 6;
const readline = () => {
  const l = lines[lineCounter];
  lineCounter += 1;
  return l;
}

const main = () => {
  const toCoord = (...ns) => ns.join('x');
  const initBallsCoords = new Set();
  const balls = [];
  const holes = new Set();
  const water = new Set();
  const inputs = readline().split(' ');
  const width = parseInt(inputs[0]);
  const height = parseInt(inputs[1]);
  for (let i = 0; i < height; i++) {
    const row = readline();
    for (let j = 0; j < width; j++) {
      if (row[j] >= '1' && row[j] <= '9') {
        initBallsCoords.add(toCoord(i, j));
        balls.push({
          y: i,
          x: j,
          turns: parseInt(row[j], 10),
          arrows: []
        })
      } else if (row[j] === 'H') {
        holes.add(toCoord(i, j))
      } else if (row[j] === 'X') {
        water.add(toCoord(i, j));
      }
    }
  }
  const directions = [
    [1, 0, '>'],
    [-1, 0, '<'],
    [0, 1, 'v'],
    [0, -1, '^']
  ];
  const findAllArrows = (ball, busy) => {
    if (ball.turns === 0) {
      return [];
    }
    const arrows = [];
    for (let i = 0; i < 4; i += 1) {
      const [dx, dy, dir] = directions[i];
      let x = ball.x;
      let y = ball.y;
      const arrow = [
        [x, y, dir]
      ];
      for (let j = 1; j <= ball.turns; j += 1) {
        x += dx;
        y += dy;
        if (x < 0 || x >= width || y < 0 || y >= height) {
          break;
        }
        const coord = toCoord(y, x);
        if (j < ball.turns && (holes.has(coord) || initBallsCoords.has(coord) || busy.has(coord))) {
          break;
        }
        if (j === ball.turns) {
          if (holes.has(coord)) {
            arrow.push([x, y, '.']);
            arrows.push(arrow);
          } else {
            if (water.has(coord)) {
              break;
            }
            const newBusy = new Set([...busy, ...arrow.map(([x1, y1, ]) => toCoord(y1, x1))]);
            const restArrows = findAllArrows({
              ...ball,
              turns: ball.turns - 1,
              x,
              y
            }, newBusy).reduce((ars, ar) => {
              ars.push([...arrow, ...ar]);
              return ars;
            }, []);
            for (let k = 0; k < restArrows.length; k += 1) {
              arrows.push(restArrows[k]);
            }
          }
        } else {
          arrow.push([x, y, dir]);
        }
      }
    }
    return arrows;
  }
  for (let i = 0; i < balls.length; i += 1) {
    const arrows = findAllArrows({
      ...balls[i]
    }, new Set());
    for (let j = 0; j < arrows.length; j += 1) {
      balls[i].arrows.push(arrows[j]);
    }
  }

  const buildGame = (i, busy) => {
    if (i >= balls.length) {
      return [
        []
      ];
    }
    for (let j = 0; j < balls[i].arrows.length; j += 1) {
      const arrow = balls[i].arrows[j];
      let goodArrow = true;
      for (let k = 0; k < arrow.length; k += 1) {
        if (busy.has(toCoord(arrow[k][1], arrow[k][0]))) {
          goodArrow = false;
        }
      }
      if (goodArrow) {
        const rest = buildGame(i + 1, new Set([...busy, ...arrow.map(([x, y, ]) => toCoord(y, x))]));
        if (rest.length > 0) {
          return [
            [...rest[0], ...arrow]
          ];
        }
      }
    }
    return [];
  };

  const game = buildGame(0, new Set())[0];

  const grid = new Array(height).fill(0).map(() => new Array(width).fill('.'));
  for (let i = 0; i < game.length; i += 1) {
    const [x, y, c] = game[i];
    grid[y][x] = c;
  }

  console.log(grid.map((cells) => cells.join('')).join('\n'));
}

main();
