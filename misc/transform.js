const fs = require('fs');

const pipe = (a, ...fns) =>
  fns.reduce((x, f) => f(x), a)

const groupBy = (f) => (as) =>
  as.reduce((acc, cur) => {
    const key = f(cur)

    if (acc[key]) {
      acc[key] = [...acc[key], cur]
    } else {
      acc[key] = [cur]
    }

    return acc
  }, {})

const byRepository = ({ Repository }) => Repository
const bySections = ({ Sections }) => Sections

const fromEntries = 
  entries => entries.reduce((acc, [key, value]) => {
    acc[key] = value
    return acc
  }, {})

const Record = {
  fromEntries,
  map: (f) => (obj) => pipe(
    obj,
    Object.entries,
    entries => entries.map(([key, a]) => [key, f(a)]),
    fromEntries,
  ),
  drop: (toDrop) => (obj) => pipe(
    obj,
    Object.entries,
    entries => entries.filter(([key, _]) => key !== toDrop),
    fromEntries,
  ),
  fromEntries,
  collect: (f) => (obj) => pipe(
    obj,
    Object.entries,
    entries => entries.map(f),
  )
}

const head = ([a]) => a

pipe(
  fs.readFileSync(0, 'utf-8'), // stdin
  JSON.parse,
  groupBy(byRepository),
  Record.map(groupBy(bySections)),
  Record.map(Record.map(xs => xs.map(Record.drop('Sections')))),
  Record.map(Record.map(xs => xs.map(Record.drop('Repository')))),
  Record.map(Record.map(head)),
  Record.map(Record.map(Record.collect(([key, value]) => ({ data: key, value })))),
  v => JSON.stringify(v, null, 2),
  console.log
)
