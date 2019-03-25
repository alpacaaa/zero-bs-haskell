
const exercise = window.EXERCISE
if (!exercise) throw new Error('No exercise specified!')

require('./test-runner.js')(exercise)
