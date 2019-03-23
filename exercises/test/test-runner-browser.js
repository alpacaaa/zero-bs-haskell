
const exercise = location.hash.substr(1)
// Check hashchange
// Mount as some sort of component?
if (!exercise) throw new Error('No exercise specified in hash')

require('./test-runner.js')(exercise)
