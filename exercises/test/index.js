
const n = process.argv[2]

const fail = () => {
  console.log("Invalid exercise number. Provide number as a cli argument.")
  process.exit(1)
}

if (!n) fail()

switch (n) {
  case "01": return require('../src/Ex01StaticString/reference.js')
  case "02": return require('../src/Ex02Echo/reference.js')
  case "03": return require('../src/Ex03CaseMatch/reference.js')
  case "04": return require('../src/Ex04StringManipulation/reference.js')
  case "05": return require('../src/Ex05OnoffSwitch/reference.js')
  default: fail()
}
