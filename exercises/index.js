
const n = process.argv[2]

const fail = () => {
  console.log("Invalid exercise number. Provide number as a cli argument.")
  process.exit(1)
}

if (!n) fail()

switch (n) {
  case "1": return require('./src/Ex01StaticString/static-string.js')
  case "2": return require('./src/Ex02Echo/echo.js')
  case "3": return require('./src/Ex03CaseMatch/case-match.js')
  case "4": return require('./src/Ex04StringManipulation/string-manipulation.js')
  default: fail()
}
