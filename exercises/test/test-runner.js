
const chai = require('chai')
const chaiHttp = require('chai-http')

chai.use(chaiHttp)
const should = chai.should()

const Ex01 = require('../src/Ex01StaticString/test.js')
const Ex02 = require('../src/Ex02Echo/test.js')
const Ex03 = require('../src/Ex03CaseMatch/test.js')
const Ex04 = require('../src/Ex04StringManipulation/test.js')
const Ex05 = require('../src/Ex05OnoffSwitch/test.js')
const Ex06 = require('../src/Ex06Counter/test.js')
const Ex07 = require('../src/Ex07ShoppingCart/test.js')

const host = 'http://localhost:7879'

const server = {
  get: (path) => {
    return chai.request(host)
      .get(path)
  },

  post: (path, body) => {
    return chai.request(host)
      .post(path)
      .send(body)
  },
}


module.exports = (exercise) => {
  switch (exercise) {
    case "01": return Ex01(server)
    case "02": return Ex02(server)
    case "03": return Ex03(server)
    case "04": return Ex04(server)
    case "05": return Ex05(server)
    case "06": return Ex06(server)
    case "07": return Ex07(server)
    default:
      throw new Error(`Invalid exercise ${exercise}`)
  }
}
