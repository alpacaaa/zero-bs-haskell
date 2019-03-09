
const chai = require('chai')
const chaiHttp = require('chai-http')

chai.use(chaiHttp)
const should = chai.should()

const Ex01 = require('./Ex01StaticString/test.js')
const Ex02 = require('./Ex02Echo/test.js')
const Ex03 = require('./Ex03CaseMatch/test.js')

const exercise = process.env.EXERCISE

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


switch (exercise) {
  case "1": return Ex01(server)
  case "2": return Ex02(server)
  case "3": return Ex03(server)
  default:
    throw new Error(`Invalid exercise ${exercise}`)
}
