
const chai = require('chai')
const chaiHttp = require('chai-http')
const { spawn } = require('child_process')

chai.use(chaiHttp)
const should = chai.should()

const exercise = process.env.EXERCISE

const host = 'http://localhost:7879'

const get = (path) => {
  return chai.request(host)
    .get(path)
}

const post = (path, body) => {
  return chai.request(host)
    .post(path)
    .send(body)
}

const test1 = () => {
  describe('Exercise 01 - Static String', () => {
    describe('GET /hello', () => {
      it('it should match `hello`', (done) => {
        get('/hello')
        .end((err, res) => {
          if (err) return done(err)
          res.should.have.status(200)
          res.text.should.equal('hello')
          done()
        })
      })
    })
  })
}

const test2 = () => {
  const echo = s => {
    it(`it should match "${s}"`, (done) => {
      post('/echo', s)
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        res.text.should.equal(s)
        done()
      })
    })
  }

  describe('Exercise 02 - Echo', () => {
    describe('POST /echo', () => {
      echo('hello')
      echo('pasta')
      echo('pizza')
    })
  })
}

const test3 = () => {
  const match = (n,s) => {
    it(`it should match "${s}"`, (done) => {
      post('/case', n)
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        res.text.should.equal(s)
        done()
      })
    })
  }

  describe('Exercise 03 - Case Match', () => {
    describe('POST /case', () => {
      match('1', 'one')
      match('2', 'two')
      match('3', 'three')
    })
  })
}

switch (exercise) {
  case "1": return test1()
  case "2": return test2()
  case "3": return test3()
  default: console.log("wtf?")
}
