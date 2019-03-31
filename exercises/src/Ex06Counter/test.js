
module.exports = (server) => {
  const shouldMatch = (expected) => {
    it(`it should match "${expected}"`, (done) => {
      server
      .get('/current-count')
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        res.text.should.equal('' + expected)
        done()
      })
    })
  }

  const increase = () => {
    it(`it should increase the current count`, (done) => {
      server
      .post('/increase', '')
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        done()
      })
    })
  }

  describe('Exercise 06 - Counter', () => {
    describe('Counter state', () => {
      shouldMatch(0)
      increase()
      shouldMatch(1)
      increase()
      increase()
      shouldMatch(3)
      increase()
      shouldMatch(4)
    })
  })
}

