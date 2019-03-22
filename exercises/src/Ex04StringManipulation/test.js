
module.exports = (server) => {
  const match = (input, expected) => {
    it(`it should match "${expected}"`, (done) => {
      server
      .post('/string-manipulation', input)
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        res.text.should.equal(expected)
        done()
      })
    })
  }

  const shouldMatch = (a) => match(a, a)
  const shouldThink = (a) => match("I'm positive " + a, "I think " + a)

  describe('Exercise 04 - String manipulation', () => {
    describe('POST /string-manipulation', () => {
      shouldMatch("I think dogs are cool")
      shouldThink("coriander is great")
      shouldMatch("Just a random statement")
    })
  })
}

