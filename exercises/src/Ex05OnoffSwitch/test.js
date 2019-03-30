
module.exports = (server) => {
  const shouldMatch = (expected) => {
    it(`it should match "${expected}"`, (done) => {
      server
      .post('/onoff-switch', "")
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        res.text.should.equal(expected)
        done()
      })
    })
  }

  describe('Exercise 05 - On/Off switch', () => {
    describe('POST /onoff-switch', () => {
      shouldMatch("On")
      shouldMatch("Off")
      shouldMatch("On")
      shouldMatch("Off")
    })
  })
}

