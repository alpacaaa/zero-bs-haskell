
module.exports = (server) => {
  const match = (n,s) => {
    it(`it should match "${s}"`, (done) => {
      server
      .post('/case', n)
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

