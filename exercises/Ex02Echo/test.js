
module.exports = (server) => {
  const echo = s => {
    it(`it should match "${s}"`, (done) => {
      server
      .post('/echo', s)
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
